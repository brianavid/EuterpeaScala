package com.brianavid.euterpea
import javax.sound.{midi => M}
import M.{MidiSystem => MS}
import scala.collection.mutable

//  The tune is transformed into a javax.sound.midi.Sequence object for playing or saving
//  This transformation uses a SequenceContext object to specify prevailing modifier-specified 
//  state, which is passed around (and changed when necessary) through the processing of the Music AST 

private[euterpea] case class SequenceContext (
  val sequence: M.Sequence,                 //  The sequence being constructed
  val strict: Boolean,                      //  Use strict (no dynamics) timing for printing
  val timeState: TimeState,                 //  The current state (including position in ticks) where music will be added to the sequence
  val timingTrack: M.Track,                 //  The first track of the sequence to which tempo and time signature changes will be added
  val currentTrackName: String = "",        //  The current track name 
  val tracks: mutable.Map[String,M.Track],  //  The mapping of track named onto sequence tracks
  val currentChannelName: String = "",      //  The current channel name 
  val channels: mutable.Map[String,Int],    //  The mapping of track named onto (all different) Midi channels
  val pendingTimingChanges: mutable.Set[M.MidiEvent] = mutable.Set.empty,
  val transpose: Int = 0,                   //  Any specified prevailing chromatic transposition
  val dTrans: Option[(Chord,Chord)] = None, //  Any specified prevailing diatonic transposition
  val tempoBPM: Int,                        //  The current tempo, in beats per minute
  val depth: Int = 1,                       //  The depth in the nested structure of note sequences
  val beat: Beat,                           //  The current beat of all notes in the music
  val scaleNum: Integer = 1,                //  The number of notes in scaleBeats number of beats
  val scaleBeats: Integer = 1,              //  The number of beats in which scaleNum notes play
  val scaleDepth: Int = 0,                  //  The depth at which the BeatScale last changed
  val tiedAddition: Beat = NoDuration,      //  The duration by which the last note of the music should be lengthened 
  val timeSig: TimeSig,                     //  The current time signature of all bars in the music
  val noteWidth: Double,                    //  The proportion of the width each note sounds within its duration
  val volume: Int = Vmf.volume,              //  The volume of notes played
  val keySig: KeySig = CMaj,                //  The key signature
  val tonic: Note = C,                      //  The current tonic (usually the key)
  val isMinor: Boolean = false,             //  Is the current key a minor?
  val broken: Option[Broken] = None,        //  For a broken chord, the delay between the onset of each note in the chord
  val arpeggio: Option[Arpeggio] = None,    //  For an arpeggio or ornament, the pattern (sequence and beat) 
  val extractRootNotes: Boolean = false,    //  True to extract root note from each chord
  val currentInstrument: Music.Patch = Music.initialPatch,           //  The General Midi instrument on which notes are played
  val rhythmPattern: Vector[RhythmBeat] = Vector.empty,
  val noteRhythm: Vector[RhythmBeat] = Vector.empty,
  val rhythmStartNotes: Int = 0,            //  At what note does the rhythm start?
  val lyrics: Vector[String] = Vector.empty,
  val lyricsStartNotes: Int = 0,            //  At what note do the lyrics start?
  val dynamics: List[ContextDynamics] = Nil,//  The set of dynamics affecting the sequence
  val rangeLow: Note = N,                   //  The lowest Note to be used - octave-shifting as needed 
  val rangeHigh: Note = N,                  //  The highest Note to be used - octave-shifting as needed
  val onString: Option[Guitar.String] = None,  //  An optional Guitar string on which the Note will sound
  val guitarFrets: Option[FretSequence] = None,//  The Guitar Frets sequence which give pitches to the pick/strum patterns 
  val guitarFretsStartTime: Option[TimeState] = None,  //  The time at which the current guitarFrets started
  val onGuitar: Option[Guitar] = None)      //  An optional Guitar on one of whose strings the Note will sound
{
  //  The TimeState of the current duration at the current tempo
  def durationTiming(noteCount: Int) = 
  {
    if (!pendingTimingChanges.isEmpty)
    {
      pendingTimingChanges.foreach(timingTrack.add(_))
      pendingTimingChanges.clear()
    }
    if (!rhythmPattern.isEmpty)
    {
      if (noteCount != 0)
        TimeState( rhythmPattern((timeState.noteCount - rhythmStartNotes) % rhythmPattern.length).duration, noteCount, timeSig)
      else
        TimeState.empty(timeSig)
    }
    else if (noteCount != 0)
      TimeState( beat+tiedAddition, noteCount, timeSig) * scaleBeats / scaleNum
    else
      TimeState( beat, noteCount, timeSig) * scaleBeats / scaleNum
  }
  
  def rhythmPreRest =
  {
    if (!rhythmPattern.isEmpty)
      TimeState( rhythmPattern((timeState.noteCount - rhythmStartNotes) % rhythmPattern.length).preRest, 0, timeSig)
    else
      TimeState.empty(timeSig)
  }
  
  def rhythmPostRest =
  {
    if (!rhythmPattern.isEmpty)
      TimeState( rhythmPattern((timeState.noteCount - rhythmStartNotes) % rhythmPattern.length).postRest, 0, timeSig)
    else
      TimeState.empty(timeSig)
  }
  
  //  Write the specified Tempo to the timing track 
  def writeTempo(bpm: Int, timeState: TimeState) = {
    val bytearray = BigInt(60000000/bpm).toByteArray
    val pad = Array.fill[Byte](3-bytearray.length)(0)
    
    pendingTimingChanges += new M.MidiEvent(new M.MetaMessage(0x51, pad++bytearray, 3),timeState.ticks)    
  }
  
  //  Write the specified Time Signature to the timing track
  def writeTimeSig(number: Byte, beat: Beat, timeState: TimeState) = {
    //  On a compound time (more than one triplet in a bar), metronome clicks are every three beats
    val beatRate = Beat.TPQN * 24 / beat.beatTicks
    val clickRate = if (number % 3 == 0 && number > 3) 3 * beatRate else beatRate
    
    val bytearray = Array[Byte](number, beat.timeSigDenom, clickRate.toByte, 8)
    pendingTimingChanges += new M.MidiEvent(new M.MetaMessage(0x58, bytearray, bytearray.length),timeState.ticks)
  }
  
  //  Write the specified Key signature to the timing track 
  def writeKeySig(keySigSharps: Byte, keySigIsMinor: Boolean, timeState: TimeState) = {
    val currentKeySigSharps = 
      if (keySigSharps >= 0) 
        (keySigSharps + (7 * transpose) + 5) % 12 - 5  //  bias towards sharp
      else
        (keySigSharps + (7 * transpose) + 7) % 12 - 7  //  bias towards flat
    val bytearray = Array[Byte](keySigSharps.toByte, if (keySigIsMinor) 1 else 0)
    
    pendingTimingChanges += new M.MidiEvent(new M.MetaMessage(0x59, bytearray, bytearray.length), timeState.ticks)    
  }
  
  def writeLyrics(ticks: Int) =
  {
    if (lyrics.length != 0)
    {
      val lyricOffset = timeState.noteCount - lyricsStartNotes
      if (lyricOffset < lyrics.length)
      {
        val lyric = lyrics(lyricOffset)
        val bytearray = lyric.getBytes
        getTrack.add(new M.MidiEvent(new M.MetaMessage(0x05, bytearray, bytearray.length),ticks))
      }
    }

  }
  
  //  Get or create the named track, and when creating add its name to the track
  def getTrack = 
  {
    if (!tracks.contains(currentTrackName)) 
    {
      val bytearray = currentTrackName.getBytes

      tracks(currentTrackName)  = sequence.createTrack()
      tracks(currentTrackName).add(new M.MidiEvent(new M.MetaMessage(0x03, bytearray, bytearray.length), 0))    
    }
    tracks(currentTrackName) 
  }
  
  //  Get or allocate the named Midi channel
  def getChannel(channelName: String) = 
  {
    if (!channels.contains(channelName)) 
    {
      //  Allocate a new Channel number, excluding any already in use (such as Drums at 10)
      channels(channelName) = ((1 to 16).toSet -- channels.values).head
    }
    channels(channelName) 
  }
  
  //  Get or allocate the named Midi channel
  def getChannel: Int = 
  {
    getChannel(currentChannelName) 
  }
  
  def getDynamics = 
  {
    if (strict)
      PointDynamics()
    else
      dynamics.foldLeft(PointDynamics())((p,d) => p + d.dynamics.getAtTime(timeState-d.startTime))
  }
  
  def getNoteWidth = if (strict) 1.0 else noteWidth
}

private[euterpea] object SequenceContext
{
  //  The root SequenceContext for the sequence
  def apply(sequence: M.Sequence, strict: Boolean) = 
  {
    val timeSig=TimeSig(4,Quarter)                    // 4/4
    new SequenceContext(
          sequence=sequence,                          // The sequence being constructed
          strict=strict,
          timeState=TimeState.empty(timeSig),         // Start at the beginning
          timingTrack=sequence.createTrack(),
          tracks=new mutable.HashMap[String,M.Track], // An empty track mapping table
          channels=mutable.HashMap("Drums" -> 9),    // A Midi channel mapping table, where Drums are pre-allocated
          tempoBPM=120,                               // Default tempo
          noteWidth=(if (strict) Legato else DefaultWidth).noteWidth,           // Not quite legato
          timeSig=timeSig,                      
          beat=Quarter)                                // Default notes are quarter notes
  }
}

