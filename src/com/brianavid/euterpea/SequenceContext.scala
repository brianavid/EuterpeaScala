package com.brianavid.euterpea
import javax.sound.{midi => M}
import M.{MidiSystem => MS}
import scala.collection.mutable

//  The tune is transformed into a javax.sound.midi.Sequence object for playing or saving
//  This transformation uses a SequenceContext object to specify prevailing modifier-specified 
//  state, which is passed around (and changed when necessary) through the processing of the Music AST 

case class SequenceContext (
  val sequence: M.Sequence,                 //  The sequence being constructed
  val timeState: TimeState,                 //  The current state (including position in ticks) where music will be added to the sequence
  val timingTrack: M.Track,                 //  The first track of the sequence to which tempo and time signature changes will be added
  val currentTrackName: String = "",        //  The current track name 
  val tracks: mutable.Map[String,M.Track],  //  The mapping of track named onto sequence tracks
  val currentChannelName: String = "",      //  The current channel name 
  val channels: mutable.Map[String,Int],    //  The mapping of track named onto (all different) Midi channels
  val transpose: Int = 0,                   //  Any specified prevailing chromatic transposition
  val dTrans: Option[(Chord,Chord)] = None, //  Any specified prevailing diatonic transposition
  val tempoBPM: Int,                        //  The current tempo, in beats per minute
  val beat: Beat,                           //  The current beat of all notes in the music
  val scaleNum: Integer = 1,                //  The number of notes in scaleBeats number of beats
  val scaleBeats: Integer = 1,              //  The number of beats in which scaleNum notes play
  val tiedAddition: Beat = NoDuration,      //  The duration by which the last note of the music should be lengthened 
  val timeSig: TimeSig,                     //  The current time signature of all bars in the music
  val noteWidth: Double,                    //  The proportion of the width each note sounds within its duration
  val volume: Int = Vmf.volume,              //  The volume of notes played
  val keySig: KeySig = CMaj,                //  The key signature
  val tonic: Note = C,                      //  The current tonic (usually the key)
  val isMinor: Boolean = false,             //  Is the current key a minor?
  val currentInstrument: Int = 1,           //  The General Midi instrument on which notes are played
  val rhythmPattern: Vector[Beat] = Vector.empty,
  val dynamics: List[ContextDynamics] = Nil)//  The set of dynamics affecting the sequence
{
  //  The TimeState of the current duration at the current tempo
  def durationTiming(noteCount: Int) = 
  {
    if (noteCount != 0 && !rhythmPattern.isEmpty)
      TimeState( rhythmPattern(timeState.noteCount % rhythmPattern.length), noteCount)
    else
      TimeState( beat, noteCount)
  }
  
  //  Write the specified Tempo to the timing track 
  def writeTempo(bpm: Int, timeState: TimeState) = {
    val bytearray = BigInt(60000000/bpm).toByteArray
    val pad = Array.fill[Byte](3-bytearray.length)(0)
    
    timingTrack.add(new M.MidiEvent(new M.MetaMessage(0x51, pad++bytearray, 3),timeState.ticks))    
  }
  
  //  Write the specified Time Signature to the timing track
  def writeTimeSig(number: Byte, beat: Beat, timeState: TimeState) = {
    //  On a compound time (more than one triplet in a bar), metronome clicks are every three beats
    val beatRate = Beat.TPQN * 24 / beat.beatTicks
    val clickRate = if (number % 3 == 0 && number > 3) 3 * beatRate else beatRate
    
    val bytearray = Array[Byte](number, beat.timeSigDenom, clickRate.toByte, 8)
    timingTrack.add(new M.MidiEvent(new M.MetaMessage(0x58, bytearray, bytearray.length),timeState.ticks))    
  }
  
  //  Write the specified Key signature to the timing track 
  def writeKeySig(keySigSharps: Byte, keySigIsMinor: Boolean, timeState: TimeState) = {
    val bytearray = Array[Byte](keySigSharps, if (keySigIsMinor) 1 else 0)
    
    timingTrack.add(new M.MidiEvent(new M.MetaMessage(0x59, bytearray, bytearray.length), timeState.ticks))    
  }
  
  //  Get or create the named track, and when creating add its name to the track
  def getTrack = 
  {
    if (!tracks.contains(currentTrackName)) 
    {
      val bytearray = currentTrackName.getBytes

      tracks(currentTrackName)  = sequence.createTrack()
      tracks(currentTrackName).add(new M.MidiEvent(new M.MetaMessage(0x03, bytearray, bytearray.length), timeState.ticks))    
    }
    tracks(currentTrackName) 
  }
  
  //  Get or allocate the named Midi channel
  def getChannel = 
  {
    if (!channels.contains(currentChannelName)) 
    {
      //  Allocate a new Channel number, excluding any already in use (such as Drums at 10)
      channels(currentChannelName) = ((1 to 16).toSet -- channels.values).head
    }
    channels(currentChannelName) 
  }
  
  def getDynamics = 
  {
    dynamics.foldLeft(PointDynamics())((p,d) => p + d.dynamics.getAtTime(timeState-d.startTime))
  }
}

object SequenceContext
{
  //  The root SequenceContext for the sequence
  def apply(sequence: M.Sequence) = 
    new SequenceContext(
          sequence=sequence,                          // The sequence being constructed
          timeState=new TimeState(0, 0, Some(0), ControlValues.empty),         // Start at the beginning
          timingTrack=sequence.createTrack(),
          tracks=new mutable.HashMap[String,M.Track], // An empty track mapping table
          channels=mutable.HashMap("Drums" -> 9),    // A Midi channel mapping table, where Drums are pre-allocated
          tempoBPM=120,                               // Default tempo
          noteWidth=DefaultWidth.noteWidth,           // Not quite legato
          timeSig=TimeSig(4,Quarter),                      // 4/4
          beat=Quarter)                                // Default notes are quarter notes
}

