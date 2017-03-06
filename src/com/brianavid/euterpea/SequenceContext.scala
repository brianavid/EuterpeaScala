package com.brianavid.euterpea
import javax.sound.{midi => M}
import M.{MidiSystem => MS}
import scala.collection.mutable

//  The tune is transformed into a javax.sound.midi.Sequence object for playing or saving
//  This transformation uses a SequenceContext object to specify prevailing modifier-specified 
//  state, which is passed around (and changed when necessary) through the processing of the Music AST 

case class SequenceContext (
  val sequence: M.Sequence,                 //  The sequence being constructed
  val position: Timing,                     //  The current position (hi res) where music will be added to the sequence
  val timingTrack: M.Track,
  val currentTrackName: String = "",        //  The current track name 
  val tracks: mutable.Map[String,M.Track],  //  The mapping of track named onto sequence tracks
  val currentChannelName: String = "",      //  The current channel name 
  val channels: mutable.Map[String,Int],    //  The mapping of track named onto (all different) Midi channels
  val transpose: Int = 0,                   //  Any specified prevailing chromatic transposition
  val tempoBPM: Int,                        //  The current tempo, in beats per minute
  val duration: Duration,                   //  The current duration of all notes in the music
  val tiedAddition: Duration = NoDuration,  //  The duration by which the last note of the music should be lengthened 
  val timeSig: TimeSig,                     //  The current time signature of all bars in the music
  val noteWidth: Double,                    //  The proportion of the width each note sounds within its duration
  val volume: Int = MFv.volume,             //  The volume of notes played
  val keySig: KeySig = CMaj,                //  The key signature
  val tonic: Note = C,                      //  The current tonic (usually the key)
  val isMinor: Boolean = false,             //  Is the current key a minor?
  val currentInstrument: Int = 1)           //  The General Midi instrument on which notes are played
{
  //  The Timing of the current duration at the current tempo
  def durationTiming = Timing( duration)
  
  //  Write the specified Tempo to the timing track 
  def writeTempo(bpm: Int, position: Timing) = {
    val bytearray = BigInt(60000000/bpm).toByteArray
    val pad = Array.fill[Byte](3-bytearray.length)(0)
    
    timingTrack.add(new M.MidiEvent(new M.MetaMessage(0x51, pad++bytearray, 3),position.ticks))    
  }
  
  //  Write the specified Time Signature to the timing track
  def writeTimeSig(number: Byte, dur: Duration, position: Timing) = {
    //  On a compound time (more than one triplet in a bar), metronome clicks are every three beats
    val beatRate = 96 / (1 << number)
    val clickRate = if (number % 3 == 0 && number > 3) 3 * beatRate else beatRate
    
    val bytearray = Array[Byte](number, dur.timeSigDenom, clickRate.toByte, 8)
    timingTrack.add(new M.MidiEvent(new M.MetaMessage(0x58, bytearray, bytearray.length),position.ticks))    
  }
  
  //  Write the specified Key signature to the timing track 
  def writeKeySig(keySigSharps: Byte, keySigIsMinor: Boolean, position: Timing) = {
    val bytearray = Array[Byte](keySigSharps, if (keySigIsMinor) 1 else 0)
    
    timingTrack.add(new M.MidiEvent(new M.MetaMessage(0x59, bytearray, bytearray.length),position.ticks))    
  }
  
  //  Get or create the named track, and when creating add its name to the track
  def getTrack = 
  {
    if (!tracks.contains(currentTrackName)) 
    {
      val bytearray = currentTrackName.getBytes

      tracks(currentTrackName)  = sequence.createTrack()
      tracks(currentTrackName).add(new M.MidiEvent(new M.MetaMessage(0x03, bytearray, bytearray.length),position.ticks))    
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
}

object SequenceContext
{
  //  The root SequenceContext for the sequence
  def apply(sequence: M.Sequence) = 
    new SequenceContext(
          sequence=sequence,                          // The sequence being constructed
          position=new Timing(0, Some(0)),            // Start at the beginning
          timingTrack=sequence.createTrack(),
          tracks=new mutable.HashMap[String,M.Track], // An empty track mapping table
          channels=mutable.HashMap("Drums" -> 10),    // A Midi channel mapping table, where Drums are pre-allocated
          tempoBPM=120,                               // Default tempo
          noteWidth=DefaultWidth.noteWidth,           // Not quite legato
          timeSig=TimeSig(4,Qd),                      // 4/4
          duration=Qd)                                // Default notes are quarter notes
}
