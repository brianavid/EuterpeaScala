package com.brianavid.euterpea

import javax.sound.{midi => M}
import M.{MidiSystem => MS}
import scala.collection.mutable
import java.io.File

//------------------------------------------------------------------------------------------------------------

//  Midi tunes are constructed from objects of trait Music and of trait Modifier.
//
//  Music objects can be combined into an abstract syntax tree of smaller Music objects,
//  combined in various way - in particular sequentially (e.g. for lines of melody) and 
//  in parallel (e.g. for chords or multiple tracks). Everything from a complete tune down
//  to an individual Note or Rest is Music.
//  
//  Modifier objects can be applied to any and all Music objects to affect how the Music is 
//  played. Modifiers can affect many aspects, such as tempo, note duration and width, 
//  instrument, volume, etc.

//------------------------------------------------------------------------------------------------------------

//  All music in a tune is constructed as an abstract syntax tree of Music objects

trait Music
{
  //  Add the music to the specified SequenceContext
  def add(context: SequenceContext) : Timing
  
  //  Play the constructed javax.sound.midi.Sequence object on the system's Midi sequencer
  private def playSequence(sequence: M.Sequence) {
    val seq = MS.getSequencer
    seq.open()
    seq.setSequence(sequence)
    seq.start()
    Thread.sleep(sequence.getMicrosecondLength/1000+1000);
    seq.stop();
    seq.close();
  }
  
  //  Make a javax.sound.midi.Sequence object from the Music structure by adding the root of
  //  the AST to a default context
  def makeSequence() = {
    //  The javax.sound.midi.Sequence object to be constructed
    val sequence = new M.Sequence(M.Sequence.PPQ, Beat.TPQN)
    
    //  Add the music by recursively adding the root
    add( SequenceContext(sequence) )
    
    sequence    
  }
  
  //  Play the music on the system's Midi sequencer
  def play() : Unit = {
    val sequence = makeSequence()
    playSequence(sequence)
  }
  
  //  Save the music as aMidit file
  def writeMidiFile(path: String) : Unit = {
    val sequence = makeSequence()
    MS.write(sequence, 1, new File(path))
  }
  
  //  Construct music by adding this part and another part sequentially one after the other, 
  //  e.g. as a line of melody
  def -(a:Music) = new -(this,a)
  
  //  A variant of sequential construction that adjusts Modifiers to make a Slur sound better
  def --(a:Music) = new Slur(this,a)
  
  //  Construct music by adding this part and another part at the same time,
  //  e.g. as a chord, or multiple instrument tracks
  def &(a:Music) = new &(this,a)
  
  //  Construct music by adding this part and another part sequentially one after the other, 
  //  requiring this position to be at a bar of the current time signature 
  def |(a:Music) = new BarJoin(this,a)
  
  //  Construct music by adding this part and another part sequentially one after the other, 
  //  requiring this position to be at a bar of the current time signature 
  def -|-(duration: Beat) = new BarExtend(this,duration)
  
  //  Apply a Modifier to the Music with the syntax as Music/Modifier
  //  This constructs subclass instances to represent the modification
  def / (mod: Modifier) = mod match 
  {
    case beat: Beat => new WithBeat (beat, this)
    case BeatScale(numberOfNotes, numberOfBeats) => new WithBeatScale(numberOfNotes, numberOfBeats, this)
    case vol: Volume => new WithVolume(vol.volume, this)
    case Tempo(tempo) => new WithTempo( tempo, this) 
    case TimeSig(number: Byte, beat: Beat) => new WithTimeSig( number, beat, this) 
    case Width(width) => new WithWidth( width, this) 
    case Transpose( num: Int) => new WithTranspose( num, this)
    case Octave( num: Int) => new WithTranspose( num*12, this)
    case keySig: KeySig => new WithKeySig( keySig, this)
    case Track( trackName: String) => new WithTrack( trackName, this)
    case Channel( channelName: String) => new WithChannel( channelName, this)
    case Instrument( instrument: Int) => new WithInstrument( instrument, this)
    case _ => this
  }
  
  //  Apply a Modifier to the Music with the alternate syntax as Modifier/:Music
  def /: (mod: Modifier) = this / mod
  
  def * (repeat: Integer) = new Repeated(this, repeat)
}

//-------------------------

//  
object EmptyMusic extends Music
{
  def add(context: SequenceContext) =  Timing(NoDuration)
}

//-------------------------

//  Combining two pieces of music sequentially by adding the first at the current position and then the second
//  at the position after the duration of the first
case class - (a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context.copy(tiedAddition=NoDuration))
    val durationTiming2 = b.add(context.copy(position = context.position+durationTiming1))
    
    durationTiming1 + durationTiming2
  }
}

//-------------------------

//  Combining two pieces of music sequentially, at the same time adjusting note width and volume to sound like a slur
case class Slur(a: Music, b: Music) extends Music
{
  def  / (lyric: String) = new WithLyric(lyric, this)
  def  /: (lyric: String) = new WithLyric(lyric, this)
  
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context.copy(noteWidth=1.0, tiedAddition=NoDuration))
    val durationTiming2 = b.add(context.copy(noteWidth = context.noteWidth-0.2, volume = context.volume-10, position = context.position+durationTiming1))
    
    durationTiming1 + durationTiming2
  }
}

//-------------------------

//  Combining two pieces of music sequentially, each of which must be whole bars
case class BarJoin(a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context.copy(tiedAddition=NoDuration))
    val barPosition = context.position+durationTiming1
    
    if (!(barPosition).isAtBar(context.timeSig))
    {
      Console.println(s"Bar line at postition ${barPosition.ticks} is not on a bar boundary")
    }

    val durationTiming2 = b.add(context.copy(position = barPosition))
    
    durationTiming1 + durationTiming2
  }
}

//-------------------------

//  Extending the last note or chord of a piece of music, which must be whole bars, 
//  by the tied addition at the start of the next bar
//  This allows verification of bars even when a note spans a bar boundary
case class BarExtend(music: Music, tiedAddition: Beat) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming = music.add(context.copy(tiedAddition=tiedAddition))
    val barPosition = context.position+durationTiming-Timing(tiedAddition)
    
    if (!(barPosition).isAtBar(context.timeSig))
    {
      Console.println(s"Bar line at postition ${barPosition.ticks} is not on a bar boundary")
    }
    
    durationTiming
  }
}

//  Repeat a piece of music a fixed number of times
case class Repeated(music: Music, repeat: Integer) extends Music
{
  def add(context: SequenceContext) =
  {
    if (repeat == 0)
      Timing(NoDuration)
    else
    {
      val durationTiming1 = music.add(context.copy(tiedAddition=NoDuration))
      val durationTiming2 = Repeated(music, repeat-1).add(context.copy(position=context.position+durationTiming1))
    
      durationTiming1 + durationTiming2
    }
  }
}

//-------------------------

//  Combining two pieces of music in parallel, by adding them both at the same position
case class & (a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context)
    val durationTiming2 = b.add(context)
    
    durationTiming1 max durationTiming2  //  The duration is the duration of the longer of the two parts
  }
}

//-------------------------

//  Add the music, with a changed current duration

case class WithBeat(beat: Beat, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(beat=beat))
  }
}

//-------------------------

//  Add the music, scaling the beat for (e.g.) triplets or quintuplets

case class WithBeatScale(numberOfNotes: Integer, numberOfBeats: Integer, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(scaleNum=numberOfNotes,scaleBeats=numberOfBeats))
  }
}

//-------------------------

//  Add the music, with a changed current tempo

case class WithTempo( bpm: Int, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val saveBPM=context.tempoBPM
    context.writeTempo(bpm, context.position)
    val durationTiming = music.add(context.copy(tempoBPM=context.tempoBPM))
    context.writeTempo(saveBPM, context.position+durationTiming)
    durationTiming
  }
}

//-------------------------

//  Add the music, with a changed current time signature

case class WithTimeSig( number: Byte, beat: Beat, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    //  The use of settingTimeSigChange causes the current position to be identified as the time at 
    //  which the time signature changed to allow determination of bar boundaries,
    //  which is necessary for bar line validation, pulse or swing 
    val saveTimeSig=context.timeSig
    context.writeTimeSig(number, beat, context.position)
    val durationTiming = music.add(context.copy( timeSig=TimeSig(number, beat), position=context.position.settingTimeSigChange))
    context.writeTimeSig(saveTimeSig.number, saveTimeSig.duration, context.position+durationTiming)
    durationTiming.settingTimeSigChange
  }
}

//-------------------------

//  Add the music, with a changed current noteWidth

case class WithWidth( noteWidth: Double, music: Music) extends Music 
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(noteWidth=noteWidth))
  }
}

//-------------------------

//  Add the music, with a changed current transposition

case class WithTranspose(num: Int, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(transpose = context.transpose + num))
  }
}

//-------------------------

//  Add the music, with a changed key signature

case class WithKeySig(keySig: KeySig, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val saveKeySig=context.keySig
    context.writeKeySig(keySig.keySigSharps, keySig.isMinor, context.position)
    val durationTiming = music.add(context.copy(keySig = keySig, tonic=keySig.tonic, isMinor=keySig.isMinor))
    context.writeKeySig(saveKeySig.keySigSharps, saveKeySig.isMinor, context.position+durationTiming)
    durationTiming
  }
}

//-------------------------

//  Add the music, with a changed current note volume

case class WithVolume(volume: Int, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(volume=volume))
  }
}

//-------------------------

//  Add the music, with a changed current track name, which may or may not already exist in the tune

case class WithTrack( trackName: String, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(currentTrackName = trackName))
  }
}

//-------------------------

//  Add the music, with a changed current channel

case class WithChannel( channelName: String, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(currentChannelName = channelName))
  }
}

//-------------------------

//  Add the music, with a changed current instrument number (from the General Midi set of instruments

case class WithInstrument( instrument: Int, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val track = context.getTrack
    val channel = context.channels.getOrElseUpdate(context.currentTrackName, context.channels.size)
    val prevInstrument = context.currentInstrument 
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.PROGRAM_CHANGE, channel, instrument-1, 0), context.position.ticks))
    val durationTiming = music.add(context.copy(currentInstrument = instrument))
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.PROGRAM_CHANGE, channel, prevInstrument-1, 0), context.position.ticks+durationTiming.ticks))
    durationTiming
  }
}

//-------------------------

//  Add the music, with the volume altered within each bar to add a stress pulse 

case class WithPulse( stressesPerBar: Int, stressVolumeIncrement: Int, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy())
  }
}

//-------------------------

//  Add the music, with the timing altered within each bar to add a swing 

case class WithSwing( duration: Beat, swing: Double, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy())
  }
}

//-------------------------

//  Add the music, which will ne a single Note, with the lyric associated with the time of the note 

case class WithLyric( lyric: String, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val bytearray = lyric.getBytes

    val track = context.getTrack
    track.add(new M.MidiEvent(new M.MetaMessage(0x05, bytearray, bytearray.length),context.position.ticks))    
    music.add(context.copy())
  }
}



