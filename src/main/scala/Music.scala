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
  def add(context: SequenceContext) : TimeState
  
  //  Compute the duration of the music without adding it
  def duration(context: SequenceContext) : TimeState
  
  //  Play the constructed javax.sound.midi.Sequence object on the system's Midi sequencer
  private def playSequence(sequence: M.Sequence) {
    val seq = Music.sequencer
    
    seq.setSequence(sequence)
    seq.start()
    Thread.sleep(sequence.getMicrosecondLength/1000+1000);
    seq.stop();
    
    seq.close();
  }
  
  //  Make a javax.sound.midi.Sequence object from the Music structure by adding the root of
  //  the AST to a default context
  //  Returns the sequence and a time-sorted Seq of Error objects
  def makeSequence(strict: Boolean) = {
    //  The javax.sound.midi.Sequence object to be constructed
    val sequence = new M.Sequence(M.Sequence.PPQ, Beat.TPQN)
    
    //  Add the music by recursively adding the root
    val context = SequenceContext(sequence, strict)
    val timeState = add( context )
    timeState.stopAllStrings()
    
    (sequence, timeState.errors.sortBy(_.position.ticks))    
  }
  
  //  Check the music is well-formed
  def check(): Seq[(String,String)] = {
    val (sequence, errors) = makeSequence(false)
    errors.map(e => (e.position.display, e.message))
  }
  
  //  Play the music on the system's Midi sequencer
  def play(): Seq[(String,String)] = {
    val (sequence, errors) = makeSequence(false)
    if (errors.length == 0)
      playSequence(sequence)
    errors.map(e => (e.position.display, e.message))
  }
  
  //  Save the music as a Midi file, with the "strict" option to prevent timing and width changes 
  def save(path: String, strict: Boolean = false): Seq[(String,String)] = {
    val (sequence, errors) = makeSequence(strict)
    if (errors.length == 0)
      MS.write(sequence, 1, new File(path))
    errors.map(e => (e.position.display, e.message))
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
  //  requiring this timeState timing to be at a bar boundary of the current time signature 
  def |(a:Music) = new BarJoin(this,a)
  
  //  Construct music by adding this part and another part sequentially one after the other, 
  //  requiring this timeState timing to be at a bar boundary of the current time signature 
  def +|+(duration: Beat) = new BarExtend(this,duration)
  
  //  Apply a Modifier to the Music with the syntax as Music/Modifier
  //  This constructs subclass instances to represent the modification
  def / (mod: Modifier): Music = mod modifying this
  
  //  Apply a Modifier to the Music with the alternate syntax as Modifier/:Music
  def /: (mod: Modifier): Music = this / mod
  
  //  Repeat a piece of music a fixed number of times
  def * (repeat: Integer) = new Repeated(this, repeat)
}

//  Music object containing the singleton Midi synthesizer and sequencer 
//  into which good quality sounds have been loaded 
object Music 
{
  //  The single default Sequencer, linked to play on the default Synthesizer 
  lazy val sequencer = 
  {
    val seq = MS.getSequencer
    seq.open()
    
    val seqTrans = seq.getTransmitter();
    val synthRcvr = synthesizer.getReceiver(); 
    seqTrans.setReceiver(synthRcvr);    
    
    seq
  }
  
  //  The single default Synthesizer into which good quality sounds are loaded
  lazy val synthesizer = {
    val syn = MS.getSynthesizer
    syn.open()
    
    val sbf1 = new File("G:\\SoundFonts\\FluidR3_GM.SF2")
    val sb1 = MS.getSoundbank(sbf1)
    if (syn.isSoundbankSupported(sb1))
      syn.loadAllInstruments(sb1)
    
//    val sbf2 = new File("G:\\SoundFonts\\Guitar Bank 2.SF2")
//    val sb2 = MS.getSoundbank(sbf2)
//    if (syn.isSoundbankSupported(sb2))
//      syn.loadAllInstruments(sb2)
    
      syn
  }
  
  //  A Midi patch is a bank number and a program within that batch
  case class Patch(val bank: Int, val program: Int)
  val initialPatch = new Patch(0, 0)
  val errorPatch = new Patch(-1, -1)
  
  //  The set of all loaded instruments as a map from name to Patch
  def instruments: Map[String,Patch] =
  {
    synthesizer.getLoadedInstruments.map(i => (i.getName, new Patch (i.getPatch.getBank, i.getPatch.getProgram))).toMap
  }
}

//-------------------------

//  Music which has no content and plays no notes - used as the basis for a Chord
object EmptyMusic extends Music
{
  def add(context: SequenceContext) = context.durationTiming(0)
  def duration(context: SequenceContext) =  context.durationTiming(0)
}

//-------------------------

//  Music which has no notes, but has duration and reports an error message when added
case class ErrorMusic(message: String) extends Music
{
  def add(context: SequenceContext) =  context.durationTiming(0).error(message)
  def duration(context: SequenceContext) =  context.durationTiming(0)
}

//-------------------------

//  Combining two pieces of music sequentially by adding the first at the current timeState and then the second
//  at the timeState after the duration of the first
//  The two parts are at a lower structural depth
case class - (a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context.copy(tiedAddition=NoDuration, depth=context.depth+1))
    val durationTiming2 = b.add(context.copy(timeState = context.timeState+durationTiming1, depth=context.depth+1))
    
    durationTiming1 + durationTiming2
  }
  
  def duration(context: SequenceContext) =
  {
    val durationTiming1 = a.duration(context.copy(tiedAddition=NoDuration, depth=context.depth+1))
    val durationTiming2 = b.duration(context.copy(timeState = context.timeState+durationTiming1, depth=context.depth+1))
    
    durationTiming1 + durationTiming2
  }
}

//-------------------------

//  Combining two pieces of music sequentially, at the same time adjusting note width and volume to sound like a slur
//  The two parts are at a lower structural depth
case class Slur(a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context.copy(noteWidth=1.0, tiedAddition=NoDuration, depth=context.depth+1))
    val durationTiming2 = b.add(context.copy(noteWidth = context.noteWidth*0.7, volume = context.volume-Volume.VolumeInc, timeState = context.timeState+durationTiming1, depth=context.depth+1))
    
    durationTiming1 + durationTiming2
  }
  
  def duration(context: SequenceContext) =
  {
    val durationTiming1 = a.duration(context.copy(tiedAddition=NoDuration, depth=context.depth+1))
    val durationTiming2 = b.duration(context.copy(timeState = context.timeState+durationTiming1, depth=context.depth+1))
    
    durationTiming1 + durationTiming2
  }
}

//-------------------------

//  Combining two pieces of music sequentially, each of which must be whole bars
//  The two parts are at a lower structural depth
private[euterpea] case class BarJoin(a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context.copy(tiedAddition=NoDuration, depth=context.depth+1))
    val barPosition = context.timeState+durationTiming1
    
    val checkedDurationTiming = if (!(barPosition).isAtBar(context.timeSig))
    {
      durationTiming1.error("Bar line is not on a bar boundary")
    }
    else
      durationTiming1

    val durationTiming2 = b.add(context.copy(timeState = context.timeState+checkedDurationTiming, depth=context.depth+1))
    
    checkedDurationTiming + durationTiming2
  }
  
  def duration(context: SequenceContext) =
  {
    val durationTiming1 = a.duration(context.copy(tiedAddition=NoDuration, depth=context.depth+1))
    val durationTiming2 = b.duration(context.copy(timeState = context.timeState+durationTiming1, depth=context.depth+1))
    
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
    val barPosition = context.timeState+durationTiming-TimeState(tiedAddition)
    
    val checkedDurationTiming = if (!(barPosition).isAtBar(context.timeSig))
    {
      durationTiming.error("Bar line is not on a bar boundary")
    }
    else
      durationTiming
    
    checkedDurationTiming
  }
  
  def duration(context: SequenceContext) =
  {
    music.duration(context.copy(tiedAddition=tiedAddition))
  }
}

//  Repeat a piece of music a fixed number of times
private[euterpea] case class Repeated(music: Music, repeat: Integer) extends Music
{
  def add(context: SequenceContext) =
  {
    if (repeat == 0)
      TimeState(NoDuration)
    else
    {
      val durationTiming1 = music.add(context.copy(tiedAddition=NoDuration))
      val durationTiming2 = Repeated(music, repeat-1).add(context.copy(timeState=context.timeState+durationTiming1))
    
      durationTiming1 + durationTiming2
    }
  }
  
  def duration(context: SequenceContext) =
  {
    if (repeat == 0)
      TimeState(NoDuration)
    else
    {
      val durationTiming1 = music.duration(context.copy(tiedAddition=NoDuration))
      val durationTiming2 = Repeated(music, repeat-1).duration(context.copy(timeState=context.timeState+durationTiming1))
    
      durationTiming1 + durationTiming2
    }
  }
}

//-------------------------

//  Combining two pieces of music in parallel, by adding them both at the same timeState
private[euterpea] case class & (a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context)
    val durationTiming2 = b.add(context)
    
    durationTiming1 max durationTiming2  //  The duration is the duration of the longer of the two parts
  }

  def duration(context: SequenceContext) =
  {
    val durationTiming1 = a.duration(context)
    val durationTiming2 = b.duration(context)
    
    durationTiming1 max durationTiming2  //  The duration is the duration of the longer of the two parts
  }
}


