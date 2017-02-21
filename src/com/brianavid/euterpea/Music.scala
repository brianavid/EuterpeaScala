package com.brianavid.euterpea

import javax.sound.{midi => M}
import M.{MidiSystem => MS}
import scala.collection.mutable
import java.io.File

//-------------------------

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

sealed trait Modifier

//-------------------------

//  The Duration modifier denotes the beat of a Note or Rest.
//  At its basic, it is a Whole, Half, Quarter, Eighth ... note
//  Durations can be added together, dotted, or split into three (for triplets)

class Duration(val beats: Int) extends Modifier
{
  def dot = new Duration( beats * 3 / 2)  //  Note and a half
  def + (next: Duration) = new Duration( beats + next.beats) //  Simply added together
}

object Duration  {
  val BPQN = 960    //  The granularity of note length sub-divisions
}

case object Wn extends Duration(Duration.BPQN*4)  //  Whole note
case object Hn extends Duration(Duration.BPQN*2)  //  Half note
case object Qn extends Duration(Duration.BPQN)    //  Quarter note
case object En extends Duration(Duration.BPQN/2)  //  Eighth note
case object Sn extends Duration(Duration.BPQN/4)  //  Sixteenth note
case object Tn extends Duration(Duration.BPQN/8)  //  Thirty-second note
case class Triplet(base: Duration) extends Duration(base.beats * 2 / 3)

//-------------------------

abstract class Dynamics(val volume: Int) extends Modifier
case object MFv  extends Dynamics(64) 
case object Fv extends Dynamics(MFv.volume+10)
case object FFv extends Dynamics(MFv.volume+20)
case object Pv extends Dynamics(MFv.volume-10)
case object PPv extends Dynamics(MFv.volume-20)

case class Tempo (bpm: Int) extends Modifier

case class Width(noteWidth: Double) extends Modifier
object Staccato extends Width(0.3) 
object Legato extends Width(1.0) 

case class Transpose( num: Int) extends Modifier
case class Octave( num: Int) extends Modifier

case class Track( trackName: String) extends Modifier

case class Instrument( instrument: Int) extends Modifier
object Instrument
{
  def apply(instrumentName: String) = new Instrument(Instruments.instrumentByName.getOrElse(instrumentName, 1))
}

//-------------------------

case class SequenceContext (
  val sequence: M.Sequence,
  val currentTrackName: String,
  val tracks: mutable.Map[String,M.Track],
  val channels: mutable.Map[String,Int],
  val transpose: Int = 0,
  val tempoBPM: Int,
  val position: Long,
  val noteWidth: Double,
  val duration: Duration,
  val volume: Int = 64,
  val currentInstrument: Int = 1)
{
  def durationTicks: Int = duration.beats * 60 / tempoBPM
  def positionTicks(durationTicks: Int) = position + durationTicks
}

//-------------------------

sealed trait Music
{
  def add(context: SequenceContext) : Int
  
  private def playSequence(sequence: M.Sequence) {
    val seq = MS.getSequencer
    seq.open()
    seq.setSequence(sequence)
    seq.start()
    Thread.sleep(sequence.getMicrosecondLength/1000+1000);
    seq.stop();
    seq.close();
  }
  
  def makeSequence() = {
    val sequence = new M.Sequence(M.Sequence.PPQ, Duration.BPQN)
    val track = sequence.createTrack()
    val context = new SequenceContext(
        sequence=sequence, 
        currentTrackName="", 
        tracks=new mutable.HashMap[String,M.Track], 
        channels=new mutable.HashMap[String,Int], 
        tempoBPM=120, 
        position=0L, 
        noteWidth=0.9, 
        duration=Qn)
    add( context )
    sequence    
  }
  
  def play() : Unit = {
    val sequence = makeSequence()
    playSequence(sequence)
  }
  
  def writeMidiFile(path: String) : Unit = {
    val sequence = makeSequence()
    MS.write(sequence, 1, new File(path))
  }
  
  def >(a:Music) = new >(this,a)
  def --(a:Music) = new Slur(this,a)
  def &(a:Music) = new &(this,a)
  
  def / (mod: Modifier) = mod match 
  {
    case (dur: Duration) => new WithBeat (dur, this)
    case (vol: Dynamics) => new WithVolume(vol.volume, this)
    case Tempo(tempo) => new WithTempo( tempo, this) 
    case Width(width) => new WithWidth( width, this) 
    case Transpose( num: Int) => new WithTranspose( num, this)
    case Octave( num: Int) => new WithTranspose( num*12, this)
    case Track( trackName: String) => new WithTrack( trackName, this)
    case Instrument( instrument: Int) => new WithInstrument( instrument, this)
    case _ => this
  }
  def /: (mod: Modifier) = this / mod
}

object Line
{
  def apply(music: Music*) = music.reduce(_>_)
}

//-------------------------

case class Note( 
    semitones: Int, 
    display: String,
    octave: Int = 0) extends Music
{
  def unary_+ = copy(octave = octave+1)
  def unary_- = copy(octave = octave-1)
  
  def add(context: SequenceContext) : Int =
  {
    val MiddleC = 60
    val track = context.tracks.getOrElseUpdate(context.currentTrackName, context.sequence.createTrack())
    val channel = context.channels.getOrElseUpdate(context.currentTrackName, context.channels.size)
    val pitch = MiddleC + semitones + octave*12 + context.transpose
    val noteDurationTicks = context.durationTicks
        
    //Console.println(s"${context.position} ${display}")
    
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.NOTE_ON, channel, pitch, context.volume),context.positionTicks(0)))
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.NOTE_OFF, channel, pitch, 0),context.positionTicks(noteDurationTicks)))
    
    noteDurationTicks
  }
}

object Cf extends Note( -1, "Cf")
object C extends Note( 0, "C")
object Cs extends Note( 1, "Cs")
object Css extends Note( 2, "Css")
object Dff extends Note( 0, "Dff")
object Df extends Note( 1, "Df")
object D extends Note( 2, "D")
object Ds extends Note( 3, "Ds")
object Dss extends Note( 4, "Dss")
object Eff extends Note( 2, "Eff")
object Ef extends Note( 3, "Ef")
object E extends Note( 4, "E")
object Es extends Note( 5, "Es")
object Ff extends Note( 4, "Ff")
object F extends Note( 5, "F")
object Fs extends Note( 6, "Fs")
object Fss extends Note( 7, "Fss")
object Gff extends Note( 5, "Gff")
object Gf extends Note( 6, "Gf")
object G extends Note( 7, "G")
object Gs extends Note( 8, "Gs")
object Gss extends Note( 9, "Gss")
object Aff extends Note( 7, "Aff")
object Af extends Note( 8, "Af")
object A extends Note( 9, "A")
object As extends Note( 10, "As")
object Ass extends Note( 11, "Ass")
object Bff extends Note( 9, "Bff")
object Bf extends Note( 10, "Bf")
object B extends Note( 11, "B")
object Bs extends Note( 12, "Bs")

//-------------------------

case object Rest extends Music
{
  def add(context: SequenceContext) : Int =
  {
    context.durationTicks
  }
}


case class > (a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    val durationTicks1 = a.add(context)
    val durationTicks2 = b.add(context.copy(position = context.positionTicks(durationTicks1)))
    durationTicks1 + durationTicks2
  }
}

case class Slur(a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    val durationTicks1 = a.add(context.copy(noteWidth=1.0))
    val durationTicks2 = b.add(context.copy(noteWidth = context.noteWidth-0.2, volume = context.volume-10, position = context.positionTicks(durationTicks1)))
    durationTicks1 + durationTicks2
  }
}

//-------------------------

case class & (a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    val durationTicks1 = a.add(context)
    val durationTicks2 = b.add(context)
    durationTicks1 max durationTicks2
  }
}

//-------------------------

case class WithBeat(duration: Duration, music: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    music.add(context.copy(duration=duration))
  }
}

//-------------------------

case class WithTempo( bpm: Int, music: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    music.add(context.copy(tempoBPM=bpm))
  }
}

//-------------------------

case class WithWidth( noteWidth: Double, music: Music) extends Music 
{
  def add(context: SequenceContext) : Int =
  {
    music.add(context.copy(noteWidth=noteWidth))
  }
}

case class WithTranspose(num: Int, music: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    music.add(context.copy(transpose = context.transpose + num))
  }
}

case class WithVolume(volume: Int, music: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    music.add(context.copy(volume=volume))
  }
}

//-------------------------

case class WithTimeSig( beatsPerBar: Int, duration: Duration, music: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    music.add(context.copy())
  }
}

//-------------------------

case class WithTrack( trackName: String, music: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    music.add(context.copy(currentTrackName = trackName))
  }
}

//-------------------------

case class WithInstrument( instrument: Int, music: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    val track = context.tracks.getOrElseUpdate(context.currentTrackName, context.sequence.createTrack())
    val channel = context.channels.getOrElseUpdate(context.currentTrackName, context.channels.size)
    val prevInstrument = context.currentInstrument 
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.PROGRAM_CHANGE, channel, instrument-1, 0), context.positionTicks(0)))
    val durationTicks = music.add(context.copy(currentInstrument = instrument))
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.PROGRAM_CHANGE, channel, prevInstrument-1, 0), context.positionTicks(durationTicks)))
    durationTicks
  }
}

//-------------------------

case class WithPulse( stressesPerBar: Int, stressValue: Int, music: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    music.add(context.copy())
  }
}

//-------------------------

case class WithSwing( duration: Duration, swing: Double, music: Music) extends Music
{
  def add(context: SequenceContext) : Int =
  {
    music.add(context.copy())
  }
}



