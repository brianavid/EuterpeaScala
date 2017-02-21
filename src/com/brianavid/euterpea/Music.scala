package com.brianavid.euterpea

import javax.sound.{midi => M}
import M.{MidiSystem => MS}
import scala.collection.mutable
import java.io.File

//-------------------------

trait Modifier

class Duration(val beats: Int) extends Modifier
{
  def dot = new Duration( beats * 3 / 2)
  def + (next: Duration) = new Duration( beats + next.beats)
}
object Duration  {
  val BPQN = 480
}

case object Wn extends Duration(Duration.BPQN*4)
case object Hn extends Duration(Duration.BPQN*2)
case object Qn extends Duration(Duration.BPQN)
case object En extends Duration(Duration.BPQN/2)
case object Sn extends Duration(Duration.BPQN/4)
case object Tn extends Duration(Duration.BPQN/8)
case class Triplet(base: Duration) extends Duration(base.beats * 2 / 3)

//-------------------------

abstract class Dynamics(val inc: Int) extends Modifier
case object Fv extends Dynamics(+10)
case object FFv extends Dynamics(+20)
case object Pv extends Dynamics(-10)
case object PPv extends Dynamics(-20)

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
  val volumeInc: Int = 0,
  val currentInstrument: Int = 1)
{
  val HiResResolution = 1000
  def durationHiRes: Long = duration.beats.toLong * 60000 * HiResResolution / Duration.BPQN / tempoBPM
  def positionMS(durationHiRes: Long) = ((position + durationHiRes + HiResResolution/2) / HiResResolution).toInt
}

//-------------------------

sealed trait Music
{
  def add(context: SequenceContext) : Long
  
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
  def &(a:Music) = new &(this,a)
  
  def / (mod: Modifier) = mod match 
  {
    case (dur: Duration) => new WithBeat (dur, this)
    case (vol: Dynamics) => new WithVolume(vol.inc, this)
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
  
  def add(context: SequenceContext) : Long =
  {
    val MiddleC = 60
    val track = context.tracks.getOrElseUpdate(context.currentTrackName, context.sequence.createTrack())
    val channel = context.channels.getOrElseUpdate(context.currentTrackName, context.channels.size)
    val pitch = MiddleC + semitones + octave*12 + context.transpose
    val noteDurationHiRes = context.durationHiRes
        
    //Console.println(s"${context.position} ${display}")
    
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.NOTE_ON, channel, pitch, 64+context.volumeInc),context.positionMS(0)))
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.NOTE_OFF, channel, pitch, 0),context.positionMS(noteDurationHiRes)))
    
    noteDurationHiRes
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
  def add(context: SequenceContext) : Long =
  {
    context.durationHiRes
  }
}


case class > (a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) : Long =
  {
    val duration1 = a.add(context)
    val duration2 = b.add(context.copy(position = context.position + duration1))
    duration1 + duration2
  }
}

//-------------------------

case class & (a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) : Long =
  {
    val duration1 = a.add(context)
    val duration2 = b.add(context)
    duration1 max duration2
  }
}

//-------------------------

case class WithBeat(duration: Duration, music: Music) extends Music
{
  def add(context: SequenceContext) : Long =
  {
    music.add(context.copy(duration=duration))
  }
}

//-------------------------

case class WithTempo( bpm: Int, music: Music) extends Music
{
  def add(context: SequenceContext) : Long =
  {
    music.add(context.copy(tempoBPM=bpm))
  }
}

//-------------------------

case class WithWidth( noteWidth: Double, music: Music) extends Music 
{
  def add(context: SequenceContext) : Long =
  {
    music.add(context.copy(noteWidth=noteWidth))
  }
}

case class WithTranspose(num: Int, music: Music) extends Music
{
  def add(context: SequenceContext) : Long =
  {
    music.add(context.copy(transpose = context.transpose + num))
  }
}

case class WithVolume(inc: Int, music: Music) extends Music
{
  def add(context: SequenceContext) : Long =
  {
    music.add(context.copy(volumeInc=context.volumeInc+inc))
  }
}

//-------------------------

case class WithTimeSig( beatsPerBar: Int, duration: Duration, music: Music) extends Music
{
  def add(context: SequenceContext) : Long =
  {
    music.add(context.copy())
  }
}

//-------------------------

case class WithTrack( trackName: String, music: Music) extends Music
{
  def add(context: SequenceContext) : Long =
  {
    music.add(context.copy(currentTrackName = trackName))
  }
}

//-------------------------

case class WithInstrument( instrument: Int, music: Music) extends Music
{
  def add(context: SequenceContext) : Long =
  {
    val track = context.tracks.getOrElseUpdate(context.currentTrackName, context.sequence.createTrack())
    val channel = context.channels.getOrElseUpdate(context.currentTrackName, context.channels.size)
    val prevInstrument = context.currentInstrument 
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.PROGRAM_CHANGE, channel, instrument-1, 0), context.positionMS(0)))
    val duration = music.add(context.copy(currentInstrument = instrument))
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.PROGRAM_CHANGE, channel, prevInstrument-1, 0), context.positionMS(duration)))
    duration
  }
}

//-------------------------

case class WithPulse( stressesPerBar: Int, stressValue: Int, music: Music) extends Music
{
  def add(context: SequenceContext) : Long =
  {
    music.add(context.copy())
  }
}

//-------------------------

case class WithSwing( duration: Duration, swing: Double, music: Music) extends Music
{
  def add(context: SequenceContext) : Long =
  {
    music.add(context.copy())
  }
}



