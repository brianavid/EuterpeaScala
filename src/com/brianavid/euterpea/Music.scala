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

//  The Timing classs is a high resolution representation of the duration and position of music
//  Use of a higher resolution reduces drift in timing due to rounding errors and tempo changes
case class Timing(val hiResTime: Long)
{
  //  Timings can be added
  def +(t: Timing) = new Timing(hiResTime + t.hiResTime)
  
  //  The later of two timings
  def max(t: Timing) = if (t.hiResTime > hiResTime) t else this
  
  //  Reduce the Timing to the lower resolution ticks
  def ticks = ((hiResTime + Timing.resolution/2) / Timing.resolution).toInt
}

object Timing
{
  val resolution = 1000    //  The granularity of computation within a beat
  
  //  Construct the timing object for the number of beats and a tempo
  def apply(duration: Duration) = new Timing( duration.beats * resolution )
}

//------------------------------------------------------------------------------------------------------------

sealed trait Modifier

//------------------------------------------------------------------------------------------------------------

//  The Duration Modifier denotes the beat of a Note or Rest.
//  At its basic, it is a Whole, Half, Quarter, Eighth ... note
//  Durations can be added together, dotted, or split into three (for triplets)

class Duration(val beats: Int, val timeSigDenom: Byte) extends Modifier
{
  def dot = new Duration( beats * 3 / 2, timeSigDenom)  //  Note and a half
  def + (next: Duration) = new Duration( beats + next.beats, timeSigDenom) //  Simply added together
}

object Duration
{
  val BPQN = 480    //  The granularity of note length sub-divisions
}

case object Wn extends Duration(Duration.BPQN*4, 0)  //  Whole note
case object Hn extends Duration(Duration.BPQN*2, 1)  //  Half note
case object Qn extends Duration(Duration.BPQN, 2)    //  Quarter note
case object En extends Duration(Duration.BPQN/2, 3)  //  Eighth note
case object Sn extends Duration(Duration.BPQN/4, 4)  //  Sixteenth note
case object Tn extends Duration(Duration.BPQN/8, 5)  //  Thirty-second note
case class Triplet(base: Duration) extends Duration(base.beats * 2 / 3, base.timeSigDenom)


//-------------------------

//  The Volume Modifier controls the volumes of notes

class Volume(val volume: Int) extends Modifier
case object MFv  extends Volume(64)           //  Normal (default) volume
case object Fv extends Volume(MFv.volume+10)  //  Louder (Forte)
case object FFv extends Volume(MFv.volume+20) //  Even louder
case object Pv extends Volume(MFv.volume-10)  //  Quieter (piano)
case object PPv extends Volume(MFv.volume-20) //  Even quieter

//  The Tempo Modifier controls and varies the tempo of the music

case class Tempo (bpm: Int) extends Modifier

//  The TimeSig Modifier controls and varies the time signature of the music

case class TimeSig (number: Byte, duration: Duration) extends Modifier

//  The Width Modifier controls the proportion of the time that a note actually 
//  sounds within its duration 

case class Width(noteWidth: Double) extends Modifier
object DefaultWidth extends Width(0.9)
object Staccato extends Width(0.3)            //  Short and marked
object Marcato extends Width(0.7)             //  Notes clearly separated  
object Legato extends Width(1.0)              //  Notes flowing together

//  The Transpose Modifier transposes chromatically all notes in its music up and down a number of semitones

case class Transpose( num: Int) extends Modifier

//  The Ocatve Modifier transposes all notes in its music up and down a number of octaves

case class Octave( num: Int) extends Modifier

//  The Track Modifier specifies that its music goes in the named track and so is distinct
//  (e.g. has a different instrument) from any music on any other named track 

case class Track( trackName: String) extends Modifier

//  The Instrument Modifier specifies the General Midi instrument on which the music should sound
//  It can be specified as an integer value (with constants defined in the Instruments object)
//  or as an instrument name string

case class Instrument( instrument: Int) extends Modifier
object Instrument
{
  def apply(instrumentName: String) = new Instrument(Instruments.instrumentByName.getOrElse(instrumentName, 1))
}

//  FUTURE MODIFIER CLASS DEFINITIONS GO HERE

//------------------------------------------------------------------------------------------------------------

//  The tune is transformed into a javax.sound.midi.Sequence object for playing or saving
//  This transformation uses a SequenceContext object to specify prevailing modifier-specified 
//  state, which is passed around (and changed when necessary) through the processing of the Music AST 

case class SequenceContext (
  val sequence: M.Sequence,                 //  The sequence being constructed
  val position: Timing,                     //  The current position (hi res) where music will be added to the sequence
  val timingTrack: M.Track,
  val currentTrackName: String,             //  The current track name 
  val tracks: mutable.Map[String,M.Track],  //  The mapping of track named onto sequence tracks
  val channels: mutable.Map[String,Int],    //  The mapping of track named onto (all different) Midi channels
  val transpose: Int = 0,                   //  Any specified prevailing chromatic transposition
  val tempoBPM: Int,                        //  The current tempo, in beats per minute
  val duration: Duration,                   //  The current duration of all notes in the music
  val timeSig: TimeSig,                     //  The current time signature of all bars in the music
  val noteWidth: Double,                    //  The proportion of the width each note sounds within its duration
  val volume: Int = MFv.volume,             //  The volume of notes played
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
    val bytearray = Array[Byte](number, dur.timeSigDenom, 24, 8)
    timingTrack.add(new M.MidiEvent(new M.MetaMessage(0x58, bytearray, bytearray.length),position.ticks))    
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
}

//------------------------------------------------------------------------------------------------------------

//  All music in a tune is constructed as an abstract syntax tree of Music objects

sealed trait Music
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
    val sequence = new M.Sequence(M.Sequence.PPQ, Duration.BPQN)
    
    //  The default root context
    val context = new SequenceContext(
        sequence=sequence,                          // The sequence being constructed
        position=Timing(0),                         // Start at the beginning
        timingTrack=sequence.createTrack(),
        currentTrackName="", 
        tracks=new mutable.HashMap[String,M.Track], // An empty track mapping table
        channels=new mutable.HashMap[String,Int],   // An empty Midi channel mapping table
        tempoBPM=120,                               // Default tempo
        noteWidth=DefaultWidth.noteWidth,           // Not quite legato
        timeSig=TimeSig(4,Qn),                      // 4/4
        duration=Qn)                                // Default notes are quarter notes
    
    //  Add the music by recursively adding the root
    add( context )
    
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
  def >(a:Music) = new >(this,a)
  
  //  A variant of sequential construction that adjusts Modifiers to make a Slur sound better
  def --(a:Music) = new Slur(this,a)
  
  //  Construct music by adding this part and another part at the same time,
  //  e.g. as a chord, or multiple instrument tracks
  def &(a:Music) = new &(this,a)
  
  //  Apply a Modifier to the Music with the syntax as Music/Modifier
  //  This constructs subclass instances to represent the modification
  def / (mod: Modifier) = mod match 
  {
    case (dur: Duration) => new WithBeat (dur, this)
    case (vol: Volume) => new WithVolume(vol.volume, this)
    case Tempo(tempo) => new WithTempo( tempo, this) 
    case TimeSig(number: Byte, dur: Duration) => new WithTimeSig( number, dur, this) 
    case Width(width) => new WithWidth( width, this) 
    case Transpose( num: Int) => new WithTranspose( num, this)
    case Octave( num: Int) => new WithTranspose( num*12, this)
    case Track( trackName: String) => new WithTrack( trackName, this)
    case Instrument( instrument: Int) => new WithInstrument( instrument, this)
    case _ => this
  }
  
  //  Apply a Modifier to the Music with the alternate syntax as Modifier/:Music
  def /: (mod: Modifier) = this / mod
}

//  Helper object for a melody line of music which can be written with commas in 
//  place of the ">" operator
object Line
{
  def apply(music: Music*) = music.reduce(_>_)
}

//-------------------------

//  The Note is the subclass of Music that plays a sound, with the Instrument, Duration and Volume
//  specified (by Modifiers) and stored in the current SequenceContext

case class Note( 
    semitones: Int,                 //  Semitones from Middle C
    display: String,                //  A string to display the note (for tracing only)
    octave: Int = 0)                //  The octave in which the note plays (default starts on Middle C)
  extends Music
{
  def unary_+ = copy(octave = octave+1)  //  The Note plays one octave higher 
  def unary_- = copy(octave = octave-1)  //  The Note plays one octave lower
  
  def  / (lyric: String) = new WithLyric(lyric, this)
  def  /: (lyric: String) = new WithLyric(lyric, this)
  
  //  Add the Note to the sequence at the current position with the Instrument, Duration and Volume
  //  specified in the current SequenceContext
  def add(context: SequenceContext) =
  {
    val MiddleC = 60
    
    //  Get the track identified by the track name, creating it if it does not exist
    val track = context.getTrack
    
    //  Get the Midi channel identified by the track name, creating it if it does not exist
    val channel = context.channels.getOrElseUpdate(context.currentTrackName, context.channels.size)
    
    //  The pitch at which the note plays, taking into account the octave and any transposition
    val pitch = MiddleC + semitones + octave*12 + context.transpose
    
    //  How long does the note last (although only sounding for part of it)
    val noteTiming = context.durationTiming
        
    //Console.println(s"${context.position} ${display}")
    
    //  Where does the note start and end playing, taking into account the note width
    val startTicks = context.position.ticks
    val endTicks = startTicks + (noteTiming.ticks * context.noteWidth).toInt
    
    //  Add Midi events to start and end the note at the right pitch, volume and timing
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.NOTE_ON, channel, pitch, context.volume),startTicks))
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.NOTE_OFF, channel, pitch, 0),endTicks))
    
    noteTiming
  }
}

//  Every note in all scales are individually named
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

//  A Rest object adds nothing to the sequence, but advances the current position according to its duration and tempo

case object Rest extends Music
{
  def add(context: SequenceContext) =
  {
    context.durationTiming
  }
}

//  Combining two pieces of music sequentially by adding the first at the current position and then the second
//  at the position after the duration of the first
case class > (a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context)
    val durationTiming2 = b.add(context.copy(position = context.position+durationTiming1))
    
    durationTiming1 + durationTiming2
  }
}

//-------------------------

//  Combining two pieces of music sequentially, at the same time adjusting note width and volume to sound like a slur
case class Slur(a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context.copy(noteWidth=1.0))
    val durationTiming2 = b.add(context.copy(noteWidth = context.noteWidth-0.2, volume = context.volume-10, position = context.position+durationTiming1))
    
    durationTiming1 + durationTiming2
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

case class WithBeat(duration: Duration, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(duration=duration))
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

case class WithTimeSig( number: Byte, dur: Duration, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val saveTimeSig=context.timeSig
    context.writeTimeSig(number, dur, context.position)
    val durationTiming = music.add(context.copy( timeSig=TimeSig(number, dur)))
    context.writeTimeSig(saveTimeSig.number, saveTimeSig.duration, context.position+durationTiming)
    durationTiming
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

case class WithSwing( duration: Duration, swing: Double, music: Music) extends Music
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



