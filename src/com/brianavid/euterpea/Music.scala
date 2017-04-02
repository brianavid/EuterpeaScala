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
  def makeSequence(strict: Boolean) = {
    //  The javax.sound.midi.Sequence object to be constructed
    val sequence = new M.Sequence(M.Sequence.PPQ, Beat.TPQN)
    
    //  Add the music by recursively adding the root
    val context = SequenceContext(sequence, strict)
    val timeState = add( context )
    
    (sequence, timeState.errors)    
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
    val (sequence, errors) = makeSequence(false)
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
  def / (mod: Modifier): Music = mod match 
  {
    case beat: Beat => new WithBeat (beat, this)
    case BeatScale(numberOfNotes, numberOfBeats) => new WithBeatScale(numberOfNotes, numberOfBeats, this)
    case vol: Volume => new WithVolume(vol.volume, this)
    case volChange: VolumeChange => new WithVolumeChange(volChange.toVolume.volume-volChange.fromVolume.volume, 
                                                         new WithVolume(volChange.fromVolume.volume, this))
    case Tempo(tempo, toBpm) => new WithTempo( tempo, toBpm, this) 
    case TimeSig(number: Byte, beat: Beat) => new WithTimeSig( number, beat, this) 
    case Width(width) => new WithWidth( width, this) 
    case Transpose( num: Int, None) => new WithTranspose( num, this)
    case Transpose( 0, Some((fromChord, toChord))) => new WithDiatonic( fromChord, toChord, this)
    case Octave( num: Int) => new WithTranspose( num*12, this)
    case keySig: KeySig => new WithKeySig( keySig, this)
    case Modulate(keySig) => new WithModulation( keySig, this)
    case Track( trackName: String) => new WithTrack( trackName, this)
    case Channel( channelName: String) => new WithChannel( channelName, this)
    case Instrument( instrument: Int) => new WithInstrument( instrument, this)
    case Rhythm( rhythmMusic: Music) => WithRhythm(rhythmMusic, this)
    case dynamics: Dynamics => new WithDynamics(dynamics, this)
    case Range(rangeLow: Note,rangeHigh: Note) => new WithRange(rangeLow, rangeHigh, this)
    case Lyric(lyric) => new WithLyric(lyric, this)
    case _ => this
  }
  
  //  Apply a Modifier to the Music with the alternate syntax as Modifier/:Music
  def /: (mod: Modifier): Music = this / mod
  
  //  A sequence (e.g. a List) of Modifiers can be used as a Modifier
  def / (mods: Seq[Modifier]): Music = 
    mods.foldLeft(this)((music, mod) => music/mod)
 
  //  Apply a Modifier sequence to the Music with the alternate syntax as Seq[Modifier]/:Music
  def /: (mods: Seq[Modifier]): Music = this / mods
 
//  Repeat a piece of music a fixed number of times
  def * (repeat: Integer) = new Repeated(this, repeat)
  
  def rhythmTimings(context: SequenceContext) =
  {
    def getTimings(m: Music, context: SequenceContext) : List[Beat] =
    {
      //  The only things that affect rhythm are the sequence of Notes and bars and the 
      //  Beat and BeatScale modifiers
      //  Normally the rhythm will be a simple single note melody or drum pattern, which may
      //  use the (unplayable) Note "?" for clarity
      m match
      {
        case m1 - m2 => getTimings(m1,context) ::: getTimings(m2,context)
        case BarJoin(m1,m2) => getTimings(m1,context) ::: getTimings(m2,context)
        case Repeated(m1,repeat) => (1 to repeat).map(i => getTimings(m1,context)).flatten.toList
        case WithBeat(beat, m1) => getTimings(m1,context.copy(beat=beat))
        case WithBeatScale(numberOfNotes, numberOfBeats, m1) => getTimings(m1,context.copy(scaleNum=numberOfNotes,scaleBeats=numberOfBeats))
        case _ => List(new Beat(m.duration(context).ticks))
      }
    }
    getTimings(this, context).toVector
  }
}

//-------------------------

//  
object EmptyMusic extends Music
{
  def add(context: SequenceContext) =  TimeState(NoDuration)
  def duration(context: SequenceContext) =  TimeState(NoDuration)
}

//-------------------------

//  Combining two pieces of music sequentially by adding the first at the current timeState and then the second
//  at the timeState after the duration of the first
case class - (a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context.copy(tiedAddition=NoDuration))
    val durationTiming2 = b.add(context.copy(timeState = context.timeState+durationTiming1))
    
    durationTiming1 + durationTiming2
  }
  
  def duration(context: SequenceContext) =
  {
    val durationTiming1 = a.duration(context.copy(tiedAddition=NoDuration))
    val durationTiming2 = b.duration(context.copy(timeState = context.timeState+durationTiming1))
    
    durationTiming1 + durationTiming2
  }
}

//-------------------------

//  Combining two pieces of music sequentially, at the same time adjusting note width and volume to sound like a slur
case class Slur(a: Music, b: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val durationTiming1 = a.add(context.copy(noteWidth=1.0, tiedAddition=NoDuration))
    val durationTiming2 = b.add(context.copy(noteWidth = context.noteWidth*0.7, volume = context.volume-Volume.VolumeInc, timeState = context.timeState+durationTiming1))
    
    durationTiming1 + durationTiming2
  }
  
  def duration(context: SequenceContext) =
  {
    val durationTiming1 = a.duration(context.copy(tiedAddition=NoDuration))
    val durationTiming2 = b.duration(context.copy(timeState = context.timeState+durationTiming1))
    
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
    val barPosition = context.timeState+durationTiming1
    
    val checkedDurationTiming = if (!(barPosition).isAtBar(context.timeSig))
    {
      durationTiming1.error("Bar line is not on a bar boundary")
    }
    else
      durationTiming1

    val durationTiming2 = b.add(context.copy(timeState = context.timeState+checkedDurationTiming))
    
    checkedDurationTiming + durationTiming2
  }
  
  def duration(context: SequenceContext) =
  {
    val durationTiming1 = a.duration(context.copy(tiedAddition=NoDuration))
    val durationTiming2 = b.duration(context.copy(timeState = context.timeState+durationTiming1))
    
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
case class Repeated(music: Music, repeat: Integer) extends Music
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
case class & (a: Music, b: Music) extends Music
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

//-------------------------

//  Add the music, with a changed current duration

case class WithBeat(beat: Beat, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(beat=beat))
  }
  
  def duration(context: SequenceContext) = music.duration(context.copy(beat=beat))
}

//-------------------------

//  Add the music, scaling the beat for (e.g.) triplets or quintuplets

case class WithBeatScale(numberOfNotes: Integer, numberOfBeats: Integer, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(scaleNum=numberOfNotes,scaleBeats=numberOfBeats))
  }
  
  def duration(context: SequenceContext) = music.duration(context.copy(scaleNum=numberOfNotes,scaleBeats=numberOfBeats))
}

//-------------------------

//  Add the music, with a changed (and optionally changing) current tempo

case class WithTempo( bpm: Int, toBpm: Option[Int], music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val saveBPM=context.tempoBPM
    context.writeTempo(bpm, context.timeState)
    //  Does the tempo change from bpm to toBpm over the duration of the music?
    toBpm match
    {
      case None => 
        ;          // No gradual Tempo change within the music (accel or rall)
        
      case Some(toBpmValue) =>
        //  The Tempo changes from bpm to toBpm over the duration of the music
        val changeDuration = music.duration(context)
        //  We do this in a granularity of a tenth of the music
        for (i <- 1 to 9)
          context.writeTempo(bpm + (toBpmValue-bpm)*i/10, context.timeState + (changeDuration*i/10))
    }
    
    //  No add the music itself
    val durationTiming = music.add(context.copy(tempoBPM=bpm))
    
    context.writeTempo(saveBPM, context.timeState+durationTiming)
    durationTiming
  }
  
  def duration(context: SequenceContext) = music.duration(context.copy(tempoBPM=context.tempoBPM))
}

//-------------------------

//  Add the music, with a changed current time signature

case class WithTimeSig( number: Byte, beat: Beat, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    //  The use of settingTimeSigChange causes the current timeState to be identified as the time at 
    //  which the time signature changed to allow determination of bar boundaries,
    //  which is necessary for bar line validation, and interpretation of Dynamics 
    val preTimeState = context.timeState
    val saveTimeSig=context.timeSig
    val newTimeSig = TimeSig(number, beat)
    context.writeTimeSig(number, beat, context.timeState)
    val durationTiming = music.add(context.copy( timeSig=newTimeSig, timeState=context.timeState.settingTimeSigChange(saveTimeSig)))
    val checkedDuration = 
      if (!(context.timeState).isAtBar(context.timeSig))
      {
        durationTiming.error(s"TimeSig change is not on a bar boundary")
      } 
      else if (!(durationTiming).isAtBar(newTimeSig))
      {
        durationTiming.error(s"TimeSig change is not on a bar boundary")
      }
      else durationTiming
    context.writeTimeSig(saveTimeSig.number, saveTimeSig.beat, context.timeState+durationTiming)
    checkedDuration.settingTimeSigChange(newTimeSig)
  }
  
  def duration(context: SequenceContext) = 
  {
    val newTimeSig = TimeSig(number, beat)
    music.duration(context.copy(timeSig=newTimeSig, timeState=context.timeState.settingTimeSigChange(newTimeSig)))
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
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with a changed current transposition

case class WithTranspose(num: Int, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(transpose = context.transpose + num))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with a changed current transposition

case class WithDiatonic(fromChord: Chord, toChord: Chord, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(dTrans = Some((fromChord, toChord))))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with a changed key signature

case class WithKeySig(keySig: KeySig, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val saveKeySig=context.keySig
    context.writeKeySig(keySig.keySigSharps, keySig.isMinor, context.timeState)
    val durationTiming = music.add(context.copy(keySig = keySig, tonic=keySig.tonic, isMinor=keySig.isMinor))
    context.writeKeySig(saveKeySig.keySigSharps, saveKeySig.isMinor, context.timeState+durationTiming)
    durationTiming
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with a changed tonic, but without a changed key signature

case class WithModulation(keySig: KeySig, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(tonic=keySig.tonic, isMinor=keySig.isMinor))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with a changed current note volume

case class WithVolume(volume: Int, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(volume=volume))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with a changing note volume across the duration of the music

case class WithVolumeChange(volumeInc: Int, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val dur = duration(context)
    WithDynamics(Dynamics.volume(new Beat(dur.ticks), volumeInc),music).add(context)
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with a changed current track name, which may or may not already exist in the tune

case class WithTrack( trackName: String, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(currentTrackName = trackName))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with a changed current channel

case class WithChannel( channelName: String, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(currentChannelName = channelName))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
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
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.PROGRAM_CHANGE, channel, instrument-1, 0), context.timeState.ticks))
    val durationTiming = music.add(context.copy(currentInstrument = instrument))
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.PROGRAM_CHANGE, channel, prevInstrument-1, 0), context.timeState.ticks+durationTiming.ticks))
    durationTiming
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with the dynamics applied 

case class WithDynamics( dyn: Dynamics, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(dynamics = ContextDynamics(context.timeState, dyn) :: context.dynamics))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, which will be a single Note, with the lyric associated with the time of the note 

case class WithLyric( lyric: String, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val bytearray = lyric.getBytes

    val track = context.getTrack
    track.add(new M.MidiEvent(new M.MetaMessage(0x05, bytearray, bytearray.length),context.timeState.ticks))    
    music.add(context.copy())
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with the duration of each Note, Drum or Rest taken in turn cyclically from  
//  repetitions of the rhythm pattern of the rhythmMusic and not from the current Beat

case class WithRhythm( rhythmMusic: Music, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val rhythmPattern=rhythmMusic.rhythmTimings(context.copy(rhythmPattern = Vector.empty))
    music.add(context.copy(rhythmPattern=rhythmPattern))
  }
  
  def duration(context: SequenceContext) = 
  {
    val rhythmPattern=rhythmMusic.rhythmTimings(context.copy(rhythmPattern = Vector.empty))
    music.duration(context.copy(rhythmPattern=rhythmPattern))
  }
}

//-------------------------

//  Add the music, constraining all notes to the specified range of Notes

case class WithRange( rangeLow: Note, rangeHigh: Note, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(rangeLow=rangeLow,rangeHigh=rangeHigh))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}



