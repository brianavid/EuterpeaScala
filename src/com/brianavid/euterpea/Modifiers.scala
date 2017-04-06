package com.brianavid.euterpea
import javax.sound.{midi => M}

//  A miscellaneous set of simple Modifier definitions

//-------------------------------------------------------------------------------------------------

//  The Transpose Modifier transposes all notes in its music either :
//    chromatically up and down a number of semitones, or else
//    diatonically from one chord position to another within the current tonic scale

case class Transpose(chromatic: Int, diatonic: Option[(Chord, Chord)]) extends Modifier
{
  def modifying(music: Music): Music =
    diatonic match
    {
    case None => WithTranspose(chromatic, music)
    case Some((fromChord, toChord)) => new WithDiatonic( fromChord, toChord, music)
    }
}

object Transpose {
  def apply(chromatic: Int) = new Transpose(chromatic, None)
  def apply(chords: Tuple2[Chord, Chord]) = new Transpose(0, Some((chords._1, chords._2)))
}

//  The Ocatve Modifier transposes all notes in its music up and down a number of octaves

case class Octave(num: Int) extends Modifier
{
  def modifying(music: Music): Music =
    new WithTranspose(num*12, music)
}

//  Add the music, with a changed current transposition

private[euterpea] case class WithTranspose(num: Int, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(transpose = context.transpose + num))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//  Add the music, with a changed current transposition

private[euterpea] case class WithDiatonic(fromChord: Chord, toChord: Chord, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(dTrans = Some((fromChord, toChord))))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  The Track Modifier specifies that its music goes in the named track which can be viewed, printed or muted
//  independently of any music on any other named track

case class Track(trackName: String) extends Modifier
{
  def modifying(music: Music): Music =
    new WithTrack(trackName, music)
}

//  Add the music, with a changed current track name, which may or may not already exist in the tune

private[euterpea] case class WithTrack( trackName: String, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(currentTrackName = trackName))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  The Channel Modifier specifies that its music goes in the named Midi channel and so is distinct
//  (e.g. has a different instrument) from any music on any other named channel.
//  The channel names are only used locally and are mapped into integer Midi channel numbers 

case class Channel(channelName: String) extends Modifier
{
  def modifying(music: Music): Music =
    new WithChannel(channelName, music)
}

//  Add the music, with a changed current channel

private[euterpea] case class WithChannel( channelName: String, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(currentChannelName = channelName))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  The Instrument Modifier specifies the General Midi instrument on which the music should sound
//  It can be specified as an integer value (with constants defined in the Instruments object)
//  or as an instrument name string

case class Instrument(instrument: Int) extends Modifier
{
  def modifying(music: Music): Music =
    new WithInstrument(instrument, music)
}

object Instrument {
  def apply(instrumentName: String) = new Instrument(Instruments.instrumentByName.getOrElse(instrumentName, 1))
}

//  Add the music, with a changed current instrument number (from the General Midi set of instruments

private[euterpea] case class WithInstrument( instrument: Int, music: Music) extends Music
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

//  The Rhythm Modifier adds the modified music with the rhythm of its pattern (repeated as necessary)

case class Rhythm(pattern: Music) extends Modifier
{
  def modifying(music: Music): Music =
    new WithRhythm(pattern, music)
}

//  Add the music, with the duration of each Note, Drum or Rest taken in turn cyclically from  
//  repetitions of the rhythm pattern of the rhythmMusic and not from the current Beat

private[euterpea] case class WithRhythm( rhythmMusic: Music, music: Music) extends Music
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

//  The Range class specifies a low and high Note to which all notes will be constrained by changing octaves

case class Range(low: Note, high: Note) extends Modifier
{
  def modifying(music: Music): Music =
    new WithRange(low, high, music)
}

//  Add the music, constraining all notes to the specified range of Notes

private[euterpea] case class WithRange( rangeLow: Note, rangeHigh: Note, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(rangeLow=rangeLow,rangeHigh=rangeHigh))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  The Lyric class specifies a lyric in the current track at the current position

case class Lyric(lyric: String) extends Modifier
{
  def modifying(music: Music): Music =
    new WithLyric(lyric, music)
}

//  Add the music, which will be a single Note, with the lyric associated with the time of the note 

private[euterpea] case class WithLyric( lyric: String, music: Music) extends Music
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

//  FUTURE MODIFIER CLASS DEFINITIONS GO HERE


//-------------------------



