package com.brianavid.euterpea
import language.implicitConversions

trait Modifier

object Modifier {
  //  A Beat or BeatScale Modifier can be created where a Modifier is expected simply by using an integer value
  //  That integer can be 3 or 5 to specify a BeatScale of triplets or quintuplets
  //  Otherwise the Beat object interprets the value as a powet of two 
  implicit def beatModifierFromInteger(divisor: Int): Modifier =
    {
      if (divisor == 3)
        new BeatScale(3, 2) // Triplet
      else if (divisor == 5)
        new BeatScale(5, 4) // Quintuplet
      else
        Beat.beatFromInteger(divisor)
    }
}

//------------------------------------------------------------------------------------------------------------

//  The Beat Modifier denotes the beat of a Note or Rest.
//  At its basic, it is a Whole, Half, Quarter, Eighth ... note
//  Beats can be added together, dotted, or lengthened by repetition

class Beat(val beatTicks: Int) extends Modifier {
  def dot = new Beat(beatTicks * 3 / 2) //  Note and a half
  def +(next: Beat) = new Beat(beatTicks + next.beatTicks) //  Simply added together
  def *(repeat: Integer) = new Beat(beatTicks * repeat) //  Lengthened

  //  The Midi meta encoding of the (simple object) Duration for time signatures (only)
  def timeSigDenom = {
    def powerOfTwo(v: Integer, power: Integer): Integer =
      if (v < 2) power else powerOfTwo(v / 2, power + 1)
    powerOfTwo(Beat.TPQN * 4 / beatTicks, 0).toByte
  }
}

object Beat {
  val TPQN = 480 //  Ticks per Quarter Note - The granularity of note length sub-divisions

  //  A Beat Modifier can be created where a Beat is expected simply by using an integer value
  //  The value must be a power of two (4 = Quarter, 16 = Sixteenth, etc) 
  implicit def beatFromInteger(divisor: Int): Beat =
    {
      def isPowerOfTwo = (divisor & (divisor - 1)) == 0
      if (!isPowerOfTwo) {
        NoDuration
      } else
        new Beat(Beat.TPQN * 4 / divisor)
    }
}

case object NoDuration extends Beat(0) //  No time
case object Whole extends Beat(Beat.TPQN * 4) //  Whole note
case object Half extends Beat(Beat.TPQN * 2) //  Half note
case object Quarter extends Beat(Beat.TPQN) //  Quarter note
case object Eighth extends Beat(Beat.TPQN / 2) //  Eighth note
case object Sixteenth extends Beat(Beat.TPQN / 4) //  Sixteenth note
case object Thirtysecond extends Beat(Beat.TPQN / 8) //  Thirty-second note
case class Dot(beat: Beat) extends Beat(beat.beatTicks * 3 / 2) //  Note and a half

//-------------------------

//  The BeatScale Modifier scales the beat to allow e.g. triplets and quintuplets  

case class BeatScale(val numberOfNotes: Integer, val numberOfBeats: Integer) extends Modifier
object Dot extends BeatScale(2,3)
object DotDot extends BeatScale(4,7)

//-------------------------

//  The Volume Modifier controls the volumes (actually velocities) of notes

case class Volume(val volume: Int) extends Modifier
{
  def -> (toVolume: Volume) = new VolumeChange(this, toVolume)
}
object Volume
{
  val MinVolume = 0
  val MaxVolume = 127
  val DefaultVolume = 68
  val VolumeInc = 15
}
object Vmf extends Volume(Volume.DefaultVolume)                       //  Normal (default) volume
object Vmp extends Volume(Volume.DefaultVolume  - Volume.VolumeInc)   //  A biq quieter than normal volume
object Vf extends Volume(Volume.DefaultVolume + Volume.VolumeInc)     //  Louder (forte)
object Vff extends Volume(Volume.DefaultVolume + Volume.VolumeInc*2)  //  Even louder
object Vfff extends Volume(Volume.DefaultVolume + Volume.VolumeInc*3) //  Even louder still
object Vp extends Volume(Volume.DefaultVolume - Volume.VolumeInc)     //  Quieter (piano)
object Vpp extends Volume(Volume.DefaultVolume - Volume.VolumeInc*2)  //  Even quieter
object Vppp extends Volume(Volume.DefaultVolume - Volume.VolumeInc*3) //  Even quieter still

//  The VolumeChange Modifier increases or decreases Volume gradually over the duration of the music

case class VolumeChange(val fromVolume: Volume, val toVolume: Volume) extends Modifier

//  The Tempo Modifier controls and varies the tempo of the music

case class Tempo(bpm: Int, toBpm: Option[Int] = None) extends Modifier
object Tempo {
  def apply(bpm: Int, toBpm: Int) = new Tempo(bpm, Some(toBpm))
  def apply(tempos: Tuple2[Int,Int]) = new Tempo(tempos._1, Some(tempos._2))
}

//  The TimeSig Modifier controls and varies the time signature of the music

case class TimeSig(number: Byte, beat: Beat) extends Modifier

//  The Width Modifier controls the proportion of the time that a note actually 
//  sounds within its duration 

case class Width(noteWidth: Double) extends Modifier
object DefaultWidth extends Width(0.9)
object Staccato extends Width(0.3) //  Short and sharp
object Marcato extends Width(0.7) //  Notes clearly separated  
object Legato extends Width(1.0) //  Notes flowing together

//  The Transpose Modifier transposes all notes in its music either :
//    chromatically up and down a number of semitones, or else
//    diatonically from one chord position to another within the current tonic scale

case class Transpose(chromatic: Int, diatonic: Option[(Chord, Chord)]) extends Modifier
object Transpose {
  def apply(chromatic: Int) = new Transpose(chromatic, None)
  def apply(chords: Tuple2[Chord, Chord]) = new Transpose(0, Some((chords._1, chords._2)))
}

//  The Ocatve Modifier transposes all notes in its music up and down a number of octaves

case class Octave(num: Int) extends Modifier

//  The KeySig Modifier specified the number of sharps (if +ve) or flats (if -ve) in the key signature

case class KeySig(keySigSharps: Byte, tonic: Note, isMinor: Boolean = false) extends Modifier
object CMaj extends KeySig(0, C)
object GMaj extends KeySig(1, G)
object DMaj extends KeySig(2, D)
object AMaj extends KeySig(3, A)
object EMaj extends KeySig(4, E)
object BMaj extends KeySig(5, B)
object FsMaj extends KeySig(6, Fs)
object CsMaj extends KeySig(7, Cs)
object BfMaj extends KeySig(-1, Bf)
object EfMaj extends KeySig(-2, Ef)
object AfMaj extends KeySig(-3, Af)
object DfMaj extends KeySig(-4, Df)
object GfMaj extends KeySig(-5, Gf)
object CfMaj extends KeySig(-6, Cf)
object FfMaj extends KeySig(-7, Ff)
object AMin extends KeySig(0, A, true)
object EMin extends KeySig(1, E, true)
object BMin extends KeySig(2, B, true)
object FsMin extends KeySig(3, Fs, true)
object CsMin extends KeySig(4, Cs, true)
object DMin extends KeySig(-1, D, true)
object GMin extends KeySig(-2, G, true)
object CMin extends KeySig(-3, C, true)
object BfMin extends KeySig(-4, Bf, true)
object EfMin extends KeySig(-5, Ef, true)
object AfMin extends KeySig(-6, Af, true)
object DfMin extends KeySig(-7, Df, true)

//  The Modulate Modifier changes the current tonic key for harmonisation without altering the the key signature

case class Modulate(tonic: KeySig) extends Modifier

//  The Track Modifier specifies that its music goes in the named track which can be viewed, printed or muted
//  independently of any music on any other named track

case class Track(trackName: String) extends Modifier

//  The Channel Modifier specifies that its music goes in the named Midi channel and so is distinct
//  (e.g. has a different instrument) from any music on any other named channel.
//  The channel names are only used locally and are mapped into integer Midi channel numbers 

case class Channel(channelName: String) extends Modifier

//  The Instrument Modifier specifies the General Midi instrument on which the music should sound
//  It can be specified as an integer value (with constants defined in the Instruments object)
//  or as an instrument name string

case class Instrument(instrument: Int) extends Modifier
object Instrument {
  def apply(instrumentName: String) = new Instrument(Instruments.instrumentByName.getOrElse(instrumentName, 1))
}

//  The Rhythm Modifier adds the modified music with the rhythm of its pattern (repeated as necessary)

case class Rhythm(pattern: Music) extends Modifier

//  FUTURE MODIFIER CLASS DEFINITIONS GO HERE

