package com.brianavid.euterpea
import language.implicitConversions

//  The Beat Modifier denotes the beat of a Note or Rest.
//  At its basic, it is a Whole, Half, Quarter, Eighth ... note
//  Beats can be added together, dotted, or lengthened by repetition

class Beat(val beatTicks: Int) extends Modifier with FretsModifier
{
  def modifying(music: Music): Music =
    new WithBeat (this, new WithBeatScale (1, 1, music))
  def modifyingFrets(fs: FretSequence): FretSequence =
    new FretSequenceWithBeat (this, new FretSequenceWithBeatScaling (1, 1, fs))
  
  def dot = new Beat(beatTicks * 3 / 2) //  Note and a half
  def +(next: Beat) = new Beat(beatTicks + next.beatTicks) //  Simply added together
  def -(next: Beat) = new Beat(beatTicks - next.beatTicks) //  Simply subtracted
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

case class BeatScale(val numberOfNotes: Integer, val numberOfBeats: Integer) extends Modifier with FretsModifier
{
  def modifying(music: Music): Music =
    new WithBeatScale (numberOfNotes, numberOfBeats, music)
  def modifyingFrets(fs: FretSequence): FretSequence =
    new FretSequenceWithBeatScaling (numberOfNotes, numberOfBeats, fs)
}
object Dot extends BeatScale(2,3)
object DotDot extends BeatScale(4,7)

//-------------------------

//  Add the music, with a changed current duration

private[euterpea] case class WithBeat(beat: Beat, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(beat=beat))
  }
  
  def duration(context: SequenceContext) = music.duration(context.copy(beat=beat))
}

//-------------------------

//  Add the music, scaling the beat for (e.g.) triplets or quintuplets

private[euterpea] case class WithBeatScale(numberOfNotes: Integer, numberOfBeats: Integer, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(scaleNum=numberOfNotes,scaleBeats=numberOfBeats))
  }
  
  def duration(context: SequenceContext) = music.duration(context.copy(scaleNum=numberOfNotes,scaleBeats=numberOfBeats))
}
