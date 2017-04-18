package com.brianavid.euterpea

case class RhythmBeat(preRest: Beat, duration: Beat, postRest: Beat)

//  The Rhythm Modifier adds the modified music with the rhythm of its pattern (repeated as necessary)

case class Rhythm(pattern: Music) extends Modifier
{
  def modifying(music: Music): Music =
    new WithRhythm(pattern, music)
}

object Rhythm
{
  def rhythmTimings(music: Music, context: SequenceContext) =
  {
    def getTimings(m: Music, context: SequenceContext) : List[RhythmBeat] =
    {
      def joinTimings(t1: List[RhythmBeat], t2: List[RhythmBeat]) =
      {
        if (t1.map(_.duration.beatTicks).sum == 0)
        {
          val t2h = t2.head
          val t1Pre = t1.map(_.preRest).reduce(_+_)
          val t1Post = t1.map(_.postRest).reduce(_+_)
          RhythmBeat(t1Pre+t1Post+t2h.preRest, t2h.duration, t2h.postRest) :: t2.tail
        }
        else if (t2.map(_.duration.beatTicks).sum == 0)
        {
          val t1l = t1.last
          val t2Pre = t2.map(_.preRest).reduce(_+_)
          val t2Post = t2.map(_.postRest).reduce(_+_)
          t1.init ::: List(RhythmBeat(t1l.preRest, t1l.duration, t1l.postRest+t2Pre+t2Post))
        }
        else
          t1 ::: t2
      }
      
      //  The only things that affect rhythm are the sequence of Notes and bars and the 
      //  Beat and BeatScale modifiers
      //  Normally the rhythm will be a simple single note melody or drum pattern, which may
      //  use the (unplayable) Note "?" for clarity
      
      m match
      {
        case m1 - m2 => joinTimings(getTimings(m1,context), getTimings(m2,context))
        case BarJoin(m1,m2) => joinTimings(getTimings(m1,context), getTimings(m2,context))
        case Repeated(m1,repeat) => (1 to repeat).map(i => getTimings(m1,context)).flatten.toList
        case WithBeat(beat, m1) => getTimings(m1,context.copy(beat=beat))
        case WithBeatScale(numberOfNotes, numberOfBeats, m1) => getTimings(m1,context.copy(scaleNum=numberOfNotes,scaleBeats=numberOfBeats))
        case Rest =>  List(RhythmBeat(NoDuration, NoDuration, new Beat(m.duration(context).ticks)))
        case _ => List(RhythmBeat(NoDuration, new Beat(m.duration(context).ticks), NoDuration))
      }
    }
    getTimings(music, context).toVector
  }

}

//  Add the music, with the duration of each Note, Drum or Rest taken in turn cyclically from  
//  repetitions of the rhythm pattern of the rhythmMusic and not from the current Beat

private[euterpea] case class WithRhythm( rhythmMusic: Music, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val rhythmPattern=Rhythm.rhythmTimings(rhythmMusic, context.copy(rhythmPattern = Vector.empty))
    music.add(context.copy(rhythmPattern=rhythmPattern, rhythmStartNotes=context.timeState.noteCount))
  }
  
  def duration(context: SequenceContext) = 
  {
    val rhythmPattern=Rhythm.rhythmTimings(rhythmMusic, context.copy(rhythmPattern = Vector.empty))
    music.duration(context.copy(rhythmPattern=rhythmPattern, rhythmStartNotes=context.timeState.noteCount))
  }
}



