package com.brianavid.euterpea

//  The TimeState class is a representation of the state of a point of time of Music,
//  encompassing those aspects which move forward with time through the sequence,
//  independently of the nested structure by which it was constructed.
//  This state includes:
//    -  the time position within the sequence (as ticks)
//    -  the number of notes played (used for cyclic rhythm patterns)
//    -  the time that the time signature last changed (for bar line validation)
case class TimeState(
    val ticks: Int, 
    val noteCount: Int, 
    val timeSigChangeTime: Option[Int])
{
  //  Timings can be added
  def +(t: TimeState) = 
    { 
      // The timeSigChangeTime may come from this TimeState object or the later one
      val newTimeSigChangeTime = t.timeSigChangeTime match {
        case None => timeSigChangeTime
        case Some(t) => Some(ticks+t)
      }
      new TimeState(ticks + t.ticks, noteCount+t.noteCount, newTimeSigChangeTime)
    }
  
  //  Subtracting has no effect on timeSigChangeTime - used for tied notes
  def -(t: TimeState) = new TimeState(ticks - t.ticks, noteCount-t.noteCount, timeSigChangeTime)
  
  //  The timing of a number of notes
  def * (number: Integer) = new TimeState( ticks * number, noteCount * number, None) //  Within a sequence of repeated chunks

  //  The timing of one of a number of notes in a time interval
  def / (number: Integer) = new TimeState( ticks / number, 1, None) //  Within a sequence of repeated chunks

  //  The timing within a the last of a sequence of fixed-sized chunks - used for Dynamics
  def % (chunk: TimeState) = new TimeState( ticks % chunk.ticks, 0, None) //  Within a sequence of repeated chunks

  //  The later of two TimeStates
  def max(t: TimeState) = if (t.ticks > ticks) t else this
  
  //  A copy of the TimeState which additionally has the current time as the timeSigChangeTime change, 
  //  indicating when the time signature last changed
  def settingTimeSigChange = copy(timeSigChangeTime = Some(ticks))
  
  //  How long since the time signature last changed 
  def timeSinceLastTimeSigChange = ticks - timeSigChangeTime.getOrElse(0)
  
  //  How many beats (of the specified time signature duration) since the time signature last changed
  def beatsSinceLastTimeSigChange(timeSig: TimeSig) = 
    timeSinceLastTimeSigChange / timeSig.beat.beatTicks
  
  //  Is the time precisely at a beat of the specified time signature? 
  def isAtBeat(timeSig: TimeSig) = 
    (timeSinceLastTimeSigChange % timeSig.beat.beatTicks) == 0
  
  //  Is the time precisely at the start of a a bar of the specified time signature? 
  def isAtBar(timeSig: TimeSig) = 
    (timeSinceLastTimeSigChange % (timeSig.number * timeSig.beat.beatTicks)) == 0
}

object TimeState
{
  //  Construct the TimeState object for the beat and number of notes
  def apply(beat: Beat, noteCount: Int) = new TimeState( beat.beatTicks, noteCount, None)
}
