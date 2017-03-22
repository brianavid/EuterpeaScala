package com.brianavid.euterpea

//  The TimeState class is a representation of the state of a point of time of Music
//  This state includes the timing (as ticks), the number of notes played (for rhythms) and the time that 
//  the time signature last ctanged (for bar line validation)
case class TimeState(val ticks: Int, val noteCount: Int, val timeSigChangeTime: Option[Int])
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

  //  The timing within a the last of a sequence of fixed-sized chunks
  def % (chunk: TimeState) = new TimeState( ticks % chunk.ticks, 0, None) //  Within a sequence of repeated chunks

  //  The later of two timings
  def max(t: TimeState) = if (t.ticks > ticks) t else this
  
  //  A Timer which has the current time as the timeSigChangeTime change, indicating when the time signature last changed
  def settingTimeSigChange = new TimeState(ticks, noteCount, Some(ticks))
  
  //  How long  since the time signature last changed 
  def timeSinceLastTimeSigChange = ticks - timeSigChangeTime.getOrElse(0)
  
  //  How many beats (of the specified time signature duration) since the time signature last changed
  def beatsSinceLastTimeSigChange(timeSig: TimeSig) = 
    timeSinceLastTimeSigChange / timeSig.duration.beatTicks
  
  //  Is the time precisely at a beat of the specified time signature? 
  def isAtBeat(timeSig: TimeSig) = 
    (timeSinceLastTimeSigChange % timeSig.duration.beatTicks) == 0
  
  //  Is the time precisely at the start of a a bar of the specified time signature? 
  def isAtBar(timeSig: TimeSig) = 
    (timeSinceLastTimeSigChange % (timeSig.number * timeSig.duration.beatTicks)) == 0
}

object TimeState
{
  //  Construct the timing object for the number of beats
  def apply(duration: Beat, noteCount: Int) = new TimeState( duration.beatTicks, noteCount, None)
}
