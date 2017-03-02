package com.brianavid.euterpea

//  The Timing class is a representation of the duration and position of music
case class Timing(val ticks: Int, val timeSigChangeTime: Option[Int])
{
  //  Timings can be added
  def +(t: Timing) = 
    { 
      // The timeSigChangeTime may come from this Timing object or the later one
      val newTimeSigChangeTime = t.timeSigChangeTime match {
        case None => timeSigChangeTime
        case Some(t) => Some(ticks+t)
      }
      new Timing(ticks + t.ticks, newTimeSigChangeTime)
    }
  
  //  Subtracting has no effect on timeSigChangeTime - used for tied notes
  def -(t: Timing) = new Timing(ticks - t.ticks, timeSigChangeTime)
  
  //  The later of two timings
  def max(t: Timing) = if (t.ticks > ticks) t else this
  
  //  A Timer which has the current time as the timeSigChangeTime change, indicating when the time signature last changed
  def settingTimeSigChange = new Timing(ticks, Some(ticks))
  
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

object Timing
{
  //  Construct the timing object for the number of beats
  def apply(duration: Duration) = new Timing( duration.beatTicks, None)
}
