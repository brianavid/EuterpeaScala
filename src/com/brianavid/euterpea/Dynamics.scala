package com.brianavid.euterpea

//  Dynamics can specify cyclic patterns which can alter volume (i.e. a pulse), 
//  timing (e.g. for swing or humanization) or phrasing (combining volume, timing and note width)

//  Class PointDynamics represents the changes to a Note at any point in time, used either when
//  defining Dynamics patterns or when finding the set of Dynamics adjustments in force at the 
//  time a note is added to the sequence
case class PointDynamics (
    val volumeInc: Int = 0,          //  The amount by which the volume is increased
    val noteWidthInc: Double = 0.0,  //  The amount by which the note width is increased
    val timingInc: Double = 0.0,     //  The proportion of the current beat length by which the 
                                     //  start of the Note is delayed 
    val timingJitter: Double = 0.0)  //  The limit of an additional random proportion of the current 
                                     //  beat length by which the start of the Note is delayed
{
  //  PointDynamics (e.g. from different Dynamics Modifiers) can and will be added together
  def + (that: PointDynamics) = PointDynamics(
      this.volumeInc + that.volumeInc,
      this.noteWidthInc + that.noteWidthInc,
      this.timingInc + that.timingInc,
      this.timingJitter + that.timingJitter)
  
  //  When adding a Note at at time between two defined values, the resultant PointDynamics is interpolated
  def interpolate( that: PointDynamics, alpha: Double) = PointDynamics(
      (this.volumeInc*(1.0-alpha) + that.volumeInc*alpha).toInt,
      this.noteWidthInc*(1.0-alpha) + that.noteWidthInc*alpha,
      this.timingInc*(1.0-alpha) + that.timingInc*alpha,
      this.timingJitter*(1.0-alpha) + that.timingJitter*alpha)
}

//  Class TimedDynamics, defines a cyclic sequence of PointDynamics, each defining the Beat 
//  after the previous one and the new PointDynamics values to which the Dynamics change
case class TimedDynamics(
    val beat: Beat,
    val values: PointDynamics)

//  Object X denotes a single TimedDynamics point in the definition of the cyclic pattern of 
//  values when constructing the Dynamics object
object X
{
  def apply(
    beat: Beat,
    volumeInc: Int = 0,
    noteWidthInc: Double = 0.0,
    timingInc: Double = 0.0,
    timingJitter: Double = 0.0) = 
      TimedDynamics(beat, PointDynamics(volumeInc, noteWidthInc, timingInc, timingJitter))
}

//  Class Dynamics defines a cyclic pattern of PointDynamics values, 
//  with the time between the points specified as Beats
case class Dynamics(val x: TimedDynamics*) extends Modifier 
{
  //  Get the computed PointDynamics value for a time within the repeated application of the TimedDynamics sequence
  def getAtTime(time: Timing) = 
  {
    //  Recursive helper function to get the PointDynamics at any point in the cyclic pattern
    def getValues(ticks: Int, x: Seq[TimedDynamics]): PointDynamics = 
    {
      //  At the start of the pattern, the value is that at the head
      if (ticks == 0)
        x.head.values
      else 
      {
        //  If we are not at the start, there must be at least two values to interpolate
        assert(!x.tail.isEmpty)
        
        //  If the time we are looking for (ticks) is within the current beat, 
        //  then interpolate the time within the two TimedDynamics points
        if (ticks < x.tail.head.beat.beatTicks)
          x.head.values.interpolate(x.tail.head.values, ticks.toDouble / x.tail.head.beat.beatTicks.toDouble)
        else
          //  Otherwise recurse onto the next point in the pattern, having removed the Beat's duration  
          getValues(ticks - x.tail.head.beat.beatTicks, x.tail)
      }  
    }
    
    //  The duration of a single cyclic pattern
    def duration = x.map(_.beat).foldLeft(NoDuration:Beat)(_+_)
  
    //  If the defined pattern's first TimedDynamics does not start at zero time, a zero PointDynamics value is pre-pended 
    val x0 = if (x.isEmpty || x.head.beat.beatTicks != 0) TimedDynamics(NoDuration, PointDynamics()) +: x else x
  
    //  Use the recursive helper function to get the PointDynamics for a time within a single cyclic pattern
    //  But of the patern duration is zero, then the head values are the only ones and will be applied unconditionally
    if (duration.beatTicks == 0)
      x0.head.values
    else
      getValues(time.ticks % duration.beatTicks, x0)
  }
}

//  The Dynamics object defines a number of generally useful cuclic Dynamics patterns
object Dynamics
{
  //  The pulse pattern adds the strength value to the volume of any Note 
  //  defined to be added precisely on the specified Beat
  def pulse(beat: Beat, strength: Int) = 
    Dynamics(X(NoDuration,volumeInc=strength), X(NoDuration,volumeInc=0), X(beat))
  
  //  The swing pattern delays the start of every other Beat by the specified delayFactor as a
  //  multiple of the current Beat Modifier (which may or may not be the same Beat value as here)
  def swing(beat: Beat, delayFactor: Double) = 
    Dynamics(X(beat,timingInc=delayFactor), X(beat))
  
  //  The delay pattern delays all notes by a fixed proportion of a specified beat
  def delay(beat: Beat, delayFactor: Double) = 
    Dynamics(X(NoDuration,timingInc=delayFactor))
}

//  The ContextDynamics objects are used as a List in the SequenceContext to denote currently applied 
//  Dynamics cyclic patterns, starting at a specified time
case class ContextDynamics(
    val startTime: Timing,    //  The time at which the Dynamics pattern starts
    val dynamics: Dynamics)   //  The Dynamics to apply to every Note added

