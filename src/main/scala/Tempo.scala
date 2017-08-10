package com.brianavid.euterpea

//  The Tempo Modifier controls and varies the tempo of the music

case class Tempo(bpm: Int, toBpm: Option[Int] = None) extends Modifier
{
  def modifying(music: Music): Music =
    new WithTempo( bpm, toBpm, music)
}
object Tempo {
  def apply(bpm: Int, toBpm: Int) = new Tempo(bpm, Some(toBpm))
  def apply(tempos: Tuple2[Int,Int]) = new Tempo(tempos._1, Some(tempos._2))
}

//  The TimeSig Modifier controls and varies the time signature of the music

case class TimeSig(number: Byte, beat: Beat) extends Modifier
{
  def modifying(music: Music): Music =
    new WithTimeSig( number, beat, music)
}

//  The Width Modifier controls the proportion of the time that a note actually 
//  sounds within its duration 

case class Width(noteWidth: Double) extends Modifier
{
  def modifying(music: Music): Music =
    new WithWidth( noteWidth, music)
}
object DefaultWidth extends Width(0.9)
object Staccato extends Width(0.3) //  Short and sharp
object Marcato extends Width(0.7) //  Notes clearly separated  
object Legato extends Width(1.0) //  Notes flowing together

//-------------------------

//  Add the music, with a changed (and optionally changing) current tempo

private[euterpea] case class WithTempo( bpm: Int, toBpm: Option[Int], music: Music) extends Music
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

private[euterpea] case class WithTimeSig( number: Byte, beat: Beat, music: Music) extends Music
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

private[euterpea] case class WithWidth( noteWidth: Double, music: Music) extends Music 
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(noteWidth=noteWidth))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

