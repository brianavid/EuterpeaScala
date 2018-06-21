package com.brianavid.euterpea
import javax.sound.{midi => M}

private[euterpea] case class NoteOnString(
    track: M.Track,
    channel: Int,
    pitch: Int)

//  TimeStates contain a list of Errors accumulated during the processing of the Music up to that point
private[euterpea] case class Error(position: TimeState, message: String)

//  The TimeState class is a representation of the state of a point of time of Music,
//  encompassing those aspects which move forward with time through the sequence,
//  independently of the nested structure by which it was constructed.
//  This state includes:
//    -  the time position within the sequence (as ticks)
//    -  the number of notes played (used for cyclic rhythm patterns)
//    -  the time that the time signature last changed (for bar line validation)
//    -  the list of errors accumulated up the that point in the music
//    -  the mapping of Guitar strings to the notes still sounding on that string
private[euterpea] case class TimeState(
    ticks: Int, 
    noteCount: Int , 
    currentTimeSig: TimeSig,
    timeSigChangeTime: Option[Int],
    controls: ControlValues,
    barsAtTimeSigChange: Option[Int],
    errors: List[Error],
    playingStrings: Map[Guitar.String, Option[NoteOnString]] = Map.empty)
{
  //  Timings can be added
  def +(t: TimeState): TimeState = 
    { 
      // The timeSigChangeTime may come from this TimeState object or the later one
      val newTimeSigChangeTime = t.timeSigChangeTime match {
        case None => timeSigChangeTime
        case Some(timeSigChangeTimeTicks) => Some(ticks+timeSigChangeTimeTicks)
      }
      // The barsAtTimeSigChange may come from this TimeState object or the later one
      val newBarsAtTimeSigChange = t.barsAtTimeSigChange match {
        case None => barsAtTimeSigChange
        case Some(timeSigChangeTimeBars) => 
          {
            Some(barsCount(currentTimeSig)+timeSigChangeTimeBars)
          }
      }
      
      //  Errors in t have their positions offset (recursively) by the current timeState duration
      //  And the errors of the new timeState are the union of those of the two added timeState
      val combinedErrors = errors ++ t.errors.map(e => e.copy(position=e.position+copy(errors=Nil)))
      
      val newTimeState = new TimeState(
          ticks + t.ticks, 
          noteCount+t.noteCount, 
          if (t.currentTimeSig.number == 0) currentTimeSig else t.currentTimeSig,
          newTimeSigChangeTime, 
          controls.merge(t.controls), 
          newBarsAtTimeSigChange, 
          combinedErrors,
          playingStrings ++ t.playingStrings)  //  Later string maps over-write
      newTimeState
    }
  
  //  Subtracting has no effect on timeSigChangeTime - used for tied notes
  def -(t: TimeState) = copy(ticks=ticks - t.ticks)
  
  //  The timing of a number of notes
  def * (number: Integer) = copy(ticks=ticks*number) //  Within a sequence of repeated chunks

  //  The timing of one of a number of notes in a time interval
  def / (number: Integer) = copy( ticks=ticks/number) //  Within a sequence of repeated chunks

  //  The timing within a the last of a sequence of fixed-sized chunks - used for Dynamics
  def % (chunk: TimeState) = copy( ticks=ticks%chunk.ticks, noteCount=0) //  Within a sequence of repeated chunks

  //  The later of two TimeStates, merging control and errors
  def max(t: TimeState) = 
    {
      val later = if (t.ticks > ticks) t else this
      val merged = later.copy(controls=controls.merge(t.controls), errors=(errors ++ t.errors), playingStrings=(playingStrings ++ t.playingStrings))
      if (t.ticks == ticks) merged
      else merged.error(s"Parallel music has different lengths: ${t.ticks} != $ticks")
    }
  
  //  A copy of the TimeState which additionally has the current time as the timeSigChangeTime change, 
  //  indicating when the time signature last changed
  def settingTimeSigChange(timeSig: TimeSig) = copy(currentTimeSig=timeSig, timeSigChangeTime=Some(ticks), barsAtTimeSigChange=Some(barsCount(timeSig)))
  
  //  Stop playing any previous Note(s) currently sounding on the specified (Guitar) String 
  def stopString(string: Guitar.String, stopTicks: Option[Long] = None): Unit =
  {
    def stopOneNoteOnString(noteOnString: Option[NoteOnString]): Unit =
      noteOnString match
      {
        case None => ()
        case Some(n) => 
        {
          n.track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.NOTE_OFF, n.channel, n.pitch, 0), stopTicks.getOrElse(ticks)))
        }
      }
    
    if (playingStrings.contains(string))
      stopOneNoteOnString(playingStrings(string))
  }
  
  //  Stop playing all previous Notes currently sounding on any (Guitar) String 
  def stopAllStrings(): Unit =
  {
    for (s <- playingStrings.keys) stopString(s)
  }
  
  //  A copy of the TimeState which additionally adds an error message at the current position
  def error(message: String) = 
  {
    copy(errors=Error(this, message) :: errors)
  }
  
  //  How long since the time signature last changed 
  def timeSinceLastTimeSigChange = ticks - timeSigChangeTime.getOrElse(0)
  
  //  How many beats (of the specified time signature duration) since the time signature last changed
  def beatsSinceLastTimeSigChange(timeSig: TimeSig) = 
    timeSinceLastTimeSigChange / timeSig.beat.beatTicks
  
  //  How many bars (of the specified time signature duration) since the time signature last changed
  def barsSinceLastTimeSigChange(timeSig: TimeSig) = 
    if (timeSig.number == 0) 0 else timeSinceLastTimeSigChange / (timeSig.number * timeSig.beat.beatTicks)
  
  //  How many bars are at in the music up to the current position
  def barsCount(timeSig: TimeSig) = 
    barsAtTimeSigChange.getOrElse(0) + barsSinceLastTimeSigChange(timeSig)
  
  //  Is the time precisely at a beat of the specified time signature? 
  def isAtBeat(timeSig: TimeSig) = 
    (timeSinceLastTimeSigChange % timeSig.beat.beatTicks) == 0
  
  //  Is the time precisely at the start of a a bar of the specified time signature? 
  def isAtBar(timeSig: TimeSig) = 
    (timeSinceLastTimeSigChange % (timeSig.number * timeSig.beat.beatTicks)) == 0
    
  def display = 
  {
    val bars = barsCount(currentTimeSig) + 1
    val ticksInbar = timeSinceLastTimeSigChange % (currentTimeSig.number * currentTimeSig.beat.beatTicks)
    val beatInBar = ticksInbar / currentTimeSig.beat.beatTicks + 1
    val ticksInBeat = timeSinceLastTimeSigChange % currentTimeSig.beat.beatTicks
    if (ticksInBeat == 0)
      s"$bars:$beatInBar/${currentTimeSig.number}"
    else
    {
      def gcd(a: Int,b: Int): Int = if(b ==0) a else gcd(b, a%b)
      val g = gcd(ticksInBeat,currentTimeSig.beat.beatTicks)
      s"$bars:$beatInBar/${currentTimeSig.number}+${ticksInBeat/g}/${currentTimeSig.beat.beatTicks/g}"
    }
  }
}

private[euterpea] object TimeState
{
  //  Construct the TimeState object for the beat and number of notes
  def apply(beat: Beat) = new TimeState( beat.beatTicks, 0, TimeSig(0,NoDuration), None, ControlValues.empty, None, Nil)
  def apply(beat: Beat, noteCount: Int, timeSig: TimeSig) = new TimeState( beat.beatTicks, noteCount, timeSig, None, ControlValues.empty, None, Nil)
  def empty(timeSig: TimeSig) = new TimeState(0, 0, timeSig, None, ControlValues.empty, None, Nil)
}
