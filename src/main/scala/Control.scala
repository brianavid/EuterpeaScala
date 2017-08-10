package com.brianavid.euterpea
import javax.sound.{midi => M}

//  Control values are music objects which have no duration, but which alter the value of a 
//  Midi continuous controller on a channel

//  A ControlPoint is the value of a controller at a specific point of 
//  time (the ticks) when it was last set.
//  The smooth parameter (0.0 .. 1.0) smooths out transitions between control points 
case class ControlPoint(ticks: Int, value: Int, smooth: Double = 0.0)

//  ControlValues are a mapping of a key (derived from the channel and controller number) to the most 
//  recently set ControlPoint
case class ControlValues(values: Map[String, ControlPoint] = Map.empty)
{
  //  The ControlValues updated with a new ControlPoint for the keyed control
  def updated(key: String, point: ControlPoint) = copy(values = values.updated(key, point))
  
  //  Get the most recent ControlPoint (if any) for the keyed control
  def get(key: String) = values.get(key)
  
  //  Is there a ControlPoint for the keyed control
  def contains(key: String) = values.contains(key)
  
  //  Merge two ControlValues, as the union of the two maps, but choosing the later of two ControlValues
  //  for conflicting entries for the same key
  def merge(that: ControlValues): ControlValues =
  {
    val existing = 
      for ((key, value1 @ ControlPoint(ticks1, _, _)) <- values) yield
        that.get(key) match
        {
          case None => key -> value1
          case Some(value2 @ ControlPoint(ticks2, _, _)) =>
            key -> (if (ticks1 > ticks2) value1 else value2)
        }
    val added = 
      for ((key, value2) <- that.values; if !values.contains(key)) yield key -> value2
      
    ControlValues(existing ++ added)
  }
}

//  The ControlValues object defines empty ControlValues map, and the construction of the key 
object ControlValues
{
  def empty = new ControlValues()
  def makeKey(controlId: Int, channel: Int) = s"controlId:$channel"
}

//  A Controller can be used to construct Control objects for given Midi control ID
case class Controller(controlId: Int)
{
  def apply(value: Int, smooth: Double = 0.0) = new Control( controlId, value, smooth)
  def apply(proportion: Double, smooth: Double) = new Control( controlId, (Control.MaxValue * proportion).toInt, smooth)
}

//  Midi controller IDs are defined by the Midi spec
object Controller
{
  val Pitch_Bend_Pseudo_Control = -1
  
  val Bank_Select = 0	
  val Modulation_Wheel = 1	
  val Breath_Controller = 2	
  val Foot_Controller = 4	
  val Portamento_Time = 5	
  val Channel_Volume = 7	
  val Balance = 8	
  val Pan = 10	
  val Expression_Controller = 11	
  val Effect_Control_1 = 12	
  val Effect_Control_2 = 13	
  val General_Purpose_Controller_1 = 16	
  val General_Purpose_Controller_2 = 17	
  val General_Purpose_Controller_3 = 18	
  val General_Purpose_Controller_4 = 19	
  val Sustain_Pedal_On_Off = 64	
  val Portamento_On_Off = 65	
  val Sostenuto_On_Off = 66	
  val Soft_Pedal_On_Off = 67	
  val Legato_Footswitch = 68	
  val Sound_Controller_1_Sound_Variation = 70	
  val Sound_Controller_2_Timbre_Harmonic_Intens = 71	
  val Sound_Controller_3_Release_Time = 72	
  val Sound_Controller_4_Attack_Time = 73	
  val Sound_Controller_5_Brightness = 74	
  val Sound_Controller_6_Decay_Time = 75	
  val Sound_Controller_7_Vibrato_Rate = 76	
  val Sound_Controller_8_Vibrato_Depth = 77	
  val Sound_Controller_9_Vibrato_Delay = 78	
  val Sound_Controller_10 = 79	
  val General_Purpose_Controller_5 = 80	
  val General_Purpose_Controller_6 = 81	
  val General_Purpose_Controller_7 = 82	
  val General_Purpose_Controller_8 = 83	
  val Portamento_Control = 84	
  val Effects_1_Depth_Reverb_Send_Level = 91	
  val Effects_2_Depth_Tremolo_Depth = 92	
  val Effects_3_Depth_Chorus_Send_Level = 93	
  val Effects_4_Depth = 94	
  val Effects_5_Depth = 95	
  
  //  A small range of control IDs are binary - not continuous 
  def isOnOf(controlId: Int) = controlId >= Sustain_Pedal_On_Off && controlId <= Legato_Footswitch
}

//  A PitchBend envelop segment, of the duration of the beat, ending at a 
//  level of proportion (-1.0 .. +1.0), and with a specified smoothness in 
//  transition to adjacent segments
case class P(beat: Beat, proportion: Double, smooth: Double = 0.0)

//  PitchBend is a pseudo-Controller, that generates a different Midi message
//  In addition to the single-point, no-duration Control values, PitchBend also supports
//  specification of the envelope as a sequence of line segments with duration
object PitchBend
{
  val MinValue = -0x1FFF
  val MaxValue = 0x1FFF
  val MaxRange = 0x4000
  
  //  It also has a helper constructor that builds the entire envelope from a Seq of segments
  def apply(initialProportion: Double, points: P*): Music = 
    apply(initialProportion, 0.0, points: _*)
  
  //  It also has a helper constructor that builds the entire envelope from a Seq of segments
  def apply(initialProportion: Double, initialSmooth: Double, points: P*): Music =
  {
    val proportion = initialProportion max -1.0 min 1.0
    val pitchBend = new Controller(Controller.Pitch_Bend_Pseudo_Control)
    val initial: Music = pitchBend((MaxValue * proportion).toInt, initialSmooth)
    
    if (points.length == 0)
      //  With no PitchBend envelope segments, the initial Control value is all there is
      initial
    else
    {
      //  Function to construct the Music for a PitchBend envelope segment
      //  This comprises the pair of a Rest of the sengment's beat duration and the 
      //  control value proportion and smoothness 
      def controlAfterTime(p: P) : Music =
      {
        val proportion = p.proportion max -1.0 min 1.0
        val afterTime: Music = WithBeat(p.beat, Rest)
        val control: Music = pitchBend((MaxValue * proportion).toInt, p.smooth)
        afterTime - control
      }
      
      //  Append a sequence of PitchBend envelope segments to the initial control value
      points.map(controlAfterTime(_)).foldLeft(initial)((a,b) => a - b)
    }
  }
}

//  The Control class is Music, which has no duration. When added, it interpolates values into the sequence 
//  from the most recently set ControlPoint (for the control ID and channel) to the currently set value
//  across the time period from when that previous ControlPoint was set
//  The interpolation is linear when smooth has its default 0.0 value.
//  For a larger smooth value (up to 1.0) a Bezier interpolation is used with a "pull" towards
//  the horizontal, which makes transitions less abrupt
case class Control(controlId: Int, value: Int, smooth: Double = 0.0) extends Music 
{
  def add(context: SequenceContext) =
  {
    //  Get the track identified by the track name, creating it if it does not exist
    val track = context.getTrack
    
    //  Get the Midi channel identified by the track name, creating it if it does not exist
    val channel = context.channels.getOrElseUpdate(context.currentChannelName, context.channels.size)
    
    //  Helper function to add a single Midi event at a time
    //  Except in the ease of the PitchBend pseudo-Controller, the Midi Message is CONTROL_CHANGE for the controlId
    //  For PitchBend, the Midi message PITCH_BEND with a high-resolution parameter 
    def addControllerMessage(value: Int, ticks: Int) =
    {
      if (controlId == Controller.Pitch_Bend_Pseudo_Control)
      {
        val normalizedValue = value + 0x2000
        track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.PITCH_BEND, channel, normalizedValue % 0x80, normalizedValue / 0x80), ticks))
      }
      else
        track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.CONTROL_CHANGE, channel, controlId, value), ticks))
    }
    
    //  The ControlValues key for the control ID and channel
    val controlValuesKey = ControlValues.makeKey(controlId, channel)
    
    //  Interpolate between the two values over the times, taking into account the two 
    //  smoothness values on the control points
    def interpolate(fromTicks: Int, fromValue: Int, fromSmooth: Double,
                    toTicks: Int, toValue: Int, toSmooth: Double)
    {
      val ticksDiff = toTicks-fromTicks
      var lastValue = fromValue
      var lastTicks = fromTicks
      
      //  Zero smoothness uses linear interpolation
      if (fromSmooth == 0.0 && toSmooth == 0.0)
      {
        //  Interpolate across all control values from that previously set until the new value 
        for (i <- 1 until ticksDiff)
        {
          val interTicks = (fromTicks * (ticksDiff-i) + toTicks * i) / ticksDiff
          val interValue = (fromValue * (ticksDiff-i) + toValue * i) / ticksDiff
  
          if (interValue != lastValue)
          {
            addControllerMessage(interValue, interTicks)
            lastValue = interValue
          }
        }
      }
      else
      {
        //  Non-zero smoothness uses a cubic Bezier interpolation with two Bezier control points 
        //  at the same values (fromValue & toValue), but at times up to half way between the two 
        //  interpolated points
        val t1 = fromTicks + (fromSmooth * ticksDiff).toInt / 2
        val t2 = toTicks - (toSmooth * ticksDiff).toInt / 2
        
        val numberOfBezierPoints = ticksDiff*2
        for (i <- 1 until numberOfBezierPoints)
        {
          val t = 1.0 * i / numberOfBezierPoints
          val interTicks = ((1.0 - t) * (1.0 - t) * (1.0 - t) * fromTicks +
                            3.0 * t * (1.0 - t) * (1.0 - t) * t1 +
                            3.0 * t * t * (1.0 - t) * t2 +
                            t * t * t * toTicks).toInt
          val interValue = ((1.0 - t) * (1.0 - t) * (1.0 - t) * fromValue +
                            3.0 * t * (1.0 - t) * (1.0 - t) * fromValue +
                            3.0 * t * t * (1.0 - t) * toValue +
                            t * t * t * toValue).toInt
          if (interValue != lastValue && interTicks != lastTicks)
          {
            addControllerMessage(interValue, interTicks)
            lastValue = interValue
            lastTicks = interTicks
          }
        }
      }
    }
    
    //  On-off controls have no interpolation
    if (!Controller.isOnOf(controlId))
    {
      //  Does the context timeState have a previous ControlPoint for the key?
      context.timeState.controls.get(controlValuesKey) match
      {
        case None => () //  No previous value, so no interpolation
        
        case Some(ControlPoint(previousTicks, previousValue, previousSmooth)) => 
        {
          //  With no value change, there need be no interpolation
          if (value != previousValue)
          {
            interpolate(previousTicks, previousValue, previousSmooth, context.timeState.ticks, value, smooth);
          }
        }
      }
    }
    
    //  Add the final specified control value
    addControllerMessage(value, context.timeState.ticks)
    
    //  The TimeState now records the most recent explicit change of that Control ID
    TimeState.empty(context.timeSig).copy(controls=context.timeState.controls.updated(controlValuesKey, ControlPoint(context.timeState.ticks, value, smooth)))
  }
  
  
  def duration(context: SequenceContext) = TimeState(NoDuration,0, context.timeSig)
}

object Control
{
  def apply (controlId: Int, proportion: Double, smooth: Double) = new Control(controlId, (MaxValue * proportion).toInt, smooth)
  val MinValue = 0
  val MaxValue = 127
}
