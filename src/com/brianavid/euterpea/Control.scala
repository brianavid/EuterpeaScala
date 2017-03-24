package com.brianavid.euterpea
import javax.sound.{midi => M}

//  Control values are music objects which have no duration, but which alter the value of a 
//  Midi continuous controller on a channel

//  A ControlPoint is the value of a controller at a specific point of time (the thicks) when it was last set
case class ControlPoint(ticks: Int, value: Int)

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
      for ((key, value1 @ ControlPoint(ticks1, _)) <- values) yield
        that.get(key) match
        {
          case None => key -> value1
          case Some(value2 @ ControlPoint(ticks2, _)) =>
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
  def apply(value: Int) = new Control( controlId, value)
}

//  Midi controller IDs are defined by the Midi spec
object Controller
{
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

//  The Control class is Music, which has no duration. When added, it interpolates values into the sequence 
//  from the most recently set ControlPoint (for the control ID and channel) linearly to the currently set value
//  across the time period from when that previous ControlPoint was set
case class Control(controlId: Int, value: Int) extends Music 
{
  def add(context: SequenceContext) =
  {
    //  Get the track identified by the track name, creating it if it does not exist
    val track = context.getTrack
    
    //  Get the Midi channel identified by the track name, creating it if it does not exist
    val channel = context.channels.getOrElseUpdate(context.currentChannelName, context.channels.size)
    
    //  Helper function to add a single CONTROL_CHANGE Midi event at a time
    def addControllerMessage(value: Int, ticks: Int) =
    {
      track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.CONTROL_CHANGE, channel, controlId, value), ticks))
    }
    
    //  They ControlValues key for the control ID and channel
    val controlValuesKey = ControlValues.makeKey(controlId, channel)
    
    //  On-off controls have no interpolation
    if (!Controller.isOnOf(controlId))
    {
      //  Does the context timeState have a previous ControlPoint for the key?
      context.timeState.controls.get(controlValuesKey) match
      {
        case None => () //  No previous value, so no interpolation
        
        case Some(ControlPoint(previousTicks, previousValue)) => 
        {
          //  Interpolate across all control values from that previously set until the new value 
          val valueDiffAbs = Math.abs(value-previousValue)
          for (i <- 1 until valueDiffAbs)
          {
            val interTicks = (previousTicks * (valueDiffAbs-i) + context.timeState.ticks * i) / valueDiffAbs
            val interValue = (previousValue * (valueDiffAbs-i) + value * i) / valueDiffAbs

            addControllerMessage(interValue, interTicks)
          }
        }
      }
    }
    
    //  Add the final specified control value
    addControllerMessage(value, context.timeState.ticks)
    
    //  The TimeState now records the most recent explicit change of that Control ID
    TimeState(0, 0, None, context.timeState.controls.updated(controlValuesKey, ControlPoint(context.timeState.ticks, value)))
  }
  
  
  def duration(context: SequenceContext) = TimeState(NoDuration,0)
}
