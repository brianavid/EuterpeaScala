package com.brianavid.euterpea
import javax.sound.{midi => M}

case class Drum(noteNumber: Integer) extends Music 
{
  def add(context: SequenceContext) =
  {
    val noteTiming = context.durationTiming * context.scaleBeats / context.scaleNum
    
    //  Get the track identified by the track name, creating it if it does not exist
    val track = context.getTrack
    
    //  Get the Midi channel identified by the track name, creating it if it does not exist
    val channel = context.channels.getOrElseUpdate(context.currentChannelName, context.channels.size)
    
    val dynamics = context.getDynamics
    
    //  Where does the note start and end playing, taking into account the note width and 
    //  any Dynamics modifiers that can affect the start time
    
    //  What factor of the current beat will the timing be altered by the dynamics?
    val timingIncFactor = dynamics.timingInc - dynamics.timingJitter + (2 * dynamics.timingJitter * new scala.util.Random().nextDouble())
    
    //  How many ticks will the timing be altered by the dynamics?
    val timingInc = (context.durationTiming.ticks.toDouble * timingIncFactor).toInt
    
    val startTicks = context.position.ticks + timingInc
    val endTicks = startTicks + (noteTiming.ticks * (context.noteWidth + dynamics.noteWidthInc)).toInt + context.tiedAddition.beatTicks
    
    //  Add Midi events to start and end the note at the right pitch, volume and timing
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.NOTE_ON, channel, noteNumber, context.volume+dynamics.volumeInc),startTicks))
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.NOTE_OFF, channel, noteNumber, 0),endTicks))
    

    noteTiming
  }
  
  def duration(context: SequenceContext) = context.durationTiming * context.scaleBeats / context.scaleNum
}

object Drum
{
  def apply(drumName: String): Drum = Drum(drumsByName(drumName))
  
  val High_Q = 27
  val Slap = 28
  val Scratch_Push = 29
  val Scratch_Pull = 30
  val Sticks = 31
  val Square_Click = 32
  val Metronome_Click = 33
  val Metronome_Bell = 34
  val Bass_Drum_2 = 35
  val Bass_Drum_1 = 36
  val Side_Stick = 37
  val Snare_Drum_1 = 38
  val Hand_Clap = 39
  val Snare_Drum_2 = 40
  val Low_Tom_2 = 41
  val Closed_Hi_hat = 42
  val Low_Tom_1 = 43
  val Pedal_Hi_hat = 44
  val Mid_Tom_2 = 45
  val Open_Hi_hat = 46
  val Mid_Tom_1 = 47
  val High_Tom_2 = 48
  val Crash_Cymbal_1 = 49
  val High_Tom_1 = 50
  val Ride_Cymbal_1 = 51
  val Chinese_Cymbal = 52
  val Ride_Bell = 53
  val Tambourine = 54
  val Splash_Cymbal = 55
  val Cowbell = 56
  val Crash_Cymbal_2 = 57
  val Vibra_Slap = 58
  val Ride_Cymbal_2 = 59
  val High_Bongo = 60
  val Low_Bongo = 61
  val Mute_High_Conga = 62
  val Open_High_Conga = 63
  val Low_Conga = 64
  val High_Timbale = 65
  val Low_Timbale = 66
  val High_Agogo = 67
  val Low_Agogo = 68
  val Cabasa = 69
  val Maracas = 70
  val Short_Whistle = 71
  val Long_Whistle = 72
  val Short_Guiro = 73
  val Long_Guiro = 74
  val Claves = 75
  val High_Wood_Block = 76
  val Low_Wood_Block = 77
  val Mute_Cuica = 78
  val Open_Cuica = 79
  val Mute_Triangle = 80
  val Open_Triangle = 81
  val Shaker = 82
  val Jingle_Bell = 83
  val Belltree = 84
  val Castanets = 85
  val Mute_Surdo = 86
  val Open_Surdo = 87
  
  val drumsByName = Map(
    "High Q" -> 27,
    "Slap" -> 28,
    "Scratch Push" -> 29,
    "Scratch Pull" -> 30,
    "Sticks" -> 31,
    "Square Click" -> 32,
    "Metronome Click" -> 33,
    "Metronome Bell" -> 34,
    "Bass Drum 2" -> 35,
    "Bass Drum 1" -> 36,
    "Side Stick" -> 37,
    "Snare Drum 1" -> 38,
    "Hand Clap" -> 39,
    "Snare Drum 2" -> 40,
    "Low Tom 2" -> 41,
    "Closed Hi-hat" -> 42,
    "Low Tom 1" -> 43,
    "Pedal Hi-hat" -> 44,
    "Mid Tom 2" -> 45,
    "Open Hi-hat" -> 46,
    "Mid Tom 1" -> 47,
    "High Tom 2" -> 48,
    "Crash Cymbal 1" -> 49,
    "High Tom 1" -> 50,
    "Ride Cymbal 1" -> 51,
    "Chinese Cymbal" -> 52,
    "Ride Bell" -> 53,
    "Tambourine" -> 54,
    "Splash Cymbal" -> 55,
    "Cowbell" -> 56,
    "Crash Cymbal 2" -> 57,
    "Vibra Slap" -> 58,
    "Ride Cymbal 2" -> 59,
    "High Bongo" -> 60,
    "Low Bongo" -> 61,
    "Mute High Conga" -> 62,
    "Open High Conga" -> 63,
    "Low Conga" -> 64,
    "High Timbale" -> 65,
    "Low Timbale" -> 66,
    "High Agogo" -> 67,
    "Low Agogo" -> 68,
    "Cabasa" -> 69,
    "Maracas" -> 70,
    "Short Whistle" -> 71,
    "Long Whistle" -> 72,
    "Short Guiro" -> 73,
    "Long Guiro" -> 74,
    "Claves" -> 75,
    "High Wood Block" -> 76,
    "Low Wood Block" -> 77,
    "Mute Cuica" -> 78,
    "Open Cuica" -> 79,
    "Mute Triangle" -> 80,
    "Open Triangle" -> 81,
    "Shaker" -> 82,
    "Jingle Bell" -> 83,
    "Belltree" -> 84,
    "Castanets" -> 85,
    "Mute Surdo" -> 86,
    "Open Surdo" -> 87)
}