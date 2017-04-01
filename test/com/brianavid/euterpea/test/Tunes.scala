package com.brianavid.euterpea.test
import com.brianavid.euterpea._

object BirdsInTheSpringBaritone extends Function0[Music]
{
  val baritonePart = 
  {
    val melody = 
    {
      val line1 = "One"/: -A - "May"/:D - "Morn-"/:D | "ing"/:(D--E) - F | E - E - D | (D--C) - D
      val line2 = F - D - D | (F - E)/8 - D - D | D - C - D | (D--C/2)
      val line3 = Rest/2 - (C - D)/8 | D - -B - D | D/2 - D | -B - C - C | D/2 - Rest 
      val line4 = Rest/2/Dot | Rest - -A - -A | (D - D)/8 - C - -B | (C--D) - E
      val line5 = (F - F)/8 - D - G | (F--D)/8 - -B - C | D - (E--D)/8 - C | D/2 - Rest
      line1 | line2 | line3 | line4 | line5
    }
  
    Track("Baritone") /: Channel("Baritone") /: Instrument("Vibraphone") /: Octave(-1)  /:melody
  }
  
  def apply(): Music = Tempo(140) /: TimeSig(3,4)  /: DMaj /: Dynamics.pulse(Quarter*3, 15) /: (baritonePart)
}

object Bugs extends Function0[Music]
{
  def apply(): Music = Tempo(100) /: AMin /: Instrument("Vibraphone") /: ((A/Min - G/Maj - F/Maj - E/Maj)*3)
}

object Blues extends Function0[Music]
{
  val broken = Broken(0.05)
  def apply(): Music = Tempo(30 -> 90) /: CMaj /: Instrument("Vibraphone") /: Octave(-1) /: (I7.c/broken - II7.b/broken - V7/broken - I)
}

object ArpeggioTest extends Function0[Music]
{
  val pattern = Arpeggio(Thirtysecond,Vector(1,2,3,4,3,1))
  val modifiers = List(Tempo(60), CMaj, Instrument("Vibraphone"), Octave(-1)) 
  def apply(): Music = modifiers /: (I7.c/pattern - II7.b/pattern - V7/pattern - I)
}

object DiatonicTest extends Function0[Music]
{
  val line = C - D - E - F - G
  def apply(): Music = line - line/Transpose(I -> II) - (line & line/Transpose(I -> III) & line/Transpose(I -> V))
}

object OrnamentTest extends Function0[Music]
{
  val line1 = C - D - E - F - G/MordentUp(Thirtysecond) - Rest/2
  val line2 = C - D - E - F - G/MordentDown(Thirtysecond) - Rest/2
  val line3 = C - D - E - F - G/TurnDownUp(Thirtysecond) - Rest/2
  val line4 = C - D - E - F - G/AcciaccaturaUp(Thirtysecond) - Rest/2
  val line5 = C - D - E - F - G/AcciaccaturaDown(Thirtysecond, true) - Rest/2
  val line6 = C - D - E - F - G/Trill(Thirtysecond) - Rest/2
  def apply(): Music = line1 - line2 - line3 - line4 - line5 - line6
}

object PitchBendTest extends Function0[Music]
{
  val pichBend = PitchBend(0, P(Quarter,0, 1.0), P(Quarter,1.0,1.0), P(Quarter,0,1.0), P(Quarter,0))
  def apply(): Music = Legato /: (C - D - E - F - (G/1 & pichBend))
}

object DynamicsTest extends Function0[Music]
{
  val line = C - D - E - F - G - F - E - D - C(5)
  def apply(): Music = line/8/Dynamics.swing(8, 0.167)
}

object RhythmTest extends Function0[Music]
{
  val line = C - D - E - G - E - D - C(5) - B - A - F - A - B - C - D - E - G - E - D - C(5) - B - A - F - A - B - C
  val rhythm = N/4 - (N-N)/8 - N*3/8/3
  def apply(): Music = Rhythm(rhythm) /: line
}

object VolumeTest extends Function0[Music]
{
  val line = (Vmp -> Vff) /:(C - D - E - G - E - D - C(5)) - (Vff -> Vmf) /: (B - A - F - A - B - C) - D - E - G - E - D - C(5) - B - A - F - A - B - C
  def apply(): Music = line
}

object RangeTest extends Function0[Music]
{
  val line = Range(C(5), C(6)) /: (C - D - E - G - E - D - C(5)) - (B - A - F - A - B - C) - Range(C(2), C(3)) /: (D - E - G - E - D - C(5) - B - A - F - A - B - C)
  def apply(): Music = line
}

object ContinuousControllerTest extends Function0[Music]
{
  val modulation = Controller(Controller.Modulation_Wheel)
  val line = C - D - E - G - modulation(0) - E - D - C(5) - B - modulation(100) - A - F - A - B - C - modulation(127)
  def apply(): Music = line
}

object Beats extends Function0[Music]
{
  val bass = Drum(Drum.Bass_Drum_1)
  val snare = Drum(Drum.Snare_Drum_1)
  val openHiHat = Drum(Drum.Open_Hi_hat)
  val closedHiHat = Drum(Drum.Closed_Hi_hat)
  
  val hh1 = (openHiHat - closedHiHat - closedHiHat - closedHiHat)/16
  val hh2 = (openHiHat - closedHiHat - closedHiHat)/8/3
  val bassLine = Track("Base Drum") /: Channel("Drums") /: (bass/8 - Rest/Dot(4))
  val snareLine = Track("Snare Drum") /: Channel("Drums") /: (Rest/4 - snare/8 - Rest/8)
  val hiHatLine = Track("Hi Hats") /: Channel("Drums") /: (hh1 - hh2)
  
  def apply(): Music = (bassLine & snareLine & hiHatLine) * 4
}

object DocTune extends Function0[Music]
{
  val tune1 = (E - D - C/2) * 2 - (G - (F - F)/8 - E/2) * 2
  val topLine = (E - D - C/2) * 2 - (G - (F - F)/8 - E/2) * 2
  val tune2 = {
    val topLine = (G * 12 - B * 6)/8 - +C/2/Dot
    val bottomLine = (F * 6 - E * 6 - D * 6)/8 - C/2/Dot
    TimeSig(6,8) /: (topLine & bottomLine)
  }
  val tune3 = C - D - E - G +|+4 - E - D - C +|+1
  val threeBlindMice = E/"Three" - D/"blind" - C/2/"mice"
  val tune4 = A/Min - G/Maj - F/Maj - E/Maj
  val tune5 = I7.c - II7.b - V7 - I
  val tune7 = {
    val pattern = Arpeggio(Thirtysecond,Vector(1,2,3,4,3,1))
    I7.c/pattern - II7.b/pattern - V7/pattern - I
  }
  val tune8 = Transpose(I -> II) /: (C - D - E - F - G)
  val tune9 = (Vmp -> Vff) /:(C - D - E - G - E - D - C(5)) - (Vff -> Vmf) /: (B - A - F - A - B - C)
  def apply(): Music = tune9
}

object Tunes 
{
  val tunesList = List(
    "doc" -> DocTune,
    "birds" -> BirdsInTheSpringBaritone,
    "bugs" -> Bugs,
    "blues" -> Blues,
    "arpeggio" -> ArpeggioTest,
    "diatonic" -> DiatonicTest,
    "ornament" -> OrnamentTest,
    "pitchbend" -> PitchBendTest,
    "dynamics" -> DynamicsTest,
    "rhythm" -> RhythmTest,
    "volume" -> VolumeTest,
    "range" -> RangeTest,
    "continuous" -> ContinuousControllerTest,
    "beats" -> Beats)
  val tunes: Map[String,Function0[Music]] = tunesList.toMap
  
  def main(args: Array[String]) {
    def usage() = {
      Console.println("Tunes (play|save|strict) <tune> [<path>]")
      for (tuneName <- tunes.keys.toVector.sorted) Console.println(s"\t$tuneName")
    }
    if (args.length <= 1)
    {
      usage()
    }
    else if (args.length <= 2 && args(0) != "play")
    {
      usage()
    }
    else if (!tunes.contains(args(1))) {
      usage()
    }
    else
    {
      val tune = tunes(args(1))()
      args(0) match {
        case "play" => tune.play
        case "save" => tune.writeMidiFile(args(2))
        case "strict" => tune.writeMidiFile(args(2), true); tune.play
        case _ => usage()
      }
    }
  }
}