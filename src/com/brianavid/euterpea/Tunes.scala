package com.brianavid.euterpea


object BirdsInTheSpringBaritone
{
  val baritonePart = 
  {
    val melody = 
    {
      val line1 = "One"/: -A - "May"/:D - "Morn-"/:D | "ing"/:(D--E) - F | E - E - D | (D--C) - D
      val line2 = F - D - D | (F - E)/8 - D - D | D - C - D | (D--C/2)
      val line3 = Rest/2 - (C - D)/8 | D - -B - D | D/2 - D | -B - C - C | D/2 - Rest 
      val line4 = Rest/Dot(1) | Rest - -A - -A | (D - D)/8 - C - -B | (C--D) - E
      val line5 = (F - F)/8 - D - G | (F--D)/8 - -B - C | D - (E--D)/8 - C | D/2 - Rest
      line1 | line2 | line3 | line4 | line5
    }
  
    Track("Baritone") /: Channel("Baritone") /: Instrument("Vibraphone") /: Octave(-1) /: melody
  }
  
  val tune = Tempo(140) /: TimeSig(3,4)  /: DMaj /: (baritonePart)
}

object Bugs
{
  val tune = Tempo(100) /: AMin /: Instrument("Vibraphone") /: ((A/Min - G/Maj - F/Maj - E/Maj)*3)
}

object Blues
{
  val broken = Broken(0.05)
  val tune = Tempo(60) /: CMaj /: Instrument("Vibraphone") /: Octave(-1) /: (I7.c/broken - II7.b/broken - V7/broken - I)
}

object ArpeggioTest
{
  val pattern = Arpeggio(Thirtysecond,Vector(1,2,3,4,3,1))
  val tune = Tempo(60) /: CMaj /: Instrument("Vibraphone") /: Octave(-1) /: (I7.c/pattern - II7.b/pattern - V7/pattern - I)
}

object DiatonicTest
{
  val line = C - D - E - F - G
  val tune = line - line/Transpose(I -> II) - (line & line/Transpose(I -> III) & line/Transpose(I -> V))
}

object OrnamentTest
{
  val line1 = C - D - E - F - G/MordentUp(Thirtysecond) - Rest/2
  val line2 = C - D - E - F - G/MordentDown(Thirtysecond) - Rest/2
  val line3 = C - D - E - F - G/TurnDownUp(Thirtysecond) - Rest/2
  val line4 = C - D - E - F - G/AcciaccaturaUp(Thirtysecond) - Rest/2
  val line5 = C - D - E - F - G/AcciaccaturaDown(Thirtysecond, true) - Rest/2
  val line6 = C - D - E - F - G/Trill(Thirtysecond) - Rest/2
  val tune = line1 - line2 - line3 - line4 - line5 - line6
}

object DynamicsTest
{
  val line = C - D - E - F - G - F - E - D - C(5)
  val tune = line/8/Dynamics.swing(8, 0.3)
}

object RhythmTest
{
  val line = C - D - E - G - E - D - C(5) - B - A - F - A - B - C - D - E - G - E - D - C(5) - B - A - F - A - B - C
  val rhythm = N/4 - (N-N)/8 - N*3/8/3
  val tune = Rhythm(rhythm) /: line
}

object VolumeTest
{
  val line = Volume.MP /: Crescendo3 /:(C - D - E - G - E - D - C(5)) - Volume.FF /: Diminuendo3 /: (B - A - F - A - B - C) - D - E - G - E - D - C(5) - B - A - F - A - B - C
  val tune = line
}

object ContinuousControllerTest
{
  val modulation = Controller(Controller.Modulation_Wheel)
  val line = C - D - E - G - modulation(0) - E - D - C(5) - B - modulation(100) - A - F - A - B - C - modulation(127)
  val tune = line
}

object Beats
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
  
  val tune = (bassLine & snareLine & hiHatLine) * 4
}

object Test1 
{
  def main(args: Array[String]) {
    ContinuousControllerTest.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    //ContinuousControllerTest.tune.play()
    //Beats.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    //Beats.tune.play()
    //OrnamentTest.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    //OrnamentTest.tune.play()
    //Blues.tune.play()
    //ArpeggioTest.tune.play()
    //BirdsInTheSpringBaritone.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    //BirdsInTheSpringBaritone.tune.play()
  }
}