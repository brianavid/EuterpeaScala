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
  val tune = Tempo(60) /: CMaj /: Instrument("Vibraphone") /: Octave(-1) /: (I7.c - II7.b - V7 - I)
}

object DiatonicTest
{
  val line = C - D - E - F - G
  val tune = Tempo(60) /: CMaj /: Instrument("Vibraphone") /: Octave(-1) /: (line - Transpose(I, II) /: line)
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
    DiatonicTest.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    DiatonicTest.tune.play()
    //Beats.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    //Beats.tune.play()
    //Blues.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    //Blues.tune.play()
    //BirdsInTheSpringBaritone.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    //BirdsInTheSpringBaritone.tune.play()
  }
}