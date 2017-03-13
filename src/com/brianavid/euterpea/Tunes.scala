package com.brianavid.euterpea


object BirdsInTheSpringBaritone
{
  val baritonePart = 
  {
    val melody = 
    {
      val line1 = "One"/: -A - "May"/:D - "Morn-"/:D | "ing"/:(D--E) - F | E - E - D | (D--C) - D
      val line2 = F - D - D | Eighth/:(F - E) - D - D | D - C - D | (D--C/Half)
      val line3 = Rest/Half - Eighth/:(C - D) | D - -B - D | D/Half - D | -B - C - C | D/Half - Rest 
      val line4 = Rest/Whole.dot | Rest - -A - -A | Eighth/:(D - D) - C - -B | (C--D) - E
      val line5 = Eighth/:(F - F) - D - G | Eighth/:(F--D) - -B - C | D - Eighth/:(E--D) - C | D/Half - Rest
      line1 | line2 | line3 | line4 | line5
    }
  
    Track("Baritone") /: Channel("Baritone") /: Instrument("Vibraphone") /: Octave(-1) /: melody
  }
  
  val tune = Tempo(140) /: TimeSig(3,Quarter)  /: DMaj /: (baritonePart)
}

object Bugs
{
  val tune = Tempo(100) /: AMin /: Instrument("Vibraphone") /: ((A/Min - G/Maj - F/Maj - E/Maj)*3)
}

object Blues
{
  val tune = Tempo(60) /: CMaj /: Instrument("Vibraphone") /: Octave(-1) /: (I7.c - II7.b - V7 - I)
}

object Beats
{
  val bass = Drum(Drum.Bass_Drum_1)
  val snare = Drum(Drum.Snare_Drum_1)
  val openHiHat = Drum(Drum.Open_Hi_hat)
  val closedHiHat = Drum(Drum.Closed_Hi_hat)
  
  val bassLine = Track("Base Drum") /: Channel("Drums") /: (bass - Rest/Half.dot)
  val snareLine = Track("Snare Drum") /: Channel("Drums") /: (Rest/Half - snare - Rest)
  val hiHatLine = Track("Hi Hats") /: Channel("Drums") /: Sixteenth /: ((openHiHat - closedHiHat - closedHiHat - closedHiHat)*4)
  
  val tune = (bassLine & snareLine & hiHatLine) * 4
}

object Test1 
{
  def main(args: Array[String]) {
    //Bugs.tune.play()
    //BirdsInTheSpringBaritone.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    BirdsInTheSpringBaritone.tune.play()
  }
}