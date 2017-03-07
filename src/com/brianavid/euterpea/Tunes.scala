package com.brianavid.euterpea


object BirdsInTheSpringBaritone
{
  val baritonePart = 
  {
    val melody = 
    {
      val line1 = "One"/: -A - "May"/:D - "Morn-"/:D | "ing"/:(D--E) - F | E - E - D | (D--C) - D
      val line2 = F - D - D | Ed/:(F - E) - D - D | D - C - D | (D--C/Hd)
      val line3 = Rest/Hd - Ed/:(C - D) | D - -B - D | D/Hd - D | -B - C - C | D/Hd - Rest 
      val line4 = Rest/Wd.dot | Rest - -A - -A | Ed/:(D - D) - C - -B | (C--D) - E
      val line5 = Ed/:(F - F) - D - G | Ed/:(F--D) - -B - C | D - Ed/:(E--D) - C | D/Hd - Rest
      line1 | line2 | line3 | line4 | line5
    }
  
    Track("Baritone") /: Channel("Baritone") /: Instrument("Vibraphone") /: Octave(-1) /: melody
  }
  
  val tune = Tempo(140) /: TimeSig(3,Qd)  /: DMaj /: (baritonePart)
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
  
  val bassLine = Track("Base Drum") /: Channel("Drums") /: (bass - Rest/Hd.dot)
  val snareLine = Track("Snare Drum") /: Channel("Drums") /: (Rest/Hd - snare - Rest)
  val hiHatLine = Track("Hi Hats") /: Channel("Drums") /: Sd /: ((openHiHat - closedHiHat - closedHiHat - closedHiHat)*4)
  
  val tune = (bassLine & snareLine & hiHatLine) * 4
}

object Test1 
{
  def main(args: Array[String]) {
    Beats.tune.play()
    //BirdsInTheSpringBaritone.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    //BirdsInTheSpringBaritone.tune.play()
  }
}