package com.brianavid.euterpea


object BirdsInTheSpringBaritone {
  val baritoneMelody = 
  {
    val line1 = "One"/: -A - "May"/:D - "Morn-"/:D | "ing"/:(D--E) - F | E - E - D | (D--C) - D
    val line2 = F - D - D | Ed/:(F - E) - D - D | D - C - D | (D--C/Hd)
    val line3 = Rest/Hd - Ed/:(C - D) | D - -B - D | D/Hd - D | -B - C - C | D/Hd - Rest 
    val line4 = Rest/Wd.dot | Rest - -A - -A | Ed/:(D - D) - C - -B | (C--D) - E
    val line5 = Ed/:(F - F) - D - G | Ed/:(F--D) - -B - C | D - Ed/:(E--D) - C | D/Hd - Rest
    line1 | line2 | line3 | line4 | line5
  }
  
  val baritonePart = Track("Baritone") /: Instrument("Vibraphone") /: Octave(-1) /: baritoneMelody
  
  val tune = Tempo(140) /: TimeSig(3,Qd)  /: DMaj /: (baritonePart)
}

object Test1 {
  def main(args: Array[String]) {
    BirdsInTheSpringBaritone.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    BirdsInTheSpringBaritone.tune.play()
  }
}