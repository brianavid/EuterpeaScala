package com.brianavid.euterpea


object BirdsInTheSpringBaritone {
  val melody1 = Line("One" /: -A, "May"/:D, "Morn-"/:D, "ing"/:D--E, F, E, E, D, D--C, D, F, D, D, Ed/:(F - E), D, D, D, C, D, D--C/Hd)
  val melody2 = Line( Rest/Hd, Ed/:(C - D), D, -B, D, D/Hd, D, -B, C, C, D/Hd, Rest, Rest/Wd) 
  val melody3 = Line( Rest, -A, -A, Ed/:(D - D), C, -B, C-D)
  val melody4 = Line( E, Ed/:(F - F), D, G, Ed/:(F--D), -B, C, D, Ed/:(E--D), C, D/Hd)
  val baritone = Track("Baritone") /: Instrument("Vibraphone") /: Octave(-1) /: Line(melody1, melody2, melody3, melody4)
  val tune = Tempo(140) /: TimeSig(3,Qd)  /: DMaj /: (baritone)
}

object Test1 {
  def main(args: Array[String]) {
    BirdsInTheSpringBaritone.tune.writeMidiFile("""D:\Desktop\Tune.mid""")
    BirdsInTheSpringBaritone.tune.play()
  }
}