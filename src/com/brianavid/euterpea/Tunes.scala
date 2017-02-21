package com.brianavid.euterpea


object BirdsInTheSpringBaritone {
  val melody1 = Line(-A, D, D, D--E, Fs, E, E, D, D--Cs, D, Fs, D, D, En/:(Fs > E), D, D, D, Cs, D, D--Cs/Hn)
  val melody2 = Line( Rest/Hn, En/:(Cs > D), D, -B, D, D/Hn, D, -B, Cs, Cs, D/Hn, Rest, Rest/Wn) 
  val melody3 = Line( Rest, -A, -A, En/:(D > D), Cs, -B, Cs--D)
  val melody4 = Line( E, En/:(Fs > Fs), D, G, En/:(Fs--D), -B, Cs, D, En/:(E--D), Cs, D/Hn)
  val baritone = Width(0.8) /: Instrument("Vibraphone") /: Line(melody1, melody2, melody3, melody4)
  val tune = Tempo(160) /: (baritone)
}

object Test1 {
  def main(args: Array[String]) {
    BirdsInTheSpringBaritone.tune.play()
  }
}