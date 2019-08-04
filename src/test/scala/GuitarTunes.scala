package com.brianavid.euterpea.test
import com.brianavid.euterpea._

object GuitarTune6 extends Function0[Music]
{
  val g = Guitar.standardTuning
  val st = StrumLoHi(0.04)
  val aMin = GuitarChord(A/Min)
  val gMaj = GuitarChord(G/Maj)
  val fMaj = GuitarChord(F/Maj)
  val eMaj = GuitarChord(E/Maj)                                                                                                                  
  val e7 = GuitarChord(E/Dom7)                                                                                                                
  val chordSequence = (aMin*2 - gMaj*2 - fMaj*2 - eMaj - e7)/1
  def p1 = g.pick( (0, 2), (), 4, 3, 0, 2, 4, ())
  val p2 = g.pick( 0, (), st(4,2), (), 0, 1, 4, ())
  val pattern = (p1 - p2)/8 *4
  val tune = Instrument(Instruments.Acoustic_Guitar_Steel) /: 
             Octave(-1) /: Tempo(160) /: Dynamics.swing(4, 0.05) /: 
             ((pattern/chordSequence)*2)
  def apply(): Music = tune
}  

object GuitarTunes 
{
  def main(args: Array[String]) {
    def display(errors: Seq[(String,String)]) = {
      for (error <- errors) Console.println(s"${error._1} : ${error._2}")
      Nil
    }
    val errors = GuitarTune6().play()
    display(errors)
  }
}