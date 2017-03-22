package com.brianavid.euterpea

//  An Ornament is an abstract class which turns a Note into a sequence of adjacent Notes played as an 
//  Arpeggio at a (presumably rapid) beat 

abstract class Ornament
{
  val arpeggio: Arpeggio      //  The Arpegio pattern (with beat) for the Ornament 
  val chomaticDown: Boolean   //  True if the adjacent "Down interval" is chromatic - i.e. a single semitone
  
  //  Non-abstract method to ornament the Note by turning it into the arpeggiated Chord of adjacent notes
  def ornament(note: Note, context: SequenceContext): Music = Chord.adjacent(note, chomaticDown, context) / arpeggio

  //  The adjacent Chord is three adjacent notes low-to-high, with 2 being the "Home" note
  protected val Up = 3
  protected val Home = 2
  protected val Down = 1
  protected val Rest = 0
  
}

//  Instance of an Ornament for a Turn, starting Up, then Down
case class TurnDownUp(beat: Beat, chomaticDown: Boolean = false) extends Ornament
{
  val arpeggio = Arpeggio(beat, Vector(Home, Down, Up, Home))
}

//  Instance of an Ornament for a Turn, starting Down, then Up
case class TurnUpDown(beat: Beat, chomaticDown: Boolean = false) extends Ornament
{
  val arpeggio = Arpeggio(beat, Vector(Home, Up, Down, Home))
}

//  Instance of an Ornament for a Mordent using the upper Note
case class MordentUp(beat: Beat, chomaticDown: Boolean = true) extends Ornament
{
  val arpeggio = Arpeggio(beat, Vector(Home, Up, Home))
}

//  Instance of an Ornament for a Mordent, using the lower Note
case class MordentDown(beat: Beat, chomaticDown: Boolean = false) extends Ornament
{
  val arpeggio = Arpeggio(beat, Vector(Home, Down, Home))
}

//  Instance of an Ornament for a Acciaccatura using the upper Note
case class AcciaccaturaUp(beat: Beat, chomaticDown: Boolean = true) extends Ornament
{
  val arpeggio = Arpeggio(beat, Vector(Up, Home))
}

//  Instance of an Ornament for a Acciaccatura, using the lower Note
case class AcciaccaturaDown(beat: Beat, chomaticDown: Boolean = false) extends Ornament
{
  val arpeggio = Arpeggio(beat, Vector( Down, Home))
}

//  Instance of an Ornament for a Trill
case class Trill(beat: Beat, chomaticDown: Boolean = true) extends Ornament
{
  val arpeggio = Arpeggio(beat, Vector(Home, Up, Home, Up, Home, Up, Home, Up, 
                                       Home, Up, Home, Up, Home, Up, Home, Up, 
                                       Home, Up, Home, Up, Home, Up, Home, Up, 
                                       Home, Up, Home, Up, Home, Up, Home, Up))
}