package com.brianavid.euterpea
import language.implicitConversions

trait Modifier
{
  //	Abstract method to apply the Modifier to Music
  def modifying(music: Music): Music
  
  //  Two Modifier values combined with / or /: is a MultipleModifiers value
  def / (that: Modifier): MultipleModifiers = new MultipleModifiers(List(that, this))
  def /: (that: Modifier): MultipleModifiers = this / that
}

object Modifier {
  //  A Beat or BeatScale Modifier can be created where a Modifier is expected simply by using an integer value
  //  That integer can be 3 or 5 to specify a BeatScale of triplets or quintuplets
  //  Otherwise the Beat object interprets the value as a powet of two 
  implicit def beatModifierFromInteger(divisor: Int): Modifier =
    {
      if (divisor == 3)
        new BeatScale(3, 2) // Triplet
      else if (divisor == 5)
        new BeatScale(5, 4) // Quintuplet
      else
        Beat.beatFromInteger(divisor)
    }
  
  //  A Lyric modifier can be created where a Modifier is expected simply by using a string value 
  implicit def lyricModifierFromInteger(lyric: String): Modifier =
    Lyric(lyric)
}

//	The NoModifier object does nothing to its Music
object NoModifier extends Modifier
{
  def modifying(music: Music): Music = music
}

//------------------------------------------------------------------------------------------------------------

//  Two or more Modifier values, combined as a MultipleModifiers is also a Modifier, acting as the combination of all contained modifiers
case class MultipleModifiers (modifiers: List[Modifier]) extends Modifier
{
  def modifying(music: Music): Music =
    modifiers.foldLeft(music)((mus, mod) => mus/mod)
  
  //  A Modifiers value and a Modifier value combined with / or /: is a (longer) Modifiers value
  override def / (that: Modifier): MultipleModifiers = new MultipleModifiers(that :: modifiers)
  override def /: (that: Modifier): MultipleModifiers = this / that
}

//------------------------------------------------------------------------------------------------------------


//-------------------------

