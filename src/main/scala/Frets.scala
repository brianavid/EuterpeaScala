package com.brianavid.euterpea
import scala.language.implicitConversions

//  FretPosition represents the position of fingers on a Guitar fretboard
//  The fingering is a mapping where the key is the string number, starting a 1 (highest), 
//  up to the number of strings on the Guitar (representing the lowest string). The value 
//  of the mapping for each string number is the fret number for a finger on that string

//  The string number 0 is special and corresponds to the string which plays the root note
//  of the chord (if such a note is known) 

private[euterpea] case class FretPosition(fingering: Map[Int,Int], rootString: Option[Int])
{
  //  Check that the frets used fit within the range of strings for the Guitar
  def check(guitar: Guitar) = {
    assert(fingering.keys.forall(i => i >= (if (rootString.isDefined) 0 else 1) && i < guitar.notes.size))
  }
  
  //  Get the Note that plays with the fingering on the specified Guitar string 
  def note(guitar: Guitar, stringNumber: Int): Music = 
  {
    //  Note zero is special
    if (stringNumber == 0 && rootString.isDefined)
      note(guitar, rootString.get)
    else 
    if (stringNumber > 0 && stringNumber < guitar.notes.size)
    {
      val transpose = fingering.getOrElse(stringNumber, 0)
      val openStringNote = guitar.notes(stringNumber)
      if (transpose == 0) openStringNote else WithTranspose(transpose, openStringNote) 
    } 
    else
      ErrorMusic(s"Guitar has no such string number $stringNumber")
  }
}

//  When playing a pattern against a fingering or chord sequence, FretPositionAndRemainingTicks
//  is the value which encodes a FretPosition, and an indication that there may be later
//  chords or fingering in the sequence that should be used instead
case class FretPositionAndRemainingTicks( frets: FretPosition, ticksAfter: Int)

//  The chord or fingering sequence can use a limited number of modifiers, similar to those used by Music
//  Only Beat and BeatScale modifiers have this trait
trait FretsModifier
{ 
  def modifyingFrets(fs: FretSequence): FretSequence
}

object FretsModifier
{
  //  A Beat or BeatScale Modifier can be created where a FretsModifier is expected simply by using an integer value
  //  That integer can be 3 or 5 to specify a BeatScale of triplets or quintuplets
  //  Otherwise the Beat object interprets the value as a power of two 
  implicit def beatFretsModifierFromInteger(divisor: Int): FretsModifier =
    {
      if (divisor == 3)
        new BeatScale(3, 2) // Triplet
      else if (divisor == 5)
        new BeatScale(5, 4) // Quintuplet
      else
        Beat.beatFromInteger(divisor)
    }
}

//  A FretSequence is a sequence of one or more fingerings or chords, with beat values to
//  modify the duration and chained in sequence with the - operator, and repeated with the * operator
//  A FretSequence is a Music Modifier, so that it can apply to complex rhythmic patterns of Guitar picking or strumming.
trait FretSequence extends Modifier
{
  //  Abstract: The duration of the FretSequence component (e.g. a chord)
  def duration(beat: Beat): Int
  
  //  Abstract: The FretPosition (recursively computed) within the FretSequence at a particular time
  def positionAfterTime(
      beat: Beat,     //  The beat which is the duration of each FretsInSequence object within the sequence 
      ticks: Int,     //  The time elapsed within the sequence to select the particular FretPosition
      guitar: Guitar, //  Context information to allow chord fingerings to be used based on the current key signature or tonic
      context: SequenceContext): FretPositionAndRemainingTicks 
  
  //  Abstract:  Check that the frets used fit within the range of strings for the Guitar
  def check(guitar: Guitar) 
  
  //  Modify the Beat or BeateScale
  def / (modifier: FretsModifier) = modifier.modifyingFrets(this)
 
  //  Combine two FretSequence into a time-ordered pair 
  def - (second: FretSequence) = FretSequencePair(this, second)
  
  //  Repeat a FretSequence a number of times in sequence
  def * (repeats: Int): FretSequence = if (repeats <= 1) this else FretSequencePair(this, this*(repeats-1))
  
  def modifying(music: Music): Music =
    new WithFrets(this, music)
}

//  A FretSequence modified by the Beat FretsModifier is a FretSequence with a different beat
case class FretSequenceWithBeat(modifiedBeat: Beat, fretSequence: FretSequence) extends FretSequence
{
  def duration(beat: Beat) = modifiedBeat.beatTicks
  def positionAfterTime(beat: Beat, ticks: Int, guitar: Guitar, context: SequenceContext): FretPositionAndRemainingTicks = 
    fretSequence.positionAfterTime(modifiedBeat, ticks, guitar, context)
    
  def check(guitar: Guitar) = fretSequence.check(guitar)
}

//  A FretSequence modified by the BeatScale FretsModifier is a FretSequence with a different beat
case class FretSequenceWithBeatScaling(numberOfNotes: Integer, numberOfBeats: Integer, fretSequence: FretSequence) extends FretSequence
{
  def duration(beat: Beat) = beat.beatTicks * numberOfBeats / numberOfNotes
  def positionAfterTime(beat: Beat, ticks: Int, guitar: Guitar, context: SequenceContext): FretPositionAndRemainingTicks = 
    fretSequence.positionAfterTime(new Beat(beat.beatTicks * numberOfBeats / numberOfNotes), ticks, guitar, context)

  def check(guitar: Guitar) = fretSequence.check(guitar)
}

//  A FretSequence comprising a pair of sequentially joined FretSequence uses the elapsed tim within
//  the sequence to select one or the other as appropriate
case class FretSequencePair(first: FretSequence, second: FretSequence) extends FretSequence
{
  def duration(beat: Beat) = first.duration(beat) + second.duration(beat)
  
  def positionAfterTime(beat: Beat, ticks: Int, guitar: Guitar, context: SequenceContext): FretPositionAndRemainingTicks = 
  {
    //  Is the ticks value within the first FretSequence, with negative reported ticksAfter
    val pos1 = first.positionAfterTime(beat, ticks, guitar, context)
    //  If so, that is the FretPosition we want
    if (pos1.ticksAfter < 0) pos1
    //  Otherwise we want the FretPosition found within the second FretSequence
    else second.positionAfterTime(beat, ticks-first.duration(beat), guitar, context)
  }

  def check(guitar: Guitar) = { first.check(guitar); second.check(guitar) }
}

//  When a FretSequence is used as a Modifier, the frets are set (and time-stamped) in the playing context for use
//  by Guitar picking
private[euterpea] case class WithFrets( frets: FretSequence, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(guitarFrets = Some(frets), guitarFretsStartTime = Some(context.timeState)))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//  The lowest (leaf) object of the FretSequence hierarchy is FretsInSequence, which wraps the specified FretPosition
case class FretsInSequence(frets: FretPosition) extends FretSequence
{
  def check(guitar: Guitar) = {
    frets.check(guitar)
  }
  
  def duration(beat: Beat) = beat.beatTicks
  
  //  The value returned is the FretPosition and the remaining ticks time after the current sequence beats 
  def positionAfterTime(beat: Beat, ticks: Int, guitar: Guitar, context: SequenceContext) = 
    FretPositionAndRemainingTicks(frets, ticks-beat.beatTicks)
}

//  The Frets object wraps a couple of constructors for FretSequence
object Frets
{
  def apply(fs: FretSequence) = fs
  
  //  To allow Fret fingering to be specified simply by string->position mappings 
  def apply(fingers: (Int, Int)*): FretsInSequence = new FretsInSequence(FretPosition(fingers.toMap, None))
}

//  A ChordPosition is a FretPosition, where the string frets are computed from the notes in the chord
//  Each string has a FretPosition value and the note for that value on that string is within the chord
case class ChordPosition(chord:Chord, barre: Int)
{
  def frets(
    guitar: Guitar,
    context: SequenceContext): FretPosition = 
  {
    //  Find the collection of stringFretPositions for the chord as a map of stringNumber to fretNumber for all strings
    val stringFretPositions = 
    {
      //  What notes are in the chord?
      val chordNotes = chord.notes(context)
      
      //  What octave-equivalent pitches are these notes
      val chordPitches = chordNotes.map{case n: Note => n.absoluteSemitones}.map(_%12)
      
      //  Given a stringFretPostition string number and a lowest fret position (e.g. for a barre),
      //  recursively find the fret position that results in a note contained in chordPitches,
      //  increasing fretNumber until we find one
      def stringFretPostition(stringNumber: Int, fretNumber: Int): (Int, Int) = 
      {
        val pitch = guitar.stringPitch(stringNumber, fretNumber)
        if (chordPitches.contains(pitch)) (stringNumber, fretNumber)
        else stringFretPostition(stringNumber, fretNumber+1)
      }

      (1 until guitar.strings.size).map(i => stringFretPostition(i, barre)).toMap
    }
    
    //  Which string number has the  root of the Chord
    val rootStringNumber = 
    {
      //  What is the bottom of the chord
      val bottom = chord.getLowest(context).absoluteSemitones%12
      
      //  And so what string (high to low) has that root note
      def findRootStringNumber(stringNumber: Int): Option[Int] =
      {
        if (stringNumber <= 0) None
        else if (guitar.stringPitch(stringNumber, stringFretPositions(stringNumber)) == bottom) Some(stringNumber)
        else findRootStringNumber(stringNumber-1)
      }

      findRootStringNumber(guitar.strings.size-1)
    }
    
    //  A ChordPosition is a FretPosition for the computed stringFretPositions for the chord
    FretPosition(stringFretPositions, rootStringNumber)
  }
}

//  A GuitarChord is a FretSequence value which wraps the ChordPosition
case class GuitarChord(chordPosition: ChordPosition) extends FretSequence
{
  def frets(
    guitar: Guitar,
    context: SequenceContext) = chordPosition.frets(guitar, context)
    
  def duration(beat: Beat) = beat.beatTicks
  
  //  The value returned is the FretPosition and the remaining ticks time after the current sequence beats 
  def positionAfterTime(beat: Beat, ticks: Int, guitar: Guitar, context: SequenceContext) = 
    FretPositionAndRemainingTicks(frets(guitar, context), ticks-beat.beatTicks)
    
  def check(guitar: Guitar) = ()
}

//  The GuitarChord object wraps a couple of constructors for FretSequence
object GuitarChord
{
  //  The optional barre value for a GuitarChord means that fret numbers lower than that cannot be used
  def apply(chord: Chord, barre: Int = 0): GuitarChord = new GuitarChord(ChordPosition(chord, barre))
  
  //  In the context of a FretSequence, a chord can be implicitly converted to a GuitarChord
  implicit def singleGuitarChord(chord: Chord): GuitarChord = new GuitarChord(ChordPosition(chord, 0))
}
