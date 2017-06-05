package com.brianavid.euterpea
import scala.language.implicitConversions

case class FretPosition(fingering: Map[Int,Int], rootString: Option[Int])
{
  def stringNumbers = fingering.keys
  def check(guitar: Guitar) = {
    assert(stringNumbers.forall(i => i >= 0 && i < guitar.notes.size))
  }
  
  def note(guitar: Guitar, stringNumber: Int): Music = 
  {
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

case class FretPositionAndRemainingTicks( frets: FretPosition, ticksAfter: Int)

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

trait FretSequence extends Modifier
{
  def duration(beat: Beat): Int
  def positionAfterTime(beat: Beat, ticks: Int, guitar: Guitar, context: SequenceContext): FretPositionAndRemainingTicks 
  def check(guitar: Guitar) 
  
  def / (modifier: FretsModifier) = modifier.modifyingFrets(this)
  
  def - (second: FretSequence) = FretSequencePair(this, second)
  
  def * (repeats: Int): FretSequence = if (repeats <= 1) this else FretSequencePair(this, this*(repeats-1))
  
  def modifying(music: Music): Music =
    new WithFrets(this, music)
}

case class FretSequenceWithBeat(modifiedBeat: Beat, fretSequence: FretSequence) extends FretSequence
{
  def duration(beat: Beat) = modifiedBeat.beatTicks
  def positionAfterTime(beat: Beat, ticks: Int, guitar: Guitar, context: SequenceContext): FretPositionAndRemainingTicks = 
    fretSequence.positionAfterTime(modifiedBeat, ticks, guitar, context)
    
  def check(guitar: Guitar) = fretSequence.check(guitar)
}

case class FretSequenceWithBeatScaling(numberOfNotes: Integer, numberOfBeats: Integer, fretSequence: FretSequence) extends FretSequence
{
  def duration(beat: Beat) = beat.beatTicks * numberOfBeats / numberOfNotes
  def positionAfterTime(beat: Beat, ticks: Int, guitar: Guitar, context: SequenceContext): FretPositionAndRemainingTicks = 
    fretSequence.positionAfterTime(new Beat(beat.beatTicks * numberOfBeats / numberOfNotes), ticks, guitar, context)

  def check(guitar: Guitar) = fretSequence.check(guitar)
}

case class FretSequencePair(first: FretSequence, second: FretSequence) extends FretSequence
{
  def duration(beat: Beat) = first.duration(beat) + second.duration(beat)
  def positionAfterTime(beat: Beat, ticks: Int, guitar: Guitar, context: SequenceContext): FretPositionAndRemainingTicks = 
  {
    val pos1 = first.positionAfterTime(beat, ticks, guitar, context)
    if (pos1.ticksAfter < 0) pos1
    else second.positionAfterTime(beat, ticks-first.duration(beat), guitar, context)
  }

  def check(guitar: Guitar) = { first.check(guitar); second.check(guitar) }
}

private[euterpea] case class WithFrets( frets: FretSequence, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(guitarFrets = Some(frets), guitarFretsStartTime = Some(context.timeState)))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

case class FretsInSequence(frets: FretPosition) extends FretSequence
{
  def check(guitar: Guitar) = {
    frets.check(guitar)
  }
  
  def duration(beat: Beat) = beat.beatTicks
  def positionAfterTime(beat: Beat, ticks: Int, guitar: Guitar, context: SequenceContext) = FretPositionAndRemainingTicks(frets, ticks-beat.beatTicks)
}

object Frets
{
  def apply(fs: FretSequence) = fs
  
  def apply(fingers: (Int, Int)*): FretsInSequence = new FretsInSequence(FretPosition(fingers.toMap, None))
}

case class ChordPosition(chord:Chord, barre: Int)
{
  def frets(
    guitar: Guitar,
    context: SequenceContext): FretPosition = 
  {
    val chordNotes = chord.notes(context)
    val chordPitches = chordNotes.map{case n: Note => n.semitones}.map(_%12)
    def stringFretPostition(stringNumber: Int, offset: Int): (Int, Int) = 
    {
      val pitch = guitar.pitches(stringNumber, offset)
      if (chordPitches.contains(pitch)) (stringNumber, offset)
      else stringFretPostition(stringNumber, offset+1)
    }
    val stringFretPositions = (1 until guitar.strings.size).map(i => stringFretPostition(i, barre)).toMap
    val rootPitch = chord.rootNote.get.absoluteSemitones%12
    def findRootStringNumber(stringNumber: Int): Option[Int] =
    {
      if (stringNumber <= 0) None
      else if (guitar.pitches(stringNumber, stringFretPositions(stringNumber)) == rootPitch) Some(stringNumber)
      else findRootStringNumber(stringNumber-1)
    }
    val rootStringNumber = findRootStringNumber(guitar.strings.size-1)
    FretPosition(stringFretPositions, rootStringNumber)
  }
}

case class GuitarChord(chordPosition: ChordPosition) extends FretSequence
{
  def frets(
    guitar: Guitar,
    context: SequenceContext) = chordPosition.frets(guitar, context)
    
  def duration(beat: Beat) = beat.beatTicks
  def positionAfterTime(beat: Beat, ticks: Int, guitar: Guitar, context: SequenceContext) = 
    FretPositionAndRemainingTicks(frets(guitar, context), ticks-beat.beatTicks)
    
  def check(guitar: Guitar) = ()
}

object GuitarChord
{
  def apply(chord: Chord, barre: Int = 0): GuitarChord = new GuitarChord(ChordPosition(chord, barre))
  implicit def singleGuitarChord(chord: Chord): GuitarChord = new GuitarChord(ChordPosition(chord, 0))
}
