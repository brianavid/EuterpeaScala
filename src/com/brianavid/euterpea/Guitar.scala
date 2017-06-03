package com.brianavid.euterpea
import scala.language.implicitConversions

case class Guitar(notes: Vector[Music]) {
  
  val strings: Vector[Guitar.String] = notes.map(p => new Guitar.String())
  
  def capo(fret: Int) = new Guitar( notes.map{ case N => N; case n => WithTranspose(fret, n)})
  
  def pitches(stringNumber: Int, offset: Int): Int =
  {
    def musicPitch( m: Music, offset: Int): Int = m match {
      case N => -1
      case n: Note => n.semitones + offset
      case WithTranspose( t, m) => musicPitch(m, offset+t)
    }
    musicPitch(notes(stringNumber), offset) % 12
  }
  
  def pick(pattern: PickedStrings*) = new Pick(this, pattern.toList)
  
  def strum(delay: Double) = new Strum( this, new StrummedStrings(notes.size, 1, delay))
  def strum(startString: Int, endString: Int, delay: Double) = new Strum( this, 
      new StrummedStrings(startString min notes.size max 1, endString min startString max 1, delay))
}

object Guitar
{
  class String extends Modifier
  {
    def modifying(music: Music): Music = {
      new WithString(this, music)
    }
  }

  //  Add the music, played on the Guitar string, so that notes sound until a later note sounds on the same string
  
  private[euterpea] case class WithString( string: String, music: Music) extends Music
  {
    def add(context: SequenceContext) =
    {
      music.add(context.copy(onString=Some(string)))
    }
    
    def duration(context: SequenceContext) = music.duration(context)
  }
  
  case class FretPosition(fingering: Map[Int,Int])
  {
    def stringNumbers = fingering.keys
    def check(guitar: Guitar) = {
      assert(stringNumbers.forall(i => i >= 0 && i < guitar.notes.size))
    }
    
    def note(guitar: Guitar, stringNumber: Int) = 
    {
      val transpose = fingering.getOrElse(stringNumber, 0)
      val openStringNote = guitar.notes(stringNumber)
      if (transpose == 0) openStringNote else WithTranspose(transpose, openStringNote) 
    }
  }
  
  object FretPosition
  {
    def apply(fingers: (Int, Int)*): FretPosition = FretPosition(fingers.toMap)
  }
  
  case class Frets(initial: FretPosition, changes: (Beat, FretPosition)*)
  {
    def check(guitar: Guitar) = {
      initial.check(guitar)
      changes.foreach(_._2.check(guitar))
      }
    
    def positionAfterTime(beats: Beat): FretPosition = 
    {
      if (changes.isEmpty || beats.beatTicks < changes.head._1.beatTicks) initial
      else Frets(changes.head._2, changes.tail: _*).positionAfterTime(beats - changes.head._1)
    }
  }
  
  case class ChordPosition(chord:Chord, barre: Int = 0)
  {
    def frets(
      guitar: Guitar,
      context: SequenceContext): FretPosition = 
    {
      val chordNotes = chord.notes(context)
      val chordPitches = chordNotes.map{case n: Note => n.semitones}.map(_%12)
      def stringFretPostition(stringNumber: Int, offset: Int): (Int, Int) = 
      {
        val stringPitch = guitar.pitches(stringNumber, offset)
        if (chordPitches.contains(stringPitch)) (stringNumber, offset)
        else stringFretPostition(stringNumber, offset+1)
      }
      val stringFretPostitions = (1 until guitar.strings.size).map(i => stringFretPostition(i, 0))
      FretPosition(stringFretPostitions.toMap)
    }
  }
  
  case class Chords(initial: ChordPosition, changes: (Beat, ChordPosition)*)
  {
    def frets(
      guitar: Guitar,
      context: SequenceContext) = Frets(initial.frets(guitar, context), changes.map(c => (c._1, c._2.frets(guitar, context))) : _*)
  }
  
  object Chords
  {
    implicit def singleGuitarChord(chord: Chord) = Chords(ChordPosition(chord))
  }

  trait Fingering
  {
    def / (frets: Frets): Music
    def /: (frets: Frets) = this / frets 
    def / (chords: Chords): Music
    def /: (chords: Chords) = this / chords
    def on (chords: Chords): Music = this / chords
  }
 
  def tuning(notes: Note*): Guitar = 
  {
    new Guitar((N +: notes.reverse).toVector)
  }
  
  def defaultTuning(): Guitar = tuning(-E, -A, D, G, B, +E)
}

case class PickedStrings(strings: Set[Int])

object PickedStrings
{
  implicit def PickedStringsFromNone(strings: Unit) = PickedStrings(Set())
  implicit def PickedStringsFromInt(string: Int) = PickedStrings(Set(string))
  implicit def PickedStringsFrom2(strings: (Int,Int)) = PickedStrings(Set(strings._1, strings._2))
  implicit def PickedStringsFrom3(strings: (Int,Int,Int)) = PickedStrings(Set(strings._1, strings._2, strings._3))
  implicit def PickedStringsFrom4(strings: (Int,Int,Int,Int)) = PickedStrings(Set(strings._1, strings._2, strings._4, strings._4))
}

class Pick(
    guitar: Guitar,
    pattern: Seq[PickedStrings]) extends Guitar.Fingering 
{
  assert(pattern.forall(_.strings.forall(i => i >= 0 && i < guitar.notes.size)))
  
  def / (frets: Guitar.Frets): Music = {
    frets.check(guitar)
    new PickFretted(guitar, frets, pattern)  
  }
  
  def / (chords: Guitar.Chords): Music = {
    new PickChords(guitar, chords, pattern)  
  }
}

class PickFretted(
    guitar: Guitar,
    frets: Guitar.Frets,
    pattern: Seq[PickedStrings]) extends Music
{
  def add(context: SequenceContext) = 
  {
    def pickedStringNote(stringNumber: Int, beats: Beat): Music = 
    {
      val fretPos = frets.positionAfterTime(beats)
      Guitar.WithString(guitar.strings(stringNumber), fretPos.note(guitar, stringNumber))
    }
    
    def pickedStringNotes(stringNumbers: Set[Int], beats: Beat): Music = 
      stringNumbers.map(pickedStringNote(_, beats)).reduce(_&_)
      
    val beat = context.beat
    
    def addPatternPicksInSequence(
        currentStrings: PickedStrings, 
        remainingPattern: Seq[PickedStrings], 
        posWithinPattern: Int,
        context: SequenceContext): TimeState =
    {
      val duration =  if (currentStrings.strings.isEmpty)
        TimeState(beat)
      else 
        pickedStringNotes(currentStrings.strings, beat * posWithinPattern).add(context)
      if (remainingPattern.isEmpty)
        duration
      else
        duration + addPatternPicksInSequence(remainingPattern.head, remainingPattern.tail, posWithinPattern+1, 
                                               context.copy(timeState = context.timeState+duration))
    }
    
    addPatternPicksInSequence(pattern.head, pattern.tail, 0, context.copy(beat=beat, scaleBeats=1, scaleNum=1))
  }
  
  def duration(context: SequenceContext) = context.durationTiming(0) * pattern.length
}

class PickChords(
    guitar: Guitar,
    chords: Guitar.Chords,
    pattern: Seq[PickedStrings]) extends Music
{
  def add(context: SequenceContext) = 
  {
    (new PickFretted(guitar, chords.frets(guitar, context), pattern)).add(context)
  }
  
  def duration(context: SequenceContext) = context.durationTiming(0) * pattern.length
}

case class StrummedStrings(
    startString: Int,
    endString: Int,
    delay: Double)
{
}

class Strum(
    guitar: Guitar,
    strum: StrummedStrings) extends Guitar.Fingering 
{
  def / (frets: Guitar.Frets): Music = {
    frets.check(guitar)
    new StrumFretted(guitar, frets, strum)  
  }
  
  def / (chords: Guitar.Chords): Music = {
    new StrumChords(guitar, chords, strum)  
  }
}

class StrumFretted(
    guitar: Guitar,
    frets: Guitar.Frets,
    strum: StrummedStrings) extends Music
{
  def add(context: SequenceContext) = 
  {
    ???
  }
  
  def duration(context: SequenceContext) = context.durationTiming(0)
}

class StrumChords(
    guitar: Guitar,
    chords: Guitar.Chords,
    strum: StrummedStrings) extends Music
{
  def add(context: SequenceContext) = 
  {
    (new StrumFretted(guitar, chords.frets(guitar, context), strum)).add(context)
  }
  
  def duration(context: SequenceContext) = context.durationTiming(0)
}