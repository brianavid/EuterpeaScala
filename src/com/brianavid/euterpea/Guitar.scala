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
  
  def strum(delay: Double) = new Pick(this, List(StrumLoHi(delay)((1 until notes.size) :_*)))
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
  
  trait Fingering
  {
    def / (frets: FretSequence): Music
    def /: (frets: FretSequence) = this / frets 
    def on (chord: GuitarChord): Music = this / chord
  }
 
  def tuning(notes: Note*): Guitar = 
  {
    new Guitar((N +: notes.reverse).toVector)
  }
  
  def defaultTuning(): Guitar = tuning(-E, -A, D, G, B, +E)
}

case class StrumStyle(isUp: Boolean, delay: Double)
case class PickedStrings(strings: Set[Int], strum: Option[StrumStyle] = None)

object PickedStrings
{
  implicit def PickedStringsFromNone(strings: Unit) = PickedStrings(Set())
  implicit def PickedStringsFromInt(string: Int) = PickedStrings(Set(string))
  implicit def PickedStringsFrom2(strings: (Int,Int)) = PickedStrings(Set(strings._1, strings._2))
  implicit def PickedStringsFrom3(strings: (Int,Int,Int)) = PickedStrings(Set(strings._1, strings._2, strings._3))
}

case class Strum (isUp: Boolean, delay: Double)
{
  def apply(strings: Int*) = PickedStrings(strings.toSet,Some(StrumStyle(isUp,delay)))
}

object StrumLoHi 
{
  def apply(delay: Double) = Strum(false,delay)
}

object StrumHiLo
{
  def apply(delay: Double)(strings: Int*) = Strum(false,delay)
}

class Pick(
    guitar: Guitar,
    pattern: Seq[PickedStrings]) extends Guitar.Fingering with Music
{
  assert(pattern.forall(_.strings.forall(i => i >= 0 && i < guitar.notes.size)))
  
  def / (frets: FretSequence): Music = {
    frets.check(guitar)
    new PickFretted(guitar, frets, pattern)  
  }
  
  def add(context: SequenceContext) = 
  {
    if (context.guitarFrets.isEmpty)
    {
      TimeState(0).error("Guitar music is missing required Frets or Chord modifiers")
    }
    else 
    {
      (this/context.guitarFrets.get).add(context)
    }
  }
  
  def duration(context: SequenceContext) = 
  {
    if (context.guitarFrets.isEmpty)
    {
      TimeState(0)
    }
    else 
    {
      (this/context.guitarFrets.get).duration(context)
    }
  }
}

class PickFretted(
    guitar: Guitar,
    frets: FretSequence,
    pattern: Seq[PickedStrings]) extends Music
{
  def add(context: SequenceContext) = 
  {
    val ticksFromStartOfGuitarFrets = context.timeState.ticks - context.guitarFretsStartTime.getOrElse(context.timeState).ticks
    
    def pickedStringNote(stringNumber: Int, beats: Beat): Music = 
    {
      val fretPos = frets.positionAfterTime(context.beat, beats.beatTicks+ticksFromStartOfGuitarFrets, guitar, context)
      Guitar.WithString(guitar.strings(stringNumber), fretPos.frets.note(guitar, stringNumber))
    }
    
    def pickedStringNotes(pickedStrings: PickedStrings, beats: Beat): Music = {
      val notes = pickedStrings.strings.toVector.sorted.map(pickedStringNote(_, beats))
      val strummedNotes = pickedStrings.strum match
      {
        case None => notes
        case Some(StrumStyle(false,delay)) => 
          notes.reverse.zipWithIndex.map{
            case (m:Music,i: Int) => (WithDynamics(Dynamics.delay(delay*i), m))
          }
        case Some(StrumStyle(true,delay)) => 
          notes.zipWithIndex.map{
            case (m:Music,i: Int) => (WithDynamics(Dynamics.delay(delay*i), m))
          }
      }
      strummedNotes.reduce(_&_)
    }
      
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
        pickedStringNotes(currentStrings, beat * posWithinPattern).add(context)
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
