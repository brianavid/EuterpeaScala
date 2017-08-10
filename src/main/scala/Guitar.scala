package com.brianavid.euterpea
import scala.language.implicitConversions

//  A Guitar object wraps a set of strings, each of which plays a specific note
//  The strings are numbered from 1 (highest note) upwards to the lowest
//  Guitar strings are such that the Note played on one continues playing until another
//  Note is played on the same string 

case class Guitar private (notes: Vector[Music]) extends Modifier {
  
  //  The set of strings
  val strings: Vector[Guitar.String] = notes.map(p => new Guitar.String())
  
  //  A guitar with the same tuning shifted up by a number of semitones as though by a capo on the frets
  def capo(fret: Int) = new Guitar( notes.map{ case N => N; case n => WithTranspose(fret, n)})
  
  //  The pitch (in absolute semitones for a single Note or a transposed Note
  def musicPitch( m: Music, fretNumber: Int = 0): Int = m match {
    case N => -1
    case n: Note => n.absoluteSemitones + fretNumber
    case WithTranspose( t, m) => musicPitch(m, fretNumber+t)
  }
  
  //  The pitch of the note on a given string, offset by a fretNumber, and mapped to the octave-equivalent range 0..11
  def pitches(stringNumber: Int, fretNumber: Int): Int =
  {
    musicPitch(notes(stringNumber), fretNumber) % 12
  }
  
  //  All strings, each zipped with the absolute pitch of the note on that string
  lazy val stringNotes = strings zip (notes.map(m => musicPitch(m)))
  
  //  The string best suited to play a particular pitch - the one which would use the lowest fret
  def pitchString(pitch: Int): Option[Guitar.String] =
  {
    //  For each string, what fret would be used to play the note?
    val stringFrets = stringNotes.map(sn => (sn._1, sn._2 - pitch))
    
    //  For all ones with a non-negative fret position, pick the one with the lowest fret
    val nearestString = stringFrets.filter(_._2 >= 0).sortBy(_._2).take(1)
    if (nearestString.isEmpty) None else Some(nearestString.head._1)
  }
  
  //  Play a picked pattern on the Guitar, which will normally have frets specified (unless open tuned)
  def pick(pattern: PickedStrings*) = new Pick(this, pattern.toList)
  
  //  Play a strummed pattern on all the strings of the Guitar, which will normally have frets specified (unless open tuned)
  def strum(delay: Double) = new Pick(this, List(StrumLoHi(delay)((1 until notes.size) :_*)))
  
  //  A Guitar can be a Modifier, and the Music will play notes on a Guitar-selected string
  def modifying( music: Music) = WithGuitar(this, music)
}

//  The Guitar object defines the Guitar.String class and the Guitar.Fingering trait
//  and provides the only public constructors for Guitar values
object Guitar
{
  //  A Guitar.String that can be applied to any music
  class String extends Modifier
  {
    def modifying(music: Music): Music = {
      new WithString(this, music)
    }
  }

  //  Add the music, played on the Guitar string, so that notes sound until a later note sounds on the same string
  
  private[euterpea] case class WithString( string: Guitar.String, music: Music) extends Music
  {
    def add(context: SequenceContext) =
    {
      music.add(context.copy(onString=Some(string)))
    }
    
    def duration(context: SequenceContext) = music.duration(context)
  }
  
  //  Construct a guitar with specified tuning
  def tuning(notes: Note*): Guitar = 
  {
    new Guitar((N +: notes.reverse).toVector)
  }
  
  //  Construct a guitar with standard Guitar tuning
  def standardTuning(): Guitar = tuning(-E, -A, D, G, B, +E)
}

//  Add the music, played on the Guitar-selected string, so that notes sound until a later note sounds on the same string
  
private[euterpea] case class WithGuitar(guitar: Guitar, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(onGuitar=Some(guitar)))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}


//  When a set of strings are picked together, they may be strummed by introducing a slight delay
//  between the playing of each string. This is its Strum
case class Strum (isUp: Boolean, delay: Double)
{
  //  A Strum value (with a specific delay and direction) can be applied as a function to a sequence of
  //  stringNumbers Int parameters to cause those strings to be picked in a strummed manner 
  def apply(stringNumbers: Int*) = PickedStrings(stringNumbers.toSet,Some(this))
}

//  Constructor object for a Strum that goes from low to high
object StrumLoHi 
{
  def apply(delay: Double) = Strum(false,delay)
}

//  Constructor object for a Strum that goes from high to low
object StrumHiLo
{
  def apply(delay: Double) = Strum(true,delay)
}

//  PickedStrings which are played (or strummed) at the same time
case class PickedStrings(strings: Set[Int], strum: Option[Strum] = None)

//  PickString values are usually constructed as integer string number or as tuples of two or three strings 
//  plucked together. As a pattern may contains beats when no strings are picked, the empty "tuple" ("()")
//  is the PickedStrings for the empty set.
object PickedStrings
{
  implicit def PickedStringsFromNone(strings: Unit) = PickedStrings(Set())
  implicit def PickedStringsFromInt(string: Int) = PickedStrings(Set(string))
  implicit def PickedStringsFrom2(strings: (Int,Int)) = PickedStrings(Set(strings._1, strings._2))
  implicit def PickedStringsFrom3(strings: (Int,Int,Int)) = PickedStrings(Set(strings._1, strings._2, strings._3))
}


//  The Pick class is Music which plays a Guitar pattern of PickedStrings with the actual Notes
//  determined by the context.guitarFrets which must have been established by a FretSequence Modifier
class Pick private [euterpea] (
    guitar: Guitar,
    pattern: Seq[PickedStrings]) extends Music
{
  assert(pattern.forall(_.strings.forall(i => i >= 0 && i < guitar.notes.size)))
  
  //  Get the Music combining the PickedStrings pattern and the FretSequence
  def fretted (frets: FretSequence): Music = {
    frets.check(guitar)
    new PickFretted(guitar, frets, pattern)  
  }
  
  //  A convenience operator which avoids confusion with the wider use of the / operator for modifiers
  //  to allow a pattern to play on a specific chord, taking advantage of the implicit conversion 
  //  of the Chord to GuitarChord
  def on (chord: GuitarChord): Music = this / chord

  //  Add the PickFretted Music, after requiring that the context.guitarFrets value is present
  def add(context: SequenceContext) = 
  {
    if (context.guitarFrets.isEmpty)
    {
      TimeState(0).error("Guitar music is missing required Frets or Chord modifiers")
    }
    else 
    {
      fretted(context.guitarFrets.get).add(context)
    }
  }
  
  //  The duration of the PickFretted Music, after requiring that the context.guitarFrets value is present
  def duration(context: SequenceContext) = 
  {
    if (context.guitarFrets.isEmpty)
    {
      TimeState(0)
    }
    else 
    {
      fretted(context.guitarFrets.get).duration(context)
    }
  }
}

//  Pick a pattern of PickedStrings on a Guitar, with the string Notes determined by the 
//  FretSequence sequence of FretPosition values
private class PickFretted (
    guitar: Guitar,
    frets: FretSequence,
    pattern: Seq[PickedStrings]) extends Music
{
  //  Add notes corresponding the to combination of PickedStrings and FretSequence
  def add(context: SequenceContext) = 
  {
    //  How far in the FretSequence (chord sequence) is this pattern  
    val ticksFromStartOfGuitarFrets = context.timeState.ticks - context.guitarFretsStartTime.getOrElse(context.timeState).ticks
    
    //  What is the Note to be played on a particular string wit the FretPosition determined from the FretSequence
    //  at a particular time (and number of beats within the pattern)
    def pickedStringNote(stringNumber: Int, nBeats: Beat): Music = 
    {
      val fretPos = frets.positionAfterTime(context.beat, nBeats.beatTicks+ticksFromStartOfGuitarFrets, guitar, context)
      Guitar.WithString(guitar.strings(stringNumber), fretPos.frets.note(guitar, stringNumber))
    }
    
    //  What are the set of Notes to be played at this point in the pattern, introducing slight delays
    //  where necessary for strumming
    def pickedStringNotes(pickedStrings: PickedStrings, beats: Beat): Music = {
      //  Get all the notes to be played in string order
      val notes = pickedStrings.strings.toVector.sorted.map(pickedStringNote(_, beats))
      
      //  Are we strumming?
      val strummedNotes = pickedStrings.strum match
      {
        //  No - just used the notes undelayed 
        case None => notes
        
        //  Yes - delay in one order or another
        case Some(Strum(false,delay)) => 
          notes.reverse.zipWithIndex.map{
            case (m:Music,i: Int) => (WithDynamics(Dynamics.delay(delay*i), m))
          }
        case Some(Strum(true,delay)) => 
          notes.zipWithIndex.map{
            case (m:Music,i: Int) => (WithDynamics(Dynamics.delay(delay*i), m))
          }
      }
      
      //  And play the notes "together", each on a different string of course
      strummedNotes.reduce(_&_)
    }
    
    //  The current beat for each point in the sequence
    val beat = context.beat
    
    //  Add notes for the currentStrings at the current time, followed by (recursively) the remainingPattern
    def addPatternPicksInSequence(
        currentStrings: PickedStrings, 
        remainingPattern: Seq[PickedStrings], 
        posWithinPattern: Int,
        context: SequenceContext): TimeState =
    {
      val duration =  if (currentStrings.strings.isEmpty)
        //  No Notes to play at this beat 
        TimeState(beat)  
      else 
        //  Play those picked Notes (strumming as needed)
        pickedStringNotes(currentStrings, beat * posWithinPattern).add(context)
        
      //  Is this the end of the pattern? 
      if (remainingPattern.isEmpty)
        duration
      else
        //  No - recursively play the rest of the notes.
        //    Note that this relies on the TimeState addition to correctly merge the state of 
        //    the playing Notes on each String in correct time order
        duration + addPatternPicksInSequence(remainingPattern.head, remainingPattern.tail, posWithinPattern+1, 
                                               context.copy(timeState = context.timeState+duration))
    }
    
    //  Start the recursive addition of the PickedStrings sequence with the head of the pattern
    addPatternPicksInSequence(pattern.head, pattern.tail, 0, context.copy(beat=beat, scaleBeats=1, scaleNum=1))
  }
  
  //  The duration of the PickFretted is the current Beat multipled by the length of the pattern
  def duration(context: SequenceContext) = context.durationTiming(0) * pattern.length
}
