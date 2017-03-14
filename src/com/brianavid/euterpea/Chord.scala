package com.brianavid.euterpea

//  A Harmony is a set of intervals, with the lowest interval being the root of the harmony
//  The harmony can be relative to a specific root note or can be relative to the current
//  tonic (often the key, but may be modulated to a different tonic root)

case class Harmony(intervalSet: Set[Int])
{
  //  Although the Harmony is a set of intervals, for inversions, arpeggios etc we need them sorted low to high
  def intervals = intervalSet.toIndexedSeq.sorted
  
  //  Invert a Harmony, my moving the N lowest notes up an octave
  def inv(i: Int) =
  {
    val (transposed, untransposed) = intervals.splitAt(i)
    Harmony(transposed.map(_+12).toSet ++ untransposed)
  }
  
  //  And a few special-case common inversions
  def inv1 = inv(1)
  def inv2 = inv(2)
  def inv3 = inv(3)
  def b = inv(1)
  def c = inv(2)
  def d = inv(3)
  
  //  What is the root (lowest) note of the Harmony
  def root = intervalSet.min
  
  //  Modify the Harmony by raising or lowering the fifth or third or adding a seventh  
  def aug = Harmony(intervalSet-(root+7)+(root+8))
  def dim = Harmony(intervalSet-(root+7)+(root+6))
  def maj = Harmony(intervalSet-(root+3)+(root+4))
  def min = Harmony(intervalSet-(root+4)+(root+3))
  def sus = Harmony(intervalSet-(root+3)-(root+4)+(root+5))
  def sus2 = Harmony(intervalSet-(root+4)-(root+3)+(root+2))
  def add7 = Harmony(intervalSet + (root+10))
  def addMaj7 = Harmony(intervalSet + (root+11)) 
  
  //  Transpose the Harmony up or down a semitone or an octave
  def flat = Harmony(intervalSet.map(_-1))
  def sharp = Harmony(intervalSet.map(_+1))
  def unary_- = Harmony(intervalSet.map(_-12))
  def unary_+ = Harmony(intervalSet.map(_+12))
}

//object Harmony
//{
//  val majorIntervals = Vector(0, 2, 4, 5, 7, 9, 11, 12, 14, 16, 17, 19, 21, 23)
//  val minorIntervals = Vector(0, 2, 3, 5, 7, 8, 18, 12, 14, 15, 17, 19, 20, 22)
//}

//  Some very common Harmonies which can be applied to a Note to form a Chord 
object Maj extends Harmony(Set(0, 4, 7))
object Min extends Harmony(Set(0, 3, 7))
object Maj7 extends Harmony(Set(0, 4, 7, 10))
object Min7 extends Harmony(Set(0, 3, 7, 10))

//  A chord is a Harmony with either an explicit root note or which is relative to the current tonic 
case class Chord(root: Option[Note], harmony: Harmony) extends Music 
{
  //  Add the Chord to the current sequence so that all the notes, transposed by the Harmony intervals) 
  //  sound at the same time.
  def add(context: SequenceContext) =
  {
    //  What is the root of the Chord? Is it provided or the current tonic?
    val rootNote = root.getOrElse(context.tonic); 
    
    //  What notes does the Chord play? Transpose the root note up by each of the Harmony intervals
    val notes = harmony.intervals.map(rootNote/Transpose(_))
    
    //  Construct a Music object of each of these notes sounding at the same time
    notes.foldRight(EmptyMusic: Music)(_ & _).add(context)
  }
  
  def duration(context: SequenceContext) = context.durationTiming * context.scaleBeats / context.scaleNum
  
  //  Modify the Chord by modifying the underlying Harmony  
  def b = Chord(root, harmony.b)
  def c = Chord(root, harmony.c)
  def d = Chord(root, harmony.d)
  def aug = Chord(root, harmony.aug)
  def dim = Chord(root, harmony.dim)
  def maj = Chord(root, harmony.maj)
  def min = Chord(root, harmony.min)
  def sus = Chord(root, harmony.sus)
  def sus2 = Chord(root, harmony.sus2)
  def add7 = Chord(root, harmony.add7)
  def addMaj7 = Chord(root, harmony.addMaj7)
  def flat = Chord(root, harmony.flat)
  def sharp = Chord(root, harmony.sharp)
  def unary_- = Chord(root, -harmony)
  def unary_+ = Chord(root, +harmony)
}

//  Some basic tonic-relative Chords using Roman number notation
object I extends Chord(None, Harmony( Set(0, 4, 7)))
object Im extends Chord(None, Harmony( Set(0, 3, 7)))
object II extends Chord(None, Harmony( Set(2, 5, 9)))
object III extends Chord(None, Harmony( Set(4, 7, 11)))
object IV extends Chord(None, Harmony( Set(5, 9, 12)))
object V extends Chord(None, Harmony( Set(7, 11, 14)))
object VI extends Chord(None, Harmony( Set(9, 12, 16)))
object VII extends Chord(None, Harmony( Set(11, 14, 17)))

//  Some tonic-relative Chords using Roman number notation, each with an added dominant seventh
object I7 extends Chord(None, Harmony( Set(0, 4, 7, 10)))
object Im7 extends Chord(None, Harmony( Set(0, 3, 7, 10)))
object II7 extends Chord(None, Harmony( Set(2, 5, 9, 12)))
object III7 extends Chord(None, Harmony( Set(4, 7, 11, 14)))
object IV7 extends Chord(None, Harmony( Set(5, 9, 12, 15)))
object V7 extends Chord(None, Harmony( Set(7, 11, 14, 17)))
object VI7 extends Chord(None, Harmony( Set(9, 12, 16, 19)))
object VII7 extends Chord(None, Harmony( Set(11, 14, 17, 21)))

//  All other tonic-relative Chords can be constructed from these with the use of modifiers 