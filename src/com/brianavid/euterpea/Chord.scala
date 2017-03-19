package com.brianavid.euterpea

//  A Harmony is a set of intervals, with the lowest interval being the root of the harmony
//  The harmony can be relative to a specific root note or can be relative to the current
//  tonic (often the key, but may be modulated to a different tonic root)

case class Harmony(intervalSet: Set[Int])
{
  //  Although the Harmony is a set of intervals, for inversions, arpeggios etc we need them sorted low to high
  def intervals = intervalSet.toIndexedSeq.sorted
  
  //  Invert a Harmony, by moving the N lowest notes up an octave
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
  def add6 = Harmony(intervalSet + (root+9))
  def min7 = Harmony(intervalSet + (root+10))
  def maj7 = Harmony(intervalSet + (root+11))
  
  //  Transpose the Harmony up or down a semitone or an octave
  def flat = Harmony(intervalSet.map(_-1))
  def sharp = Harmony(intervalSet.map(_+1))
  def unary_- = Harmony(intervalSet.map(_-12))
  def unary_+ = Harmony(intervalSet.map(_+12))
}

//  The Harmony object defines scale intervals and chord shapes
object Harmony
{
  val majorIntervals = Vector(-1, 0, 2, 4, 5, 7, 9, 11, 12, 14, 16, 17, 19, 21, 23)
  val minorIntervals = Vector(-1, 0, 2, 3, 5, 7, 8, 10, 12, 14, 15, 17, 19, 20, 22)
  def triad(root: Int): Set[Int] = Set(root, root+2, root+4)  //  I.e. 1, 3, 5 for triad(1)
  def seventh(root: Int): Set[Int] = Set(root, root+6)        //  Add a seventh
  
  //  An empty harmony with no root and no intervals
  def apply() = new Harmony(Set.empty)
}

//  Some very common Harmonies which can be applied to a Note to form a Chord 
object Maj extends Harmony(Harmony.triad(1).map(Harmony.majorIntervals))
object Min extends Harmony(Harmony.triad(1).map(Harmony.minorIntervals))
object Dom7 extends Harmony(Harmony.triad(1).map(Harmony.majorIntervals) ++ Harmony.seventh(1).map(Harmony.minorIntervals))
object Maj7 extends Harmony(Harmony.triad(1).map(Harmony.majorIntervals) ++ Harmony.seventh(1).map(Harmony.majorIntervals))
object Min7 extends Harmony(Harmony.triad(1).map(Harmony.minorIntervals) ++ Harmony.seventh(1).map(Harmony.minorIntervals))


//  A chord is a Harmony with either an explicit root note or which is in a position relative to the current tonic 
//  A chord contains a list of transforms that can be applied to the harmony e.g. to add and remove intervals
case class Chord(
    root: Option[Note],           //  An explicit root note
    harmony: Harmony,             //  An explicit harmony
    chordPosition: Int = 1,   //  If the root is None, the chord position relative to the current tonic 
    addSeventh: Boolean = false,  //  Add a seventh to the chord?
    brokenDelay: Option[Double] = None,  //  For a broken chord, the delay between the onset of each note in the chord
    transforms: List[Harmony => Harmony] = Nil) extends Music 
  {
  //  Make the chord a broken cord, adding a fraction of a beat delay to each note in the chord
  def broken(delay: Double) = copy(brokenDelay=Some(delay))
    
  //  Add the Chord to the current sequence so that all the notes (transposed by the Harmony intervals) 
  //  sound at the same time.
  def add(context: SequenceContext) =
  {
    //  Do we have an explicit root note and harmony? 
    val (rootNote, baseHarmony: Harmony) = root match
    {
      //  If so, the explicit root note and harmony are the ones we start from
      case Some(n) => (n, harmony)  
      
      //  Otherwise use the context.tonic as the root note and construct a base harmony
      //  from the chord position and the appropriate (major/minor) intervals for the tonic mode
      case None => {
        //  Get the appropriate (major/minor) intervals for the tonic mode
        val intervals = if (context.isMinor) Harmony.minorIntervals else Harmony.majorIntervals
        
        //  Construct a base harmony from the chord position and tonic intervals, adding a dominant seventh if specified 
        if (addSeventh) 
          (context.tonic, Harmony(Harmony.triad(chordPosition).map(intervals) ++ Harmony.seventh(chordPosition).map(Harmony.minorIntervals)))
        else
          (context.tonic, Harmony(Harmony.triad(chordPosition).map(intervals)))
      }
    }
    
    //  The effective harmony is constructed from the base harmony by apply each of the chord's transforms in turn 
    val effectiveHarmony = 
    {
      def addTransforms(
          transforms: List[Harmony => Harmony]): Harmony =
      {
        transforms match
        {
          case Nil => baseHarmony
          case t :: ts => t(addTransforms( ts))
        }
      }
      
      addTransforms(transforms)
    }
    
    //  What notes does the Chord play? Transpose the root note up by each of the effective Harmony intervals
    val notes = effectiveHarmony.intervals.map(rootNote/Transpose(_))
    
    //  For a broken chord, each note has a delay (increasing as you go up the chord)
    val delayedNotes = brokenDelay match
    {
      case None => notes
      case Some(delay) => 
        {
          notes.zipWithIndex.map{
            case (n:Music,i: Int) => (WithDynamics(Dynamics.delay(context.beat, delay*i), n))
          }
        }
    }
    
    //  Construct a Music object of each of these notes sounding at the same time
    delayedNotes.foldRight(EmptyMusic: Music)(_ & _).add(context)
  }
  
  def duration(context: SequenceContext) = context.durationTiming(1) * context.scaleBeats / context.scaleNum
  
  //  Modify the Chord by adding a transform which will modify the underlying Harmony as it is added  
  def b = copy(transforms = ((h: Harmony) => h.b) :: transforms)
  def c = copy(transforms = ((h: Harmony) => h.c) :: transforms)
  def d = copy(transforms = ((h: Harmony) => h.d) :: transforms)
  def aug = copy(transforms = ((h: Harmony) => h.aug) :: transforms)
  def dim = copy(transforms = ((h: Harmony) => h.dim) :: transforms)
  def maj = copy(transforms = ((h: Harmony) => h.maj) :: transforms)
  def min = copy(transforms = ((h: Harmony) => h.min) :: transforms)
  def sus = copy(transforms = ((h: Harmony) => h.sus) :: transforms)
  def sus2 = copy(transforms = ((h: Harmony) => h.sus2) :: transforms)
  def add6 = copy(transforms = ((h: Harmony) => h.add6) :: transforms)
  def dom7 = copy(transforms = ((h: Harmony) => h.min7) :: transforms)
  def min7 = copy(transforms = ((h: Harmony) => h.min7) :: transforms)
  def maj7 = copy(transforms = ((h: Harmony) => h.maj7) :: transforms)
  def flat = copy(transforms = ((h: Harmony) => h.flat) :: transforms)
  def sharp = copy(transforms = ((h: Harmony) => h.sharp) :: transforms)
  def unary_- = copy(transforms = ((h: Harmony) => -h) :: transforms)
  def unary_+ = copy(transforms = ((h: Harmony) => +h) :: transforms)
}

//  Some basic tonic-relative Chords using Roman number notation
object I extends Chord(None, Harmony(), 1)
object II extends Chord(None, Harmony(), 2)
object III extends Chord(None, Harmony(), 3)
object IV extends Chord(None, Harmony(), 4)
object V extends Chord(None, Harmony(), 5)
object VI extends Chord(None, Harmony(), 6)
object VII extends Chord(None, Harmony(), 7)

//  Some tonic-relative Chords using Roman number notation, each with an added dominant seventh
object I7 extends Chord(None, Harmony(), 1, true)
object II7 extends Chord(None, Harmony(), 2, true)
object III7 extends Chord(None, Harmony(), 3, true)
object IV7 extends Chord(None, Harmony(), 4, true)
object V7 extends Chord(None, Harmony(), 5, true)
object VI7 extends Chord(None, Harmony(), 6, true)
object VII7 extends Chord(None, Harmony(), 7, true)


//  All other tonic-relative Chords can be constructed from these with the use of modifiers 

//  --------------------------

object Chord
{
  //  Diatonic transposition of a note (specified as semitones) from one Chord position to another
  def transpose(fromChord: Chord, toChord: Chord, semitones: Int, context: SequenceContext): Int =
  {
    //  Get the appropriate (major/minor) intervals for the tonic mode
    val intervals = if (context.isMinor) Harmony.minorIntervals else Harmony.majorIntervals
    
    //  Where (chromatically) in the tonic scale is the note to be transposed? 
    val noteTonicOffset = (semitones - context.tonic.semitones) % 12
 
    //  Where (diatonically) in the scale intervals is the note to be transposed?
    val fromNoteTonicIndex = intervals.indexWhere(_ >= noteTonicOffset)
    
    //  And so where in the scale intervals is the note to which it will be transposed
    //  Note that this may be less than 0 and so must be octave adjusted
    val toNoteTonicIndex = fromNoteTonicIndex + toChord.chordPosition - fromChord.chordPosition
    
    //  Transpose the note by adding and subtracting the found diatonic intervals
    if (fromNoteTonicIndex <= 0)
      semitones - 12 + intervals(toNoteTonicIndex) - intervals(fromNoteTonicIndex + 7)
    else
      semitones + intervals(toNoteTonicIndex) - intervals(fromNoteTonicIndex)
  }
}