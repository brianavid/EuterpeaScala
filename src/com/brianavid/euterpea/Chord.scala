package com.brianavid.euterpea
import language.implicitConversions

//  A Harmony is a set of intervals, with the lowest interval being the root of the harmony
//  The harmony can be relative to a specific root note or can be relative to the current
//  tonic (often the key, but may be modulated to a different tonic root)
//  The root is a member of the intervalSet that is the root of the harmonic triad, which (if non-zero)
//  will be different from the current tonic or whatever note is used as the base of a Chord

case class Harmony(intervalSet: Set[Int], val root: Int = 0)
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
  
  //  Modify the Harmony by raising or lowering the fifth or third or adding a seventh  
  def aug = copy(intervalSet=intervalSet-(root+7)+(root+8))
  def dim = copy(intervalSet=intervalSet-(root+7)+(root+6))
  def maj = copy(intervalSet=intervalSet-(root+3)+(root+4))
  def min = copy(intervalSet=intervalSet-(root+4)+(root+3))
  def sus = copy(intervalSet=intervalSet-(root+3)-(root+4)+(root+5))
  def sus2 = copy(intervalSet=intervalSet-(root+4)-(root+3)+(root+2))
  def add6 = copy(intervalSet=intervalSet + (root+9))
  def min7 = copy(intervalSet=intervalSet + (root+10))
  def maj7 = copy(intervalSet=intervalSet + (root+11))
  def add8 = copy(intervalSet=intervalSet + (root+12))
  def add9 = copy(intervalSet=intervalSet + (root+14))
  def add11 = copy(intervalSet=intervalSet + (root+17))
  def add13 = copy(intervalSet=intervalSet + (root+21))
  
  //  Transpose the Harmony up or down a semitone or an octave
  def flat = Harmony(intervalSet.map(_-1), root-1)
  def sharp = Harmony(intervalSet.map(_+1), root+1)
  def unary_- = Harmony(intervalSet.map(_-12), root-12)
  def unary_+ = Harmony(intervalSet.map(_+12), root+12)
}

//  The Harmony object defines scale intervals and chord shapes
object Harmony
{
  val majorIntervals = Vector(-1, 0, 2, 4, 5, 7, 9, 11, 12, 14, 16, 17, 19, 21, 23)
  val minorIntervals = Vector(-1, 0, 2, 3, 5, 7, 8, 10, 12, 14, 15, 17, 19, 20, 22)
  def triad(root: Int, addSeventh: Boolean = false): Set[Int] = 
    if (addSeventh)
      Set(root, root+2, root+4, root+6)  //  I.e. 1, 3, 5, 7 for triad(1, true)
    else
      Set(root, root+2, root+4)  //  I.e. 1, 3, 5 for triad(1)
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

//  A harmony object, which when applied to a Note will be the triad starting at that note in the current tonic scale
object Triad extends Harmony(Set.empty)


//  A Chord can be broken by the addition of a Broken modifier, which specifies note spacing
case class Broken( 
    val delay: Double  //  The delay between the onset of each note in the chord
  ) extends Modifier
{
  def modifying(music: Music): Music =
    new WithBroken (this, music)
}

//  Class ArpeggioNotes is the set of notes in the Chord (1-base, low to high) which plays at a beat of the arpeggio
case class ArpeggioNotes(notes: List[Int])
object ArpeggioNotes
{
  //  A single Integer is a single notes of the Chord sounding at that beat)
  implicit def arpeggioPointFromInteger (n: Int): ArpeggioNotes =
    {
      new ArpeggioNotes(List(n))
    }
  //  A tuple of two Integers are a two notes of the Chord sounding together at that beat)
  implicit def arpeggioPointFromTuple2(ns: Tuple2[Int,Int]): ArpeggioNotes =
    {
      ArpeggioNotes(List(ns._1, ns._2))
    }
  //  A tuple of three Integers are a three notes of the Chord sounding together at that beat)
  implicit def arpeggioPointFromTuple3(ns: Tuple3[Int,Int,Int]): ArpeggioNotes =
    {
      ArpeggioNotes(List(ns._1, ns._2, ns._3))
    }
  //  A tuple of four Integers are a four notes of the Chord sounding together at that beat)
  implicit def arpeggioPointFromTuple4(ns: Tuple4[Int,Int,Int,Int]): ArpeggioNotes =
    {
      ArpeggioNotes(List(ns._1, ns._2, ns._3, ns._4))
    }
}

//  A Chord can become an Arpeggio by the addition of an Arpeggio modifier, which specifies the beat and note pattern sequence
case class Arpeggio( 
    val beat: Beat,             //  The beat of each note in the arpeggio
    val sequence: Vector[ArpeggioNotes] //  The sequence of the Chord's notes (low to high, 1-based, 0=Rest)
  ) extends Modifier
{
  def modifying(music: Music): Music =
    new WithArpeggio (this, music)
}

//  An Arpeggio is created with a variable number of ArpeggioNotes values
object Arpeggio
{
  def apply(beat: Beat, sequence: ArpeggioNotes*) = new Arpeggio(beat, sequence.toVector)
}

//  A chord is a Harmony with either an explicit rootNote note or which is in a position relative to the current tonic 
//  A chord contains a list of transforms that can be applied to the harmony e.g. to add and remove intervals
private[euterpea] case class Chord(
    rootNote: Option[Note],       //  An explicit rootNote note
    harmony: Harmony,             //  An explicit harmony
    chordPosition: Int = 1,       //  If the rootNote is None, the chord position relative to the current tonic 
    addSeventh: Boolean = false,  //  Add a seventh to the chord?
    transforms: List[Harmony => Harmony] = Nil) extends Music 
{
  def root = 
    {
    new Note(0, "?", chord=Some(this))
    }
  
  //  What is the rootNote note 
  def getRoot(context: SequenceContext) = rootNote match
  {
    //  Do we have an explicit rootNote note and harmony? 
    //  If so, the explicit rootNote note and harmony are the ones we start from
    case Some(n) => n  
    
    //  Otherwise use the context.tonic as the rootNote note and construct a base harmony
    //  from the chord position and the appropriate (major/minor) intervals for the tonic mode
    case None => {
      //  Get the appropriate (major/minor) intervals for the tonic mode
      val intervals = if (context.isMinor) Harmony.minorIntervals else Harmony.majorIntervals
      
      //  Construct a base harmony from the chord position and tonic intervals 
      val triad =  Harmony.triad(chordPosition).map(intervals)
      Note(context.tonic.semitones+triad.min, "?")
    }
  }
  
  //  Add the Chord to the current sequence processing lyrics, and root extraction
  def add(context: SequenceContext) =
  {
    if (!context.lyrics.isEmpty)
    {
      context.writeLyrics(context.timeState.ticks)
      add(context.copy(lyrics = Vector.empty))
    }
    else if (context.extractRootNotes)
      root.add(context.copy(extractRootNotes=false))
    else
      addChord(context)
  }
  
  //  Add the Chord to the current sequence so that all the notes (transposed by the Harmony intervals) 
  //  sound at the same time.
  def addChord(context: SequenceContext) =
  {
    //  Do we have an explicit rootNote note and harmony? 
    val (root, baseHarmony: Harmony) = rootNote match
    {
      //  If so, the explicit rootNote note and non-empty harmony are the ones we start from
      case Some(n) => 
        {
          val actualRoot = n.getActualNote(context)
          if (!harmony.intervalSet.isEmpty)
            (actualRoot, harmony)
          else
          {
            //  But an explicit rootNote and empty harmony means a triad based on that rootNote
            //  in the scale of the current tonality
            
            //  Get the appropriate (major/minor) intervals for the tonic mode
            val intervals = if (context.isMinor) Harmony.minorIntervals else Harmony.majorIntervals
      
            //  Where (chromatically) in the tonic scale is the chord rootrootNote? 
            val noteTonicOffset = (actualRoot.semitones - context.tonic.semitones) % 12
         
            //  Where (diatonically) in the scale intervals is the chord rootNote?
            val fromNoteTonicIndex = intervals.indexWhere(_ >= noteTonicOffset)
            
            //  We build a triad (fixed) rooted at that rootNote, using the intervals of the current scale
            val triad =  Harmony.triad(fromNoteTonicIndex).map(intervals)
            (context.tonic, Harmony(triad, triad.min))
          }
        }
      
      //  Otherwise use the context.tonic as the rootNote note and construct a base harmony
      //  from the chord position and the appropriate (major/minor) intervals for the tonic mode
      case None => {
        //  Get the appropriate (major/minor) intervals for the tonic mode
        val intervals = if (context.isMinor) Harmony.minorIntervals else Harmony.majorIntervals
        
        //  Construct a base harmony from the chord position and tonic intervals, adding a seventh from the tonic scale if specified 
        val triad =  Harmony.triad(chordPosition, addSeventh).map(intervals)
        val root = triad.min
        
        (context.tonic, Harmony(triad, root))
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
    
    //  What notes does the Chord play? Transpose the rootNote note up by each of the effective Harmony intervals
    val notes = effectiveHarmony.intervals.map(root/Transpose(_))
    
    //  Does the Chord have an Arpeggio modifier, to play the Chord's note as a sequence?
    context.arpeggio match
    {
      case Some(Arpeggio(beat, sequence)) =>
      {
        //  How many arpeggiated notes can play in the duration of the note?
        val patternSequenceBeatRatio = context.durationTiming(0).ticks / beat.beatTicks
        
        //  How many notes in the pattern sequence will play?
        val patternSequenceCount = sequence.length min patternSequenceBeatRatio
        
        //  For all notes up to (but not including) that patternSequenceCount limit, play each note, 
        //  each with an ever-increasing start timeState based on the arpeggio beat
        sequence.take(patternSequenceCount-1).zipWithIndex.map{
          case (ArpeggioNotes(List(0)), i) => 
            //  If the note index in the sequence is out of range (e.g. zero) add a Rest
            Rest.add(context.copy(beat=beat, timeState=context.timeState+TimeState(beat * i, 1, context.timeSig)))
          case (ArpeggioNotes(indexes), i) => 
            //  Otherwise play the indexed notes from those in the chord (indexed low-to-high, 1-based)
            indexes.map(j => (if (j < 1 || j > notes.length) Rest else notes(j-1))).
              reduceLeft(_ & _).
              add(context.copy(beat=beat, scaleBeats=1, scaleNum=1, timeState=context.timeState+TimeState(beat * i, 1, context.timeSig)))
        }
        
        //  For the last note in the pattern, also play it with an appropriate delay,
        //  but with a duration of the rest of the Chord's beat
        val lastInPattern = patternSequenceCount-1
        val remainingTiming = new Beat(context.durationTiming(0).ticks - (beat.beatTicks * lastInPattern))
        sequence(patternSequenceCount-1) match {
          case ArpeggioNotes(List(0)) => 
            //  If the note index in the sequence is out of range (e.g. zero) add a Rest
            Rest.add(context.copy(beat=remainingTiming, timeState=context.timeState+TimeState(beat * (patternSequenceCount-1), 1, context.timeSig)))
          case ArpeggioNotes(indexes) => 
            //  Otherwise play the indexed notes from those in the chord (indexed low-to-high, 1-based)
            indexes.map(j => (if (j < 1 || j > notes.length) Rest else notes(j-1))).
              reduceLeft(_ & _).
              add(context.copy(beat=remainingTiming, timeState=context.timeState+TimeState(beat * (patternSequenceCount-1), 1, context.timeSig)))
        }
        //  The length of the arpeggiated Chord remains the same as though not arpeggiated
        context.durationTiming(0)
      }
      
      case None =>
      {
        //  For a broken chord, each note has a delay (increasing as you go up the chord)
        val delayedNotes = context.broken match
        {
          case None => notes  //  I.e. NOT broken
          case Some(Broken(delay)) => 
            {
              notes.zipWithIndex.map{
                case (m:Music,i: Int) => (WithDynamics(Dynamics.delay(delay*i), m))
              }
            }
        }
        
        //  Construct a Music object of each of these notes sounding at the same time
        delayedNotes.foldRight(EmptyMusic: Music)(_ & _).add(context)
      }
    }
  }
  
  def duration(context: SequenceContext) = context.durationTiming(1)
  
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
  def add8 = copy(transforms = ((h: Harmony) => h.add8) :: transforms)
  def add9 = copy(transforms = ((h: Harmony) => h.add9) :: transforms)
  def add11 = copy(transforms = ((h: Harmony) => h.add11) :: transforms)
  def add13= copy(transforms = ((h: Harmony) => h.add13) :: transforms)
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

private[euterpea] object Chord
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
    val toNoteTonicIndex = fromNoteTonicIndex + toChord.chordPosition - fromChord.chordPosition
    
    //  Transpose the note by adding and subtracting the found diatonic intervals
    //  Note that the difference may be less than 0 and so must be octave adjusted
    if (toNoteTonicIndex < 0)
      semitones - 12 + intervals(toNoteTonicIndex+7) - intervals(fromNoteTonicIndex)
    else
      semitones + intervals(toNoteTonicIndex) - intervals(fromNoteTonicIndex)
  }
  
  //  Construct a Chord based on the specified Note, plus the next diatonic Note above and
  //  the next chromatic or diatonic Note below
  //  The Chord will be used as an Ornament and will be arpeggiated on some pattern
  def adjacent(
      note: Note,               //  The note for which diatonic adjacent notes are added in a Chord
      chomaticDown: Boolean,    //  True if the adjacent "Down interval" is chromatic - i.e. a single semitone 
      context: SequenceContext): Chord =
  {
    //  The next diatonic Note above the specified Note in the current tonic
    val intervalUp = transpose(I, II, note.semitones, context) - note.semitones
    //  The next chromatic or diatonic Note below the specified Note in the current tonic
    val intervalDown = if (chomaticDown) -1 else transpose(I, VII, note.semitones, context) - note.semitones - 12
    
    //  Construct a Chord of these three notes - it will be arpeggiated on some pattern
    new Chord( Some(note), Harmony(Set(intervalUp, 0, intervalDown)))
  }
}

//-------------------------

object Root extends Modifier
{
  def modifying(music: Music): Music =
    new WithRoot (music)
}


//-------------------------

//  Add the music, ...

private[euterpea] case class WithBroken( broken: Broken, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(broken=Some(broken)))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, ...

private[euterpea] case class WithArpeggio( arpeggio: Arpeggio, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(arpeggio=Some(arpeggio)))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, ...

private[euterpea] case class WithRoot( music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(extractRootNotes=true))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}
