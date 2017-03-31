package com.brianavid.euterpea
import javax.sound.{midi => M}

//  The Note is the subclass of Music that plays a sound, with the Instrument, Duration and Volume
//  specified (by Modifiers) and stored in the current SequenceContext

case class Note( 
    semitones: Int,                 //  Semitones from Middle C
    display: String,                //  A string to display the note (for tracing only)
    numSharpsToSharpen: Int = 0,    //  The number of sharps in the key signature needed to sharpen this note
    octave: Int = 0,                //  The octave in which the note plays (default starts on Middle C)
    ornament: Option[Ornament] = None)
  extends Music
{
  val MiddleC = 60
  def absoluteSemitones = MiddleC + semitones + octave*12
  
  def unary_+ = copy(octave = octave+1)  //  The Note plays one octave higher 
  def unary_- = copy(octave = octave-1)  //  The Note plays one octave lower
  
  //  Add a harmony string to the Note to make it a Chord
  def / (harmony: Harmony) = new Chord( Some(this), harmony)
  
  //  Add an Ornament to the Note, so that it will play as a sequence of (diatonically or chromatically) adjacent Notes
  def / (o: Ornament) = copy(ornament = Some(o))
  def /: (o: Ornament) = copy(ornament = Some(o))
  
  //  The number of flats in the key signature needed to flatten this note
  def numFlatsToFlatten = if (numSharpsToSharpen == 0) 0 else 8-numSharpsToSharpen
  
  //  Add the Note to the sequence at the current timeState with the Instrument, Beat duration and Volume
  //  specified in the current SequenceContext
  def add(context: SequenceContext) =
  {
    //  An ornamented Note plays as a sequence of adjacent Notes determined by the Ornament
    ornament match
    {
      case None => addNote(context)
      case Some(o) => o.ornament(copy(ornament=None), context).add(context)
    }
  }
  
  //  Add an unornamented Note to the sequence at the current timeState with the Instrument, Beat duration and Volume
  def addNote(context: SequenceContext) =
  {
    //  Get the track identified by the track name, creating it if it does not exist
    val track = context.getTrack
    
    //  Get the Midi channel identified by the track name, creating it if it does not exist
    val channel = context.channels.getOrElseUpdate(context.currentChannelName, context.channels.size)
    
    //  Get the pitch of the note in the current key signature
    val pitchInKey = 
    {
      if (numSharpsToSharpen == 0) 
        semitones    //  Not transposable (i.e. natural or already sharp or flat) 
      else if (context.keySig.keySigSharps > 0 && context.keySig.keySigSharps >= numSharpsToSharpen)
        semitones+1  //  Note which is sharpened by the current key signature
      else if (context.keySig.keySigSharps < 0 && -context.keySig.keySigSharps >= numFlatsToFlatten)
        semitones-1  //  Note which is flattened by the current key signature
      else 
        semitones    //  Note which the current key signature does not affect
    }
    
    //  Apply any Diatonic transposition
    val transposedPitch = context.dTrans match
    {
      case None => pitchInKey
      case Some((fromChord, toChord)) => Chord.transpose(fromChord, toChord, semitones, context)
    }
    
    //  The pitch at which the note plays, taking into account the octave and any transposition
    val pitch = MiddleC + transposedPitch + octave*12 + context.transpose
    
    //  The pitch limited by any Range Modifier
    val pitchRangeOctaveAdjustment = 
      if (context.rangeHigh.semitones != N.semitones && pitch > context.rangeHigh.absoluteSemitones) 
        (context.rangeHigh.absoluteSemitones - pitch - 11) / 12
      else if (context.rangeLow.semitones != N.semitones && pitch < context.rangeLow.absoluteSemitones) 
        (context.rangeLow.absoluteSemitones - pitch + 11) / 12
      else 
        0
    val pitchInRange = pitch + pitchRangeOctaveAdjustment*12
    
    //if (pitchRangeOctaveAdjustment != 0)
    //{
    //  Console.println(s"Limit $display=$pitch between ${context.rangeLow.display}=${context.rangeLow.absoluteSemitones}..${context.rangeHigh.display}=${context.rangeHigh.absoluteSemitones} to $pitchInRange")
    //}
    
    //  How long does the note last (although only sounding for part of it)
    val noteTiming = context.durationTiming(1) * context.scaleBeats / context.scaleNum
        
    val dynamics = context.getDynamics
    
    //  Where does the note start and end playing, taking into account the note width and 
    //  any Dynamics modifiers that can affect the start time
    
    //  What factor of the current beat will the timing be altered by the dynamics?
    val timingIncFactor = dynamics.timingInc - dynamics.timingJitter + (2 * dynamics.timingJitter * new scala.util.Random().nextDouble())
    
    //  How many ticks will the timing be altered by the dynamics?
    val timingInc = (context.durationTiming(0).ticks.toDouble * timingIncFactor).toInt
    
    val startTicks = context.timeState.ticks + timingInc
    val endTicks = startTicks + (noteTiming.ticks * (context.getNoteWidth + dynamics.noteWidthInc)).toInt
    
    //  Add Midi events to start and end the note at the right pitch, volume and timing
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.NOTE_ON, channel, pitchInRange, context.volume+dynamics.volumeInc), startTicks))
    track.add(new M.MidiEvent(new M.ShortMessage(M.ShortMessage.NOTE_OFF, channel, pitchInRange, 0), endTicks))
    
    noteTiming
  }
  
  def duration(context: SequenceContext) = context.durationTiming(1) * context.scaleBeats / context.scaleNum
  
  //  Allow integer parameter to specify the absolute octave - default is C(4) == MiddleC
  def apply(octave: Int) = new Note(semitones, s"$display($octave)", numSharpsToSharpen, this.octave+octave-4)
}

//  Every note in all scales are individually named
//  These include simple notes which are automatically transposable to the key signature
//  Plus explicit sharps, flats and natural variants, which are NOT transposed 
object Cf extends Note( -1, "Cf")
object C extends Note( 0, "C", 2)
object Cn extends Note( 0, "Cn")
object Cs extends Note( 1, "Cs")
object Css extends Note( 2, "Css")
object Dff extends Note( 0, "Dff")
object Df extends Note( 1, "Df")
object D extends Note( 2, "D", 4)
object Dn extends Note( 2, "Dn")
object Ds extends Note( 3, "Ds")
object Dss extends Note( 4, "Dss")
object Eff extends Note( 2, "Eff")
object Ef extends Note( 3, "Ef")
object E extends Note( 4, "E", 6)
object En extends Note( 4, "En")
object Es extends Note( 5, "Es")
object Ff extends Note( 4, "Ff")
object F extends Note( 5, "F", 1)
object Fn extends Note( 5, "Fn")
object Fs extends Note( 6, "Fs")
object Fss extends Note( 7, "Fss")
object Gff extends Note( 5, "Gff")
object Gf extends Note( 6, "Gf")
object G extends Note( 7, "G", 3)
object Gn extends Note( 7, "Gn")
object Gs extends Note( 8, "Gs")
object Gss extends Note( 9, "Gss")
object Aff extends Note( 7, "Aff")
object Af extends Note( 8, "Af")
object A extends Note( 9, "A", 5)
object An extends Note( 9, "An")
object As extends Note( 10, "As")
object Ass extends Note( 11, "Ass")
object Bff extends Note( 9, "Bff")
object Bf extends Note( 10, "Bf")
object B extends Note( 11, "B", 7)
object Bn extends Note( 11, "Bn")
object Bs extends Note( 12, "Bs")

//  The "N" Note is unplayable, but has a Beat and so can be used in rhythm patterns
object N extends Note(Int.MaxValue, "N")

//-------------------------

//  A Rest object adds nothing to the sequence, but advances the current timeState according to its Beat

case object Rest extends Music
{
  def add(context: SequenceContext) =
  {
    context.durationTiming(1) * context.scaleBeats / context.scaleNum
  }
  
  def duration(context: SequenceContext) = context.durationTiming(1) * context.scaleBeats / context.scaleNum
}

