package com.brianavid.euterpea

sealed trait Modifier

//------------------------------------------------------------------------------------------------------------

//  The Duration Modifier denotes the beat of a Note or Rest.
//  At its basic, it is a Whole, Half, Quarter, Eighth ... note
//  Durations can be added together, dotted, or split into three (for triplets)

class Duration(
    val beatTicks: Int,     //  How many ticks in the duration beat
    val timeSigDenom: Byte) //  The Midi meta encoding of the (simple object) Duration for time signatures (only)
    extends Modifier
{
  def dot = new Duration( beatTicks * 3 / 2, 0)  //  Note and a half
  def + (next: Duration) = new Duration( beatTicks + next.beatTicks, 0) //  Simply added together
}

object Duration
{
  val TPQN = 480    //  Ticks per Quarter Note - The granularity of note length sub-divisions
}

case object NoDuration extends Duration(0, 0)        //  No time
case object Wd extends Duration(Duration.TPQN*4, 0)  //  Whole note
case object Hd extends Duration(Duration.TPQN*2, 1)  //  Half note
case object Qd extends Duration(Duration.TPQN, 2)    //  Quarter note
case object Ed extends Duration(Duration.TPQN/2, 3)  //  Eighth note
case object Sd extends Duration(Duration.TPQN/4, 4)  //  Sixteenth note
case object Td extends Duration(Duration.TPQN/8, 5)  //  Thirty-second note
case class Triplet(base: Duration) extends Duration(base.beatTicks * 2 / 3, 0)


//-------------------------

//  The Volume Modifier controls the volumes of notes

class Volume(val volume: Int) extends Modifier
case object MFv  extends Volume(64)           //  Normal (default) volume
case object Fv extends Volume(MFv.volume+10)  //  Louder (Forte)
case object FFv extends Volume(MFv.volume+20) //  Even louder
case object Pv extends Volume(MFv.volume-10)  //  Quieter (piano)
case object PPv extends Volume(MFv.volume-20) //  Even quieter

//  The Tempo Modifier controls and varies the tempo of the music

case class Tempo (bpm: Int) extends Modifier

//  The TimeSig Modifier controls and varies the time signature of the music

case class TimeSig (number: Byte, duration: Duration) extends Modifier

//  The Width Modifier controls the proportion of the time that a note actually 
//  sounds within its duration 

case class Width(noteWidth: Double) extends Modifier
object DefaultWidth extends Width(0.9)
object Staccato extends Width(0.3)            //  Short and sharp
object Marcato extends Width(0.7)             //  Notes clearly separated  
object Legato extends Width(1.0)              //  Notes flowing together

//  The Transpose Modifier transposes chromatically all notes in its music up and down a number of semitones

case class Transpose( num: Int) extends Modifier

//  The Ocatve Modifier transposes all notes in its music up and down a number of octaves

case class Octave( num: Int) extends Modifier

//  The KeySig Modifier specified the number of sharps (if +ve) or flats (if -ve) in the key signature

case class KeySig( keySigSharps: Byte, isMinor: Boolean = false ) extends Modifier
object CMaj extends KeySig(0)
object GMaj extends KeySig(1)
object DMaj extends KeySig(2)
object AMaj extends KeySig(3)
object EMaj extends KeySig(4)
object BMaj extends KeySig(5)
object FsMaj extends KeySig(6)
object CsMaj extends KeySig(7)
object BfMaj extends KeySig(-1)
object EfMaj extends KeySig(-2)
object AfMaj extends KeySig(-3)
object DfMaj extends KeySig(-4)
object GfMaj extends KeySig(-5)
object CfMaj extends KeySig(-6)
object FfMaj extends KeySig(-7)
object AMin extends KeySig(0, true)
object EMin extends KeySig(1, true)
object BMin extends KeySig(2, true)
object FsMin extends KeySig(3, true)
object CsMin extends KeySig(4, true)
object DMin extends KeySig(-1, true)
object GMin extends KeySig(-2, true)
object CMin extends KeySig(-3, true)
object BfMin extends KeySig(-4, true)
object EfMin extends KeySig(-5, true)
object AfMin extends KeySig(-6, true)
object DfMin extends KeySig(-7, true)

//  The Track Modifier specifies that its music goes in the named track which can be viewed, printed or muted
//  independently of any music on any other named track

case class Track( trackName: String) extends Modifier

//  The Channel Modifier specifies that its music goes in the named Midi channel and so is distinct
//  (e.g. has a different instrument) from any music on any other named channel.
//  The channel names are only used locally and are mapped into integer Midi channel numbers 

case class Channel( channelName: String) extends Modifier

//  The Instrument Modifier specifies the General Midi instrument on which the music should sound
//  It can be specified as an integer value (with constants defined in the Instruments object)
//  or as an instrument name string

case class Instrument( instrument: Int) extends Modifier
object Instrument
{
  def apply(instrumentName: String) = new Instrument(Instruments.instrumentByName.getOrElse(instrumentName, 1))
}

//  FUTURE MODIFIER CLASS DEFINITIONS GO HERE

