package com.brianavid.euterpea

//  The KeySig Modifier specified the number of sharps (if +ve) or flats (if -ve) in the key signature

case class KeySig(keySigSharps: Byte, tonic: Note, isMinor: Boolean = false) extends Modifier
{
  def modifying(music: Music): Music =
    new WithKeySig (this, music)
}
object CMaj extends KeySig(0, C)
object GMaj extends KeySig(1, G)
object DMaj extends KeySig(2, D)
object AMaj extends KeySig(3, A)
object EMaj extends KeySig(4, E)
object BMaj extends KeySig(5, B)
object FsMaj extends KeySig(6, Fs)
object CsMaj extends KeySig(7, Cs)
object FMaj extends KeySig(-1, F)
object BfMaj extends KeySig(-2, Bf)
object EfMaj extends KeySig(-3, Ef)
object AfMaj extends KeySig(-4, Af)
object DfMaj extends KeySig(-5, Df)
object GfMaj extends KeySig(-6, Gf)
object CfMaj extends KeySig(-7, Cf)
object AMin extends KeySig(0, A, true)
object EMin extends KeySig(1, E, true)
object BMin extends KeySig(2, B, true)
object FsMin extends KeySig(3, Fs, true)
object CsMin extends KeySig(4, Cs, true)
object DMin extends KeySig(-1, D, true)
object GMin extends KeySig(-2, G, true)
object CMin extends KeySig(-3, C, true)
object FMin extends KeySig(-4, F, true)
object BfMin extends KeySig(-5, Bf, true)
object EfMin extends KeySig(-6, Ef, true)
object AfMin extends KeySig(-7, Af, true)

//  The Modulate Modifier changes the current tonic key for harmonisation without altering the the key signature

case class Modulate(tonic: KeySig) extends Modifier
{
  def modifying(music: Music): Music =
    new WithModulation (tonic, music)
}


//-------------------------

//  Add the music, with a changed key signature

private[euterpea] case class WithKeySig(keySig: KeySig, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val saveKeySig=context.keySig
    context.writeKeySig(keySig.keySigSharps, keySig.isMinor, context.timeState)
    val durationTiming = music.add(context.copy(keySig = keySig, tonic=keySig.tonic, isMinor=keySig.isMinor))
    context.writeKeySig(saveKeySig.keySigSharps, saveKeySig.isMinor, context.timeState+durationTiming)
    durationTiming
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with a changed tonic, but without a changed key signature

private[euterpea] case class WithModulation(keySig: KeySig, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(tonic=keySig.tonic, isMinor=keySig.isMinor))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}
