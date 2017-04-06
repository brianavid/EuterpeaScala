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
object BfMaj extends KeySig(-1, Bf)
object EfMaj extends KeySig(-2, Ef)
object AfMaj extends KeySig(-3, Af)
object DfMaj extends KeySig(-4, Df)
object GfMaj extends KeySig(-5, Gf)
object CfMaj extends KeySig(-6, Cf)
object FfMaj extends KeySig(-7, Ff)
object AMin extends KeySig(0, A, true)
object EMin extends KeySig(1, E, true)
object BMin extends KeySig(2, B, true)
object FsMin extends KeySig(3, Fs, true)
object CsMin extends KeySig(4, Cs, true)
object DMin extends KeySig(-1, D, true)
object GMin extends KeySig(-2, G, true)
object CMin extends KeySig(-3, C, true)
object BfMin extends KeySig(-4, Bf, true)
object EfMin extends KeySig(-5, Ef, true)
object AfMin extends KeySig(-6, Af, true)
object DfMin extends KeySig(-7, Df, true)

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
