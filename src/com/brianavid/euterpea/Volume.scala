package com.brianavid.euterpea

//  The Volume Modifier controls the volumes (actually velocities) of notes

case class Volume(volume: Int, isRelative: Boolean = false) extends Modifier
{
  def modifying(music: Music): Music =
    new WithVolume (volume, isRelative, music)

  def -> (toVolume: Volume) = new VolumeChange(this, toVolume)
}

object Volume
{
  val MinVolume = 0
  val MaxVolume = 127
  val DefaultVolume = 68
  val VolumeInc = 15
}

//  Absolute Volume Mofier objects
object Vmf extends Volume(Volume.DefaultVolume)                       //  Normal (default) volume
object Vmp extends Volume(Volume.DefaultVolume  - Volume.VolumeInc)   //  A biq quieter than normal volume
object Vf extends Volume(Volume.DefaultVolume + Volume.VolumeInc)     //  Louder (forte)
object Vff extends Volume(Volume.DefaultVolume + Volume.VolumeInc*2)  //  Even louder
object Vfff extends Volume(Volume.DefaultVolume + Volume.VolumeInc*3) //  Even louder still
object Vp extends Volume(Volume.DefaultVolume - Volume.VolumeInc)     //  Quieter (piano)
object Vpp extends Volume(Volume.DefaultVolume - Volume.VolumeInc*2)  //  Even quieter
object Vppp extends Volume(Volume.DefaultVolume - Volume.VolumeInc*3) //  Even quieter still

//  Relative Volume Modifier objects
object Vcurrent extends Volume(0, true)
object Vlouder extends Volume(Volume.VolumeInc, true)
object Vlouder2 extends Volume(Volume.VolumeInc*2, true)
object Vlouder3 extends Volume(Volume.VolumeInc*3, true)
object Vquieter extends Volume(-Volume.VolumeInc, true)
object Vquieter2 extends Volume(-Volume.VolumeInc*2, true)
object Vquieter3 extends Volume(-Volume.VolumeInc*3, true)

//  The VolumeChange Modifier increases or decreases Volume gradually over the duration of the music

private[euterpea] case class VolumeChange(val fromVolume: Volume, val toVolume: Volume) extends Modifier
{
    def modifying(music: Music): Music =
      new WithVolumeChange(if (toVolume.isRelative) toVolume.volume else toVolume.volume-fromVolume.volume, 
                           new WithVolume(fromVolume.volume, fromVolume.isRelative, music))
}

//-------------------------

//  Add the music, with a changed current note volume

private[euterpea] case class WithVolume(volume: Int, isRelative: Boolean, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    music.add(context.copy(volume=if (isRelative) (context.volume+volume max 0 min 127) else volume))
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  Add the music, with a changing note volume across the duration of the music

private[euterpea] case class WithVolumeChange(volumeInc: Int, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val dur = duration(context)
    WithDynamics(Dynamics.volume(new Beat(dur.ticks), volumeInc),music).add(context)
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}
