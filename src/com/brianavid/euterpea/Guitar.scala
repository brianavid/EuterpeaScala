package com.brianavid.euterpea

case class Guitar(numStrings: Int) {
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
 
}