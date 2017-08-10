package com.brianavid.euterpea
import javax.sound.{midi => M}

case class Lyrics(lyricString: String) extends Modifier
{
  val syllables : Vector[String] =
  {
  	if (lyricString.isEmpty)
  	  Vector()
  	else {
      val lyricsNormalized = "  +".r.replaceAllIn(lyricString, " ").trim()
    	val words = lyricsNormalized.split(' ')
    	def splitWords(word: String): Array[String] = 
    	{
    	  if (word.head == '-')
    	    "-" +: splitWords(word.tail)
    	  else if (word.last == '-')
    	    splitWords(word.init) :+ "-"
    	  else
    	  {
    	    val dash = word.indexOf('-')
    	    if (dash > 0)
    	      Array(word.substring(0, dash+1)) ++ splitWords(word.substring(dash+1)) 
    	    else
    	      Array(word)
    	  }
    	}
    	words.map(splitWords).flatten.map(s => s.replaceAll("_", " ")).toVector
  	}
  }

  def modifying(music: Music): Music =
    new WithLyrics(syllables, lyricString, music)
}


//  Add the music with the lyrics 

private[euterpea] case class WithLyrics( lyrics: Vector[String], lyricString: String, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val duration = music.add(context.copy(lyrics=lyrics, lyricsStartNotes=context.timeState.noteCount))
    if (duration.noteCount == lyrics.length)
      duration
    else {
      duration.error(s"Lyrics '$lyricString' has ${lyrics.length} sylables but ${duration.noteCount} notes")
    }
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}

//-------------------------

//  The Lyric class specifies a single lyric in the current track at the current position

case class Lyric(lyric: String) extends Modifier
{
  def modifying(music: Music): Music =
    new WithLyric(lyric, music)
}

//  Add the music, which will be a single Note, with the lyric associated with the time of the note 

private[euterpea] case class WithLyric( lyric: String, music: Music) extends Music
{
  def add(context: SequenceContext) =
  {
    val bytearray = lyric.getBytes

    val track = context.getTrack
    track.add(new M.MidiEvent(new M.MetaMessage(0x05, bytearray, bytearray.length),context.timeState.ticks))    
    music.add(context)
  }
  
  def duration(context: SequenceContext) = music.duration(context)
}


