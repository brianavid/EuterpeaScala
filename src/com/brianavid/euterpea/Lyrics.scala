package com.brianavid.euterpea

case class Lyrics(lyricString: String) extends Modifier
{
  val syllables : Vector[String] =
  {
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

