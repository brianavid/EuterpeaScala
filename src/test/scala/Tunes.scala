package com.brianavid.euterpea.test
import com.brianavid.euterpea._

object BirdsInTheSpringBaritone extends Function0[Music]
{
  val baritonePart = 
  {
    val melody = 
    {
      val line1 = "One"/: -A - "May"/:D - "Morn-"/:D | "ing"/:(D--E) - F | E - E - D | (D--C) - D
      val line2 = F - D - D | (F - E)/8 - D - D | D - C - D | (D--C/2)
      val line3 = Rest/2 - (C - D)/8 | D - -B - D | D/2 - D | -B - C - C | D/2 - Rest 
      val line4 = Rest/2/Dot | Rest - -A - -A | (D - D)/8 - C - -B | (C--D) - E
      val line5 = (F - F)/8 - D - G | (F--D)/8 - -B - C | D - (E--D)/8 - C | D/2 - Rest
      line1 | line2 | line3 | line4 | line5
    }
  
    Track("Baritone") /: Channel("Baritone") /: Instrument("Vibraphone") /: Octave(-1)  /:melody
  }
  
  def apply(): Music = Tempo(140) /: TimeSig(3,4)  /: DMaj /: Dynamics.pulse(Quarter*3, 15) /: (baritonePart)
}

object Bugs extends Function0[Music]
{
  def apply(): Music = Tempo(100) /: AMin /: Instrument("Vibraphone") /: ((A/Min - G/Maj - F/Maj - E/Maj)*3)
}

object Blues extends Function0[Music]
{
  val broken = Broken(0.05)
  def apply(): Music = Tempo(30 -> 90) /: CMaj /: Instrument("Vibraphone") /: Octave(-1) /: (I7.c/broken - II7.b/broken - V7/broken - I)
}

object ArpeggioTest extends Function0[Music]
{
  val pattern = Arpeggio(Thirtysecond,1,2,3,4,3,1)
  val modifiers = Tempo(60) /: CMaj /: Instrument("Vibraphone")
  def apply(): Music = modifiers /: ((VI7 - II7.b - V7)/pattern - I)
}

object CompTest extends Function0[Music]
{
  val chords = VI7 - II7.b - V7 - I
  val pattern = Arpeggio(Sixteenth,1,(2,3,4),(1,3),(2,3,4))
  val modifiers = Tempo(60) /: CMaj
  val bass = Channel("Bass") /: Track("Bass") /: Octave(-2) /: Instrument(Instruments.Synth_Bass_1) /: Root /: chords
  val arp = Channel("Arp") /: Track("Arp") /: Instrument(Instruments.Vibraphone) /: (chords/pattern)
  def apply(): Music = modifiers /: arp // (arp & bass)
}

object DiatonicTest extends Function0[Music]
{
  val line = C - D - E - F - G
  def apply(): Music = line - line/Transpose(I -> II) - (line & line/Transpose(I -> III) & line/Transpose(I -> V))
}

object OrnamentTest extends Function0[Music]
{
  val line1 = C - D - E - F - G/MordentUp(Thirtysecond) - Rest/2
  val line2 = C - D - E - F - G/MordentDown(Thirtysecond) - Rest/2
  val line3 = C - D - E - F - G/TurnDownUp(Thirtysecond) - Rest/2
  val line4 = C - D - E - F - G/AcciaccaturaUp(Thirtysecond) - Rest/2
  val line5 = C - D - E - F - G/AcciaccaturaDown(Thirtysecond, true) - Rest/2
  val line6 = C - D - E - F - G/Trill(Thirtysecond) - Rest/2
  def apply(): Music = line1 - line2 - line3 - line4 - line5 - line6
}

object PitchBendTest extends Function0[Music]
{
  val pichBend = PitchBend(0, P(Quarter,0, 1.0), P(Quarter,1.0,1.0), P(Quarter,0,1.0), P(Quarter,0))
  def apply(): Music = Legato /: (C - D - E - F - (G/1 & pichBend))
}

object DynamicsTest extends Function0[Music]
{
  val line = C - D - E - F - G - F - E - D - C(5)
  def apply(): Music = line/8/Dynamics.swing(8, 0.167)
}

object RhythmTest extends Function0[Music]
{
  val line = C - D - E - G - E - D - C(5) - B - A - F - A - B - C - D - E - G - E - D - C(5) - B - A - F - A - B - C
  val rhythm = N/4 - (N-N)/8 - N*3/8/3
  def apply(): Music = E - D - Rhythm(rhythm) /: line
}

object VolumeTest extends Function0[Music]
{
  val line = (Vmp -> Vff) /:(C - D - E - G - E - D - C(5)) - (Vff -> Vmf) /: (B - A - F - A - B - C) - D - E - G - E - D - C(5) - B - A - F - A - B - C
  def apply(): Music = line
}

object RangeTest extends Function0[Music]
{
  val line = Range(C(5), C(6)) /: (C - D - E - G - E - D - C(5)) - (B - A - F - A - B - C) - Range(C(2), C(3)) /: (D - E - G - E - D - C(5) - B - A - F - A - B - C)
  def apply(): Music = line
}

object ContinuousControllerTest extends Function0[Music]
{
  val modulation = Controller(Controller.Modulation_Wheel)
  val line = C - D - E - G - modulation(0) - E - D - C(5) - B - modulation(100) - A - F - A - B - C - modulation(127)
  def apply(): Music = line
}

object LyricsTest extends Function0[Music]
{
  val melody1 = G/4 - F/4 - E - F - G/4 | E - F - G - A - G/4 - F - G | F - G - F - E - E - F - E - D   
  val lyrics1 = Lyrics("Shen- khar-- ve----na---khi-------")
  val line1 = melody1/lyrics1
  val melody2 = D/4 - E/4 - F - G - E - F | G/4 - F - E - E - F - E - D | E - F - E - D - C/2
  val lyrics2 = Lyrics("Ax-Lad a--khva- ve---- bu--l----")
  val line2 = melody2/lyrics2
  val tune = line1// | line2
  def apply(): Music = EMaj /: Eighth /: Tempo(60) /: tune
}

object Beats extends Function0[Music]
{
  val bass = Drum(Drum.Bass_Drum_1)
  val snare = Drum(Drum.Snare_Drum_1)
  val openHiHat = Drum(Drum.Open_Hi_hat)
  val closedHiHat = Drum(Drum.Closed_Hi_hat)
  
  val hh1 = (openHiHat - closedHiHat - closedHiHat - closedHiHat)/16
  val hh2 = (openHiHat - closedHiHat - closedHiHat)/8/3
  val bassLine = Track("Base Drum") /: Channel("Drums") /: (bass/8 - Rest/Dot(4))
  val snareLine = Track("Snare Drum") /: Channel("Drums") /: (Rest/4 - snare/8 - Rest/8)
  val hiHatLine = Track("Hi Hats") /: Channel("Drums") /: (hh1 - hh2)
  
  def apply(): Music = (bassLine & snareLine & hiHatLine) * 4
}

object DocTune extends Function0[Music]
{
  val tune1 = (E - D - C/2) * 2 - (G - (F - F)/8 - E/2) * 2
  val topLine = (E - D - C/2) * 2 - (G - (F - F)/8 - E/2) * 2
  val tune2 = {
    val topLine = (G * 12 - B * 6)/8 - +C/2/Dot
    val bottomLine = (F * 6 - E * 6 - D * 6)/8 - C/2/Dot
    TimeSig(6,8) /: (topLine & bottomLine)
  }
  val tune3 = C - D - E - G +|+4 - E - D - C +|+1
  val threeBlindMice = E/"Three" - D/"blind" - C/2/"mice"
  val tune4 = A/Min - G/Maj - F/Maj - E/Maj
  val tune5 = VI7 - II7.b - V7 - I
  val tune7 = {
    val pattern = Arpeggio(Thirtysecond,1,2,3,4,3,1)
    VI7/pattern - II7.b/pattern - V7/pattern - I
  }
  val tune8 = Transpose(I -> II) /: (C - D - E - F - G)
  val tune9 = (Vmp -> Vff) /:(C - D - E - G - E - D - C(5)) - (Vff -> Vmf) /: (B - A - F - A - B - C)
  val tune10 = 
  {
    val r = (Rest - N*3)/8 - N/2
    CMin /: Rhythm(r) /: (G*3 - E - F*3 - D)
  }
  val tune11 = C - D - E - F - G/TurnDownUp(Thirtysecond)
  val tune12 = IV.root/Triad
  def apply(): Music = tune10
}

object AllTheLoveInTheWorld  extends Function0[Music]
{
  val chordSequence = List(+D/Maj7, B/Min.b, +Cs/Min7, Fs/Min.c, +D/Maj7, B/Min.b, +Cs/Min7, Fs/Min.c, 
                            B/Min.add9, B/Min.b, Fs/Min.c, Fs/Min.c, B/Min.add9, B/Min.b, Fs/Min, Fs/Min.b) 
  val chords = chordSequence.foldLeft(EmptyMusic: Music)((m, c) => m - (c/1))
  val chordLine = Track("chords") /: Channel("chords") /: Instrument(Instruments.Vibraphone) /: chords
  val melodyIntro = Rest/4 - (+C - +C)/8 - (+C - +F - +C)/4/3 | B/1 | Rest/4 - (B - B)/8 - (B - +E - B)/4/3 | A/1 
  val melodyV1 = (Rest - A - A - B - B - +C)/8 - +C/4 |  (Rest - A - A - B - B - +C - +C - A)/8 | +C/2/DotDot - A/8 | B/8 - A/8 - Fs/2/Dot
  val melodyLine = Track("melody") /: Channel("melody") /: Instrument(Instruments.Acoustic_Grand_Piano) /: (melodyIntro *2 - melodyV1 * 2) 
  val tune = Tempo(90) /: DMaj /: Octave(-1) /: (Rest/1 - (melodyLine & chordLine))
  def apply(): Music = tune
}

object BabyPlaysAround  extends Function0[Music]
{
  val chords1 = (-A/Maj - -A/Dom7.b | D/Maj - D/Maj.sus | 
                -A/Maj - B/Maj.b | -E/Maj - Cs/Min7 | 
                D/Maj - (B/Min7 -- B/Min7.dim)/4 | +Cs/Min7 - Fs/Min)/2
  val chords2 = (-B/Maj.b - -B/Maj.b | B/Min7 - (B/Min7/4 - E/Maj/4) |
                -A/Maj - -A/Dom7.d | D/Maj.b - D/Min.b | 
                -A/Maj - B/Maj.b | -E/Maj - Cs/Min7 | 
                D/Maj.b - D/Min.b | +Cs/Min7 - Fs/Min | 
                -B/Maj.b - E/Maj.add11 | A/Maj - E/Dom7.d)/2
  val chords3 = (D/Maj.b - -A/Dom7.c | D/Maj - Cs/Min.dim | 
                E/Min7 - A/Dom7 | D/Maj - Ds/Maj.dim | 
                F/Maj.dim - F/Maj.dim | Fs/Min - B/Min7 | 
                D/Min - E/Maj.sus | F/Maj.aug - F/Maj.aug | F/Maj.aug - E/Dom7)/2
  val chords4 = chords1
  val chords5 = (B/Maj.b - B/Maj.b | F/Maj.add6 - F/Maj.add6 | D/Maj - D/Maj |
                F/Maj7 - F/Dom7 | E/Min7 - -B/Maj7.b | E/Min - D/Min7 | 
                A/Maj - A/Maj | F/Maj - F/Maj | F/Maj - F/Maj | A/Dom7/1)/2
                
  val melody1 = (Rest/4/Dot - (A - B)/16 - +Cs/8 - A/Dot) / Lyrics("It's not op-en") |
                (Rest/8 - (A - +D - +C - A - A - B)/16 - A/4 - Rest/4) /Lyrics("to disc-uss-ion a-ny more") |
                (Rest/4 - (Rest - A - A/8/Dot - B - A - F)/16 - F/4) / Lyrics("She's out a-gain to-night") | 
                (Rest/4 - G/16 - G/8 - G/16 - G/8 - E/8 - E/4) / Lyrics("and I'm a-lone once more")|
                (Rest/4 - (F - E - D)/8/3 - A/4/Dot - B/8) / Lyrics("She's all I have worth") |
                (Rest/8 - +C/4 - B/8 - A/4/Dot - F/8) / Lyrics("wait-ing for, but")
               
  val melody2 = (Rest/2 - (+C - B - A - G)/8 | B/1) / Lyrics("ba-by plays a-round") |
                (Rest/4 - (Rest - E - A - B)/16 - A/HalfDot - (A - B - +D)/16) / Lyrics("And so it seems I've al-ways") |
                ((+C - A - A - B)/8 - A/4 - Rest/4) / Lyrics("been the last to know") |
                (Rest/4 - (Rest - A)/16 - A/8 - (B - A)/16 - F/8 - F/4) / Lyrics("to hold on to that girl") |
                (Rest/4 - G/16 - G/8 - G/16 - G/16 - E/8 - E/4/HalfDot) / Lyrics("I had to let her go") |
                (Rest/4 - (F - E - D)/8/3 - A/4/Dot - B/8) / Lyrics("I wish to God I") |
                ((+D - +D - +C - +C)/8 - A/2) / Lyrics("did-n't love her so") |
                (Rest/4/DotDot - E/16 - (+C - B - A - B)/8) / Lyrics("'cos ba-by plays a") |
                (A/2 - Rest/8/Dot - A/16 - (A - B - A)/8/3) / Lyrics("round. I try to be")
                
  val melody3 = (F/2 - Rest/8 - A/16 - A/8/Dot - A/16 - F/16) / Lyrics("strong. Hold on to my") | 
                (F/2 - Rest/2) / Lyrics("pride.") |
                (Rest/4/HalfDot - (-A - E - F)/16 - (Gn - E - B - Gn)/8) / Lyrics("She does-n't e-ven lnow it's") |
                (A/4 - (Rest - F - F - Gn)/16 - (A - F)/8 - +C/4) / Lyrics("wrong, how much I hurt in-side") |
                (Rest/4  - (G - G - A)/8/3 - (B - G - +D - B)/8) / Lyrics("and heav-en knows i've tried but") |
                (Rest/8 - +E/2 - (+D - +D - B)/8) / Lyrics("ba-by plays a") |
                ((+D -- +Cn)/8 - Rest/8 - (A - E)/16 - (E - E)/8 - Rest/4) / Lyrics("round-, just a play thing.") |
                (Rest/4 - (Rest - D - Fn - D)/16 - (Fn - D)/8 - G/4) / Lyrics("It's hard to rec-on-cile") |
                ((Rest - Fn - Fn - Fn)/8 - Fn/8 - E/4/Dot) / Lyrics("the facts I'm fac-ing")
                
  val melody4 = (Rest/4/Dot - (A - B)/16 - +Cs/8 - A/Dot) / Lyrics("It's not op-en") |
                (Rest/8 - (A - +D - +C - A - A - B)/16 - A/4 - Rest/4) /Lyrics("to disc-uss-ion a-ny more") |
                (Rest/4 - (Rest - A - A/8/Dot - B - A - F)/16 - F/4) / Lyrics("She walks these shin-y streets") | 
                (Rest/4 - G/16 - G/8 - G/16 - G/8 - E/8 - E/4) / Lyrics("I walk the worn out floor")|
                (Rest/4 - (F - E - D)/8/3 - A/4/Dot - B/8) / Lyrics("She's all I have worth") |
                (Rest/8 - +C/4 - B/8 - A/4/Dot - F/8) / Lyrics("liv-ing for, but")
                
  val melody5 = (Rest/2/Dot - (+C - B)/8) / Lyrics("Ba-by") |
                (A/2/Dot - (+C - +D)/8) / Lyrics("plays, ba-by") |
                (A/2/Dot - (Rest - D)/8) / Lyrics("plays a") |
                (A/1) / Lyrics("round") |
                Rest/1 * 6
                
                
    val tenor1 = (Rest/4/Dot - (A - B)/16 - +Cs/8 - A/Dot) / Lyrics("It's not op-en") |
                 (Rest/8 - (A - +D - +C - A - A - B)/16 - A/4 - Rest/4) /Lyrics("to disc-uss-ion a-ny more") |
                 (Rest/4 - (Rest - A - A/8/Dot - B - A - F)/16 - F/4) / Lyrics("She's out a-gain to-night") | 
                 (Rest/4 - G/16 - G/8 - G/16 - G/8 - E/8 - E/4) / Lyrics("and I'm a-lone once more")|
                 (Rest/4 - (F - E - D)/8/3 - A/4/Dot - B/8) / Lyrics("She's all I have worth") |
                 (Rest/8 - +C/4 - B/8 - A/4/Dot - F/8) / Lyrics("wait-ing for, but")
               
    val bari1  = (Rest/4/Dot - (A - B)/16 - A/8 - E/Dot) / Lyrics("It's not op-en") |
                 (Rest/8 - (F - A - A - F - F - A)/16 - Gn/4 - Rest/4) /Lyrics("to disc-uss-ion a-ny more") |
                 (Rest/4 - (Rest - A - A/8/Dot - B - A - F)/16 - F/4) / Lyrics("She's out a-gain to-night") | 
                 (Rest/4 - G/16 - G/8 - G/16 - G/8 - E/8 - E/4) / Lyrics("and I'm a-lone once more")|
                 (Rest/4 - (F - E - D)/8/3 - A/4/Dot - B/8) / Lyrics("She's all I have worth") |
                 (Rest/8 - +C/4 - B/8 - A/4/Dot - F/8) / Lyrics("wait-ing for, but")
               
    val bass1  = (A/2 - C/2) / Lyrics("No-") |
                 (Rest/8 - (Rest - D - D - D - D - D)/16 - A/4 - Rest/4) /Lyrics("disc-uss-ion a-ny more") |
                 (Rest/4 - (Rest - A - A/8/Dot - B - A - F)/16 - F/4) / Lyrics("She's out a-gain to-night") | 
                 (Rest/4 - G/16 - G/8 - G/16 - G/8 - E/8 - E/4) / Lyrics("and I'm a-lone once more")|
                 (Rest/4 - (F - E - D)/8/3 - A/4/Dot - B/8) / Lyrics("She's all I have worth") |
                 (Rest/8 - +C/4 - B/8 - A/4/Dot - F/8) / Lyrics("wait-ing for, but")
               

  val chordLine = Track("chords") /: Channel("chords") /: Instrument(Instruments.Vibraphone) /: Octave(-1) /: Vpp /: (chords1 | chords2 | chords3 | chords4 | chords5)
  val melodyLine = Track("melody") /: Channel("melody") /: Instrument(Instruments.Cello) /: Octave(-1) /: (melody1 | melody2 | melody3 | melody4 | melody5)
  
  val tenorLine = Track("Tenor") /: Channel("Tenor") /: Instrument(Instruments.Flute) /: Octave(0) /: (tenor1)
  val bariLine  = Track("Baritone") /: Channel("Baritone") /: Instrument(Instruments.Cello) /: Octave(0) /: (bari1)
  val bassLine  = Track("Bass") /: Channel("Bass") /: Instrument(Instruments.Choir_Aahs) /: Octave(0) /: Vp /: (bass1)
  
  val tune = Tempo(60)/: AMaj /: (Rest/1 - (tenorLine & bariLine & bassLine))
  def apply(): Music = tune
}

object Caledonia extends Function0[Music]
{
  def line(notes: Music, lyricText: String, chords: Music, lastChord: Boolean=false): Music =
  {
    val asMelody =  Track("melody") /: Channel("melody") /: 
                    Instrument("Ohh Voices")
    val asHarmony = Track("harmony") /: Channel("harmony") /:
                    Instrument("Nylon String Guitar") /: 
                    (if (lastChord) Broken(0.05) else Arpeggio(8,1,2,(3,4),1,(3,4),1))
    val asBass =    Track("bass") /: Channel("bass") /: 
                    Instrument("Acoustic Bass")
    
    val melody = notes/8/Lyrics(lyricText)
    val harmony = chords/2/Dot
 
    val bassRhythm = if (lastChord) NoModifier else NoteRhythm(N/(Half+Eighth) - N/Eighth)
    val bass = Octave(-1) /: Vp /: bassRhythm /: Root /: harmony

    melody/asMelody & harmony/asHarmony & bass/asBass
  }
  
  val verse1Line1 = 
    line(C - F - F/4/Dot - C | C - G - G/4/Dot - C | A - B - +C/4 - A - A | B - A - G -- F/4 - Rest,
         "I don't know if you can see the chan-ges that have come ov-er me.-",
         F/Maj | C/Maj | D/Min | -Bf/Maj)
         
  val verse1Line2 =
    line(C/Pickup | C - F - F/4/Dot - C | C - G - G/4/Dot - C | A/4 - B/4 - +C/4 | B/2/Dot | Rest/2/Dot,
         "These last few days I've been a-fraid that_I might drift a-way.",
         F/Maj | C/Maj | D/Min | -Bf/Maj | -Bf/Maj7)
  
  val verse1Line3 =
    line((C - C)/Pickup | C - F - F/4 - D - C | C - G - G/4/Dot - C | A - B - +C/4 - A - A | B - A - G - F/4/Dot,
         "So_I've been tell-ing old sto-ries, sing-ing songs, that make me think a-bout where I come from",
         F/Maj | C/Maj | D/Min | -Bf/Maj)
  
  val verse1Line4 =
    line(C - F - F/4/Dot - C | C - G - G/4/Dot - C | A - B - +C/4/Dot - A | B/2/Dot | Rest/2/Dot,
         "That's the reas-on why I seem so far a-way to-day",
         F/Maj | C/Maj | D/Min | -Bf/Maj | -Bf/Maj7)
         
  val verse1 = verse1Line1 | verse1Line2 | verse1Line3 | verse1Line4
  
  val chorusLine1 =
    line((A - B)/Pickup | +C - +C - +C - B - B - A | A - G/4/Dot -F - G | A/Dot - A/16 - A - +C - (A -- G)/16 - F | D/2/Dot,
         "Ah but Let me tell you that I love you and I think a-bout you all- the time",
         F/Maj | C/Maj | D/Min | -Bf/Maj)
         
  val chorusLine2 =
    line(D/Dot - F/16 - F/4 - F - D | D - C - C/4 - B - A | B - A - G/4/Dot - F | F/2 - Rest/4,
         "Cal-e-don-ia you're call-ing me and- now I'm go-ing home",
         Bf/Maj | F/Maj | C/Maj | F/Maj)
         
  val chorusLine3 =
    line((C)/Pickup | +C - +C - +C - B - B - A | A - G/4/Dot -F - G | A/Dot - A/16 - A - +C - (A -- G)/16 - F | D/2/Dot,
         "But if I should become a stran-ger you know that it would make me more- than sad",
         F/Maj | C/Maj | D/Min | -Bf/Maj)
         
  val chorusLine4 =
    line(D/Dot - F/16 - F/4 - F - A | A - G - G - F - (A--G)/16 - F | F/2 - Rest/4,
         "Cal-e-don-ia been ev-ery-thing I've ev--er had",
         Bf/Maj | C/Maj | F/Maj)
         
  val chorus = chorusLine1 | chorusLine2 | chorusLine3 | chorusLine4 | line(Rest/2/Dot, "", F/Maj, true)
  
  val tune = TimeSig(3,4) /: FMaj /: Tempo(100) /: (verse1 | chorus)
  
  def apply(): Music = tune
}

object DaintyDavy extends Function0[Music]
{
  val notes = B/Dot - G/8 - E - (F -- G)/8 | A - C - F/Dot - Rest/8 | D - E/8 - F/8 - G - G | G - D - E - F |
  A/Dot - (G - F - E - D - C)/8 | F - G/8 - A/8 - B - Rest/8 - G/8 | F - (G - A - C - D - E - F)/8 | G/2 - A - Rest   
  val lyrics="Leeze me on your- cur-ly pow, Dain-ty- Da-vy Dain-ty Da-vy. Leeze me on- your- cur-ly- pow, my ain dear- Dain--ty- Da-vy."
  val tune = FMaj /: Tempo(120) /: Octave(-1) /: (notes/Lyrics(lyrics))
  
  def apply(): Music = tune
}

object JohnHardy extends Function0[Music]
{
  val lyrics1 = "John Har-dy was a desp-arate lit-tle man" 
  val lyrics2 = "He car-ried a ra-zor each day" 
  val lyrics3 = "He killed a man down in mob-ile- town" 
  val lyrics4 = "You ought'-a seen John Har-dy get a-way" 
  val lyrics5 = "You ought'-a seen John Har-dy get a-way" 
  
  val rhythm1 = N/8 - N - N - N - N - N/8 - N - N/16 - N/16 - N/Dot
  val rhythm2 = N/8 - N/8 - N/8 - N - N - N/8 - N - N/(Half+Quarter+Eighth)
  val rhythm3 = N - N/8/Dot - N/16 - N - N/8 - N/8 - N/8 - N/8 - N/8 - N/2
  val rhythm4 = N/8 - N/16 - N/16 - N - N/8 - N/8 - N - N/16 - N/16 - N/Dot
  val rhythm5 = N/8 - N/16 - N/16 - N - N/8 - N/8 - N - N/16 - N/16 - N/Dot
  
  val mNotes1 = C/Pickup - F - A - Bf - A - G - G - E - D - C 
  val mNotes2 = C - F - F - A - Bf - A - G - G 
  val mNotes3 = C - F - A - Bf - A - G - G - E - D - C  
  val mNotes4 = C - E - E - E - D - C - C - -A - -G - E  
  val mNotes5 = C - E - E - E - D - C - C - -A - -G - C  
  
  val hNotes1 = C/Pickup - A - +C - +D - +C - +C - +C - G - F - E 
  val hNotes2 = C - A - A - +C - +D - +C - +C - +E 
  val hNotes3 = A - +C - +C - +D - +C - +C - +C - G - F - E  
  val hNotes4 = F - G - G - G - G - A - A - A - A - G  
  val hNotes5 = F - G - G - G - G - F - F - F - F - E  
  
  val lNotes1 = C/Pickup - F - F - F - F - F - C - C - C - C 
  val lNotes2 = C - F - F - F - F - F - F - C 
  val lNotes3 = F - F - F - F - F - F - C - C - C - -A  
  val lNotes4 = -F - C - C - C - C - -F - -F - -F - -F - C  
  val lNotes5 = -F - C - C - C - C - -F - -F - -F - -F - -C  
  
  val mLine1 = mNotes1 / Rhythm(rhythm1) / Lyrics(lyrics1)
  val mLine2 = mNotes2 / Rhythm(rhythm2) / Lyrics(lyrics2)
  val mLine3 = mNotes3 / Rhythm(rhythm3) / Lyrics(lyrics3)
  val mLine4 = mNotes4 / Rhythm(rhythm4) / Lyrics(lyrics4)
  val mLine5 = mNotes5 / Rhythm(rhythm5) / Lyrics(lyrics5)
  
  val hLine1 = hNotes1 / Rhythm(rhythm1) / Lyrics(lyrics1)
  val hLine2 = hNotes2 / Rhythm(rhythm2) / Lyrics(lyrics2)
  val hLine3 = hNotes3 / Rhythm(rhythm3) / Lyrics(lyrics3)
  val hLine4 = hNotes4 / Rhythm(rhythm4) / Lyrics(lyrics4)
  val hLine5 = hNotes5 / Rhythm(rhythm5) / Lyrics(lyrics5)
  
  val lLine1 = lNotes1 / Rhythm(rhythm1) / Lyrics(lyrics1)
  val lLine2 = lNotes2 / Rhythm(rhythm2) / Lyrics(lyrics2)
  val lLine3 = lNotes3 / Rhythm(rhythm3) / Lyrics(lyrics3)
  val lLine4 = lNotes4 / Rhythm(rhythm4) / Lyrics(lyrics4)
  val lLine5 = lNotes5 / Rhythm(rhythm5) / Lyrics(lyrics5)
  
  val countIn = Rest * 4
  val high = Track("Tenor") /: (countIn - hLine1 - hLine2 - hLine3 - hLine4 - hLine5)
  val mid = Track("Baritone") /: (countIn - mLine1 - mLine2 - mLine3 - mLine4 - mLine5)
  val low = Track("Bass") /: (countIn - lLine1 - lLine2 - lLine3 - lLine4 - lLine5)
  
  val tune = Octave(-1) /: Tempo(80) /: CMaj /: (high & mid & low)
  
  def apply(): Music = tune
}

object Guitar1 extends Function0[Music]
{
  val s1 = new Guitar.String
  val s2 = new Guitar.String
  val tune = Instrument(Instruments.Acoustic_Guitar_Steel) /: Octave(-1) /: (C/s1 - (D -- E)/s2 - G/s2 - +C/s1/2)
  def apply(): Music = tune
}

object Guitar2 extends Function0[Music]
{
  val g = Guitar.standardTuning
  val p1 = g.pick( (5, 2), (), 4, 3, 5, 2, 4, ())
  val p2 = g.pick( 5, (), (4,2), (), 5, 3, 4, ())
  val p1c = p1 on C/Maj
  val p2c = p2 on C/Maj
  val p3 = g.pick( (6, 2), (), 4, 3, 6, 2, 4, ())
  val p4 = g.pick( 6, (), (4,2), (), 6, 3, 4, ())
  val p3g = p3 on G/Maj
  val p4g = p4 on G/Maj
  val tune = Instrument(Instruments.Acoustic_Guitar_Steel) /: Octave(-1) /: Tempo(160) /: ((p1c-p2c-p3g-p4g)/8*4)
  def apply(): Music = tune
}

object Guitar3 extends Function0[Music]
{
  val g = Guitar.standardTuning
  val st = StrumLoHi(0.04)
  val cChord = Frets(2->1, 4->2, 5->3)
  val gChord = Frets(1->3, 5->2, 6->3)
  def p1Alt(s1: Int, s2: Int) =
    g.pick( (s1, 2), (), s2, 3, s1, 2, s2, ())
  def p2Alt(s1: Int, s2: Int) =
    g.pick( s1, (), st(s2,2), (), s1, 3, s2, ())
  val p1 = p1Alt(5,4)
  val p1c = p1Alt(5,4)/cChord
  val p2c = p2Alt(5,4)/cChord
  val p3g = p1Alt(6,4)/gChord
  val p4g = p2Alt(6,4)/gChord
  val tune = Instrument(Instruments.Acoustic_Guitar_Steel) /: Octave(-1) /: Tempo(160) /: ((p1c-p2c-p3g-p4g)/8*4)
  def apply(): Music = tune
}

object Guitar4 extends Function0[Music]
{
  val g = Guitar.standardTuning
  val cChord = Frets(2->1, 4->2, 5->3)
  val gChord = Frets(1->3, 5->2, 6->3)
  val p1c = g.strum(0.02)/cChord
  val p1g = g.strum(0.02)/gChord
  val tune = Instrument(Instruments.Acoustic_Guitar_Steel) /: Octave(-1) /: Tempo(160) /: (Rest - (p1c*4 - p1g*4))
  def apply(): Music = tune
}

object Guitar5 extends Function0[Music]
{
  val g = Guitar.standardTuning
  val cChord = Frets(2->1, 4->2, 5->3)
  val gChord = Frets(1->3, 5->2, 6->3)
  val rhythm = Rhythm(N - N - (N-N)/8 - N - N - (N-N)/8 - N/2)
  val p1c = (g.strum(0.02)*9) / rhythm / (cChord/1 - gChord/1)
  val tune = Instrument(Instruments.Acoustic_Guitar_Steel) /: Octave(-1) /: Tempo(160) /: (Rest - (p1c))
  def apply(): Music = tune
}

object Guitar6 extends Function0[Music]
{
  val g = Guitar.standardTuning
  val st = StrumLoHi(0.04)
  val aMin = GuitarChord(A/Min)
  val gMaj = GuitarChord(G/Maj)
  val fMaj = GuitarChord(F/Maj)
  val eMaj = GuitarChord(E/Maj)                                                                                                                  
  val e7 = GuitarChord(E/Dom7)                                                                                                                
  val chordSequence = (aMin*2 - gMaj*2 - fMaj*2 - eMaj - e7)/1
  def p1 = g.pick( (0, 2), (), 4, 3, 0, 2, 4, ())
  val p2 = g.pick( 0, (), st(4,2), (), 0, 1, 4, ())
  val pattern = (p1 - p2)/8 *4
  val tune = Instrument(Instruments.Acoustic_Guitar_Steel) /: 
             Octave(-1) /: Tempo(160) /: Dynamics.swing(8, 0.05) /: 
             ((pattern/chordSequence)*2)
  def apply(): Music = tune
}  

object Guitar7 extends Function0[Music]
{
  val g = Guitar.standardTuning.capo(1).named("G")
  val notes = B/Dot - G/8 - E - (F -- G)/8 | A - C - F/Dot - Rest/8 | D - E/8 - F/8 - G - G | G - D - E - F |
              A/Dot - (G - F - E - D - C)/8 | F - G/8 - A/8 - B - Rest/8 - G/8 | F - (G - A - C - D - E - F)/8 | G/2 - A - Rest   
  val tune = g /: Instrument(Instruments.Acoustic_Guitar_Nylon) /: Octave(-1) /: FMaj /: Tempo(120) /: (notes)
  def apply(): Music = tune
}  


object SilentNight extends Function0[Music]
{
  val lh = (4/Dot) /: 
            ( (B & +C & +E) - (A & +C & +E & +G) | (A & +C & (Gs/8 -- Fs/4)) - F
            | (E & B) - -A | (D & Fs) - (-G & F)
            | (B & +C & +E) - (A & +C & +E) | (B & +C & +E) - (A & +C & (Gs/8 -- Fs/4))
            | (F & A)/2 - (Gf & Bf)/8 - (F & A)/8 | (Ef & G) - (Af & D)/8 -- (G & Cs)/4
            | (C & E) - -F | -E - (-As & (G/8/Dot - Fs/16 - G/8))
            | -F - -Fs | (-G & E) - (-A & Fs) 
            | (D & A & +C) - -G | (B & +C & +E) - (A & +C & +E & +G) | (-G & E) - (-G & (G/8/Dot - F/16 - D/8))
            | (B & +C & +E) - (A & +C & +E) | (B & +C & +E) - (A & +C & (Gs/8 -- Fs/4))
            | (A & +Ef & +G) - (Bf & +E & +Af) | (G & +Df & +F) - (Gs & B & +F)/8 - (G & B & +Cs)/4
            | (C & E & Bf) - -F | -E - (Bf & +Cs & +E)
            | ((+E & +F) - B - (C - D - E - F - Fs - Gs - A - B - +C - +D)/Octave(+1))/16 | (G/8/Dot - A/16 - G/8)/Octave(+1) - (Fs & +E)
            | (D & A & +C)/2/Dot | Rest/8 - (-G & F) - (G & +F)/4 | (-C & -G)/1/Dot 
            | (B & +C & +E) - (A & +C & +E) | (B & +C & +E) - (A & +C & (Gs/8 -- Fs/4))
            | E - -A | (D & A & +C)/4 - (G & B)/8 - (C & G & +C)
            )
            
  val rh = ( (G - +C)/16 - +G/4 - (Rest - +F - +E - +C - A - G)/16 
           | (E - G)/16 - D/4 - ((Ef & G)/4/Dot & (B - A - B)/8)
           | (D & G)/4/Dot - ((Df & Gf & Bf) - +Df - +Df - +Gf - +Gf  - +Bf)/16 
           | ((C & E)/4/Dot & (B - A - B)/8) - (G/4/Dot & (-Bf/8 - -A/4) & (Ef/8 - D/32 - Ef/32 - D/8/Dot)) 
           | G/8/Dot - A/16 - G/8 - (Rest - +F - +E - +C - A - G)/16 +|+ (Eighth+Sixteenth)  - A/16 - G/8 - E/4/Dot
           | {
               val v1 = ( +D/4 - +D/8 - B/4/Dot
                        | +C/4 - +C/8 - G/4/Dot
                        | A/4 - A/8 - +C/8/Dot - B/16 - A/8
                        | G/8/Dot - A/16 - G/8 - Rest/4/Dot
                        | A/4 - A/8 - +C/8/Dot - B/16 - A/8
                        | G/8/Dot - A/16 - G/8 - B/8/Dot - +C/16 - B/8
                        | +D/4 - +D/8 - Rest/4/Dot
                        | Rest/4/Dot - (Rest - F - E - C - -A - -G)/16 +|+ (Quarter+Eighth) - Rest/4/Dot
                        )
               val v2 = ( (Ef & G)/2 - ((E & Gs) - (Ef & G))/8
                        | (Df & F)/4/Dot - (C & F)/8 - (E & -B)/4
                        | ((-Bf & D) - (Ef & G))/4/Dot
                        | ((Fs & D) - (Cs & E))/4/Dot
                        | ((E & +E) - (Ef & +D))/4/Dot
                        | ((D & +C) - (E & +C))/4/Dot
                        | (F & G)/4/Dot - (A & +F)/8/Dot - (F & +D)/16 - (D & B)/8
                        | (+C - +E)/4/Dot
                        | C/8/Dot - -G/16 - -E/8 - (-A & C & G)/4/Dot
                        )
               v1 & v2
             }
           | (Rest/2/Dot)*16
           ) 
  val tune =   TimeSig(6,8) /: CMaj /: Tempo(40) /: (lh/Octave(-1) & rh/Vf)
  def apply(): Music = tune
}  


object Tunes 
{
  val tunesList = List(
    "caledonia" -> Caledonia,
    "dainty" -> DaintyDavy,
    "hardy" -> JohnHardy,
    "doc" -> DocTune,
    "birds" -> BirdsInTheSpringBaritone,
    "bugs" -> Bugs,
    "blues" -> Blues,
    "arpeggio" -> ArpeggioTest,
    "comp" -> CompTest,
    "diatonic" -> DiatonicTest,
    "ornament" -> OrnamentTest,
    "pitchbend" -> PitchBendTest,
    "dynamics" -> DynamicsTest,
    "rhythm" -> RhythmTest,
    "volume" -> VolumeTest,
    "range" -> RangeTest,
    "continuous" -> ContinuousControllerTest,
    "lyrics" -> LyricsTest,
    "guitar1" -> Guitar1,
    "guitar2" -> Guitar2,
    "guitar3" -> Guitar3,
    "guitar4" -> Guitar4,
    "guitar5" -> Guitar5,
    "guitar6" -> Guitar6,
    "guitar7" -> Guitar7,
    "beats" -> Beats,
    "silent" -> SilentNight,
    "BabyPlaysAround" -> BabyPlaysAround,
    "AllTheLoveInTheWorld" -> AllTheLoveInTheWorld)
  val tunes: Map[String,Function0[Music]] = tunesList.toMap
  
  private def instrumentSort(i1: (String,Music.Patch), i2: (String,Music.Patch)): Boolean =
  {
    if (i1._2.bank == i2._2.bank) 
      i1._2.program.compareTo(i2._2.program) < 0 
    else 
      i1._2.bank.compareTo(i2._2.bank) < 0
  }
  
  def main(args: Array[String]) {
    def usage(): Seq[(String,String)] = {
      Console.println("Tunes (play|save|strict) <tune> [<path>]")
      for (tuneName <- tunes.keys.toVector.sorted) Console.println(s"\t$tuneName")
      Nil
    }
    def display(errors: Seq[(String,String)]) = {
      for (error <- errors) Console.println(s"${error._1} : ${error._2}")
      Nil
    }
    val numArgsRequired = List("instruments" -> 0, "check" -> 0, "all" -> 0, "play" -> 1, "save" -> 2, "strict" -> 2).toMap 
    if (args.length < 1 || !numArgsRequired.contains(args(0)) || args.length < numArgsRequired(args(0)))
    {
      usage()
    }
    else args(0) match {
      case "instruments" =>
        for ((name,patch) <- Music.instruments.toIndexedSeq.sortWith(instrumentSort))
        {
          Console.println(s"$name")
        }
      case "check" =>
        if (args.length < 2)
        {
          for ((name,tune) <- tunesList) 
          {
            Console.println(name)
            display(tune().check)
          }
        }
        else
        {
          display(tunes(args(1))().check)
        }
      case "all" =>
        for ((name,tune) <- tunesList) 
        {
          Console.println(name)
          display(tune().play())
        }
      case _ =>
      {
        val tune = tunes(args(1))()
        val errors = args(0) match {
          case "play" => tune.play
          case "save" => tune.save(args(2))
          case "strict" => tune.save(args(2), true); tune.play
          case _ => usage()
        }
        display(errors)
      }
    }
  }
}