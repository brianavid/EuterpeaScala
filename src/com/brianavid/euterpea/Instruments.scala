package com.brianavid.euterpea

object Instruments {
  val Acoustic_Grand_Piano = 1
  val Bright_Acoustic_Piano = 2
  val Electric_Grand_Piano = 3
  val Honky_Tonk_Piano = 4
  val Electric_Piano_1 = 5
  val Electric_Piano_2 = 6
  val Harpsichord = 7
  val Clavi = 8
  val Celesta = 9
  val Glockenspiel = 10
  val Music_Box = 11
  val Vibraphone = 12
  val Marimba = 13
  val Xylophone = 14
  val Tubular_Bells = 15
  val Dulcimer = 16
  val Drawbar_Organ = 17
  val Percussive_Organ = 18
  val Rock_Organ = 19
  val Church_Organ = 20
  val Reed_Organ = 21
  val Accordion = 22
  val Harmonica = 23
  val Tango_Accordion = 24
  val Acoustic_Guitar_Nylon = 25
  val Acoustic_Guitar_Steel = 26
  val Electric_Guitar_Jazz = 27
  val Electric_Guitar_Clean = 28
  val Electric_Guitar_Muted = 29
  val Overdriven_Guitar = 30
  val Distortion_Guitar = 31
  val Guitar_Harmonics = 32
  val Acoustic_Bass = 33
  val Electric_Bass_Finger = 34
  val Electric_Bass_Pick = 35
  val Fretless_Bass = 36
  val Slap_Bass_1 = 37
  val Slap_Bass_2 = 38
  val Synth_Bass_1 = 39
  val Synth_Bass_2 = 40
  val Violin = 41
  val Viola = 42
  val Cello = 43
  val Contrabass = 44
  val Tremolo_Strings = 45
  val Pizzicato_Strings = 46
  val Orchestral_Harp = 47
  val Timpani = 48
  val String_Ensemble_1 = 49
  val String_Ensemble_2 = 50
  val Synthstrings_1 = 51
  val Synthstrings_2 = 52
  val Choir_Aahs = 53
  val Voice_Oohs = 54
  val Synth_Voice = 55
  val Orchestra_Hit = 56
  val Trumpet = 57
  val Trombone = 58
  val Tuba = 59
  val Muted_Trumpet = 60
  val French_Horn = 61
  val Brass_Section = 62
  val Synthbrass_1 = 63
  val Synthbrass_2 = 64
  val Soprano_Sax = 65
  val Alto_Sax = 66
  val Tenor_Sax = 67
  val Baritone_Sax = 68
  val Oboe = 69
  val English_Horn = 70
  val Bassoon = 71
  val Clarinet = 72
  val Piccolo = 73
  val Flute = 74
  val Recorder = 75
  val Pan_Flute = 76
  val Blown_Bottle = 77
  val Shakuhachi = 78
  val Whistle = 79
  val Ocarina = 80
  val Lead_1_Square = 81
  val Lead_2_Sawtooth = 82
  val Lead_3_Calliope = 83
  val Lead_4_Chiff = 84
  val Lead_5_Charang = 85
  val Lead_6_Voice = 86
  val Lead_7_Fifths = 87
  val Lead_8_Bass_And_Lead = 88
  val Pad_1_New_Age = 89
  val Pad_2_Warm = 90
  val Pad_3_Polysynth = 91
  val Pad_4_Choir = 92
  val Pad_5_Bowed = 93
  val Pad_6_Metallic = 94
  val Pad_7_Halo = 95
  val Pad_8_Sweep = 96
  val Fx_1_Rain = 97
  val Fx_2_Soundtrack = 98
  val Fx_3_Crystal = 99
  val Fx_4_Atmosphere = 100
  val Fx_5_Brightness = 101
  val Fx_6_Goblins = 102
  val Fx_7_Echoes = 103
  val Fx_8_Sci_Fi = 104
  val Sitar = 105
  val Banjo = 106
  val Shamisen = 107
  val Koto = 108
  val Kalimba = 109
  val Bag_Pipe = 110
  val Fiddle = 111
  val Shanai = 112
  val Tinkle_Bell = 113
  val Agogo = 114
  val Steel_Drums = 115
  val Woodblock = 116
  val Taiko_Drum = 117
  val Melodic_Tom = 118
  val Synth_Drum = 119
  val Reverse_Cymbal = 120
  val Guitar_Fret_Noise = 121
  val Breath_Noise = 122
  val Seashore = 123
  val Bird_Tweet = 124
  val Telephone_Ring = 125
  val Helicopter = 126
  val Applause = 127
  val Gunshot = 128  
  
  val instrumentByName = Map(
    "Acoustic Grand Piano" -> 1,
    "Bright Acoustic Piano" -> 2,
    "Electric Grand Piano" -> 3,
    "Honky-tonk Piano" -> 4,
    "Electric Piano 1" -> 5,
    "Electric Piano 2" -> 6,
    "Harpsichord" -> 7,
    "Clavi" -> 8,
    "Celesta" -> 9,
    "Glockenspiel" -> 10,
    "Music Box" -> 11,
    "Vibraphone" -> 12,
    "Marimba" -> 13,
    "Xylophone" -> 14,
    "Tubular Bells" -> 15,
    "Dulcimer" -> 16,
    "Drawbar Organ" -> 17,
    "Percussive Organ" -> 18,
    "Rock Organ" -> 19,
    "Church Organ" -> 20,
    "Reed Organ" -> 21,
    "Accordion" -> 22,
    "Harmonica" -> 23,
    "Tango Accordion" -> 24,
    "Acoustic Guitar (nylon)" -> 25,
    "Acoustic Guitar (steel)" -> 26,
    "Electric Guitar (jazz)" -> 27,
    "Electric Guitar (clean)" -> 28,
    "Electric Guitar (muted)" -> 29,
    "Overdriven Guitar" -> 30,
    "Distortion Guitar" -> 31,
    "Guitar harmonics" -> 32,
    "Acoustic Bass" -> 33,
    "Electric Bass (finger)" -> 34,
    "Electric Bass (pick)" -> 35,
    "Fretless Bass" -> 36,
    "Slap Bass 1" -> 37,
    "Slap Bass 2" -> 38,
    "Synth Bass 1" -> 39,
    "Synth Bass 2" -> 40,
    "Violin" -> 41,
    "Viola" -> 42,
    "Cello" -> 43,
    "Contrabass" -> 44,
    "Tremolo Strings" -> 45,
    "Pizzicato Strings" -> 46,
    "Orchestral Harp" -> 47,
    "Timpani" -> 48,
    "String Ensemble 1" -> 49,
    "String Ensemble 2" -> 50,
    "SynthStrings 1" -> 51,
    "SynthStrings 2" -> 52,
    "Choir Aahs" -> 53,
    "Voice Oohs" -> 54,
    "Synth Voice" -> 55,
    "Orchestra Hit" -> 56,
    "Trumpet" -> 57,
    "Trombone" -> 58,
    "Tuba" -> 59,
    "Muted Trumpet" -> 60,
    "French Horn" -> 61,
    "Brass Section" -> 62,
    "SynthBrass 1" -> 63,
    "SynthBrass 2" -> 64,
    "Soprano Sax" -> 65,
    "Alto Sax" -> 66,
    "Tenor Sax" -> 67,
    "Baritone Sax" -> 68,
    "Oboe" -> 69,
    "English Horn" -> 70,
    "Bassoon" -> 71,
    "Clarinet" -> 72,
    "Piccolo" -> 73,
    "Flute" -> 74,
    "Recorder" -> 75,
    "Pan Flute" -> 76,
    "Blown Bottle" -> 77,
    "Shakuhachi" -> 78,
    "Whistle" -> 79,
    "Ocarina" -> 80,
    "Lead 1 (square)" -> 81,
    "Lead 2 (sawtooth)" -> 82,
    "Lead 3 (calliope)" -> 83,
    "Lead 4 (chiff)" -> 84,
    "Lead 5 (charang)" -> 85,
    "Lead 6 (voice)" -> 86,
    "Lead 7 (fifths)" -> 87,
    "Lead 8 (bass + lead)" -> 88,
    "Pad 1 (new age)" -> 89,
    "Pad 2 (warm)" -> 90,
    "Pad 3 (polysynth)" -> 91,
    "Pad 4 (choir)" -> 92,
    "Pad 5 (bowed)" -> 93,
    "Pad 6 (metallic)" -> 94,
    "Pad 7 (halo)" -> 95,
    "Pad 8 (sweep)" -> 96,
    "FX 1 (rain)" -> 97,
    "FX 2 (soundtrack)" -> 98,
    "FX 3 (crystal)" -> 99,
    "FX 4 (atmosphere)" -> 100,
    "FX 5 (brightness)" -> 101,
    "FX 6 (goblins)" -> 102,
    "FX 7 (echoes)" -> 103,
    "FX 8 (sci-fi)" -> 104,
    "Sitar" -> 105,
    "Banjo" -> 106,
    "Shamisen" -> 107,
    "Koto" -> 108,
    "Kalimba" -> 109,
    "Bag pipe" -> 110,
    "Fiddle" -> 111,
    "Shanai" -> 112,
    "Tinkle Bell" -> 113,
    "Agogo" -> 114,
    "Steel Drums" -> 115,
    "Woodblock" -> 116,
    "Taiko Drum" -> 117,
    "Melodic Tom" -> 118,
    "Synth Drum" -> 119,
    "Reverse Cymbal" -> 120,
    "Guitar Fret Noise" -> 121,
    "Breath Noise" -> 122,
    "Seashore" -> 123,
    "Bird Tweet" -> 124,
    "Telephone Ring" -> 125,
    "Helicopter" -> 126,
    "Applause" -> 127,
    "Gunshot" -> 128)
}