# EuterpeaScala
A Scala project loosely modelled on the Yale Haskell Euterpea* project (Haskell School of Music), 
from where I have stolen the name. 

*Euterpe was the ancient Greek muse of music - the less well known sister of Terpsichore, muse of dance.

This is certainly not a port of the Yale Euterpea from Haskell to Scala, 
but aims to achieve similar capability (and perhaps more) for Midi music only.

It aims to have a concise music notation for fast and easy transcription of music, with a very uniform syntax, 
while allowing a rich set of musical capability and performance. 

It supports :
- multiple tracks, multiple instruments, multiple channels
- both melodic and drum instruments, with a full range of note pitches and beat lengths
- lyrics
- arbitrary changes of tempo, time signature, key signature and modulation
- notation for bar boundaries which aids validation of correct transcription
- construction of chords and harmonies, including based on the current tonality, and including broken chords and arpeggios
- beat scaling for complex rhythms
- dynamics on volume and note width
- independent specification of rhythm and note sequence, either of which are easily re-used
- transposition - both chromatic and diatonic
- patterns for note dynamics (volume, width and timing) for phrase-based control of stress, swing, humanisation, etc 
- note ornaments (turns, mordants, trills etc)
- control envelopes for continuous Midi controls and pitch bending
- range limitation with octave shifts to keep notes within an instrument's range

It should be seen as a work-in-progress and also as a platform to improve my understanding of Scala's capabilities.
