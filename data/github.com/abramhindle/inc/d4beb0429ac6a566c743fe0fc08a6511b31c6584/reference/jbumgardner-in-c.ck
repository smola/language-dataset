// From http://electro-music.com/forum/viewtopic.php?t=14237&#038;sid=dbb3ada88a20169e90ee4ce45d79f10
// ChucK Performance of Terry Riley's 'In C'
//
// In this famous canon by composer Terry Riley, each performer is given the
// same set of phrases.
//
// Each performer repeats each phrase a random number of times before moving
// on to the next phrase.
//
// A single player keeps time by playing a pulse beat
// When all the players are done, the pulse continues for a while, and then stops.
//
// ChucK port 11/2006 -- Jim Bumgardner
// Original Perl Program 9/2004 -- Jim Bumgardner


// With these settings, ChucK produces about 14 minutes of music.
// A more typical 'In C' performance is 20-60 minutes.  Increase the
// min/max repetitions to achieve this...
//
240 => float bps;     // ticks/second
4 => int minRepeats;  // minimum times to repeat a phrase
10 => int maxRepeats; // maximum times to repeat a phrase

 [new StifKarp,
 new StifKarp,
 new StifKarp,
 new StifKarp,
 new StifKarp,
 new StifKarp,
 new Mandolin,
 new Mandolin,
 new Mandolin,
 new Mandolin,
 new Mandolin] @=> StkInstrument players[];

1.0/players.cap() => float mainGain;

[
  // midi pitch, vol, start-ticks, dur-ticks
  [72, 50, 0, 48],
  [72, 50, 48, 48],
  [72, 50, 96, 48],
  [72, 50, 144, 48],
  [72, 50, 192, 48],
  [72, 50, 240, 48],
  [72, 50, 288, 48],
  [72, 50, 336, 48]
] @=> int pulsePhrase[][];

// Phrases transcribed from 'In C' by Terry Riley
[
[
  // midi pitch, vol, start-ticks, dur-ticks
  [0, 63, 0, 96]
],
[
  [60, 63, 0, 6],
  [64, 63, 6, 90],
  [60, 63, 96, 6],
  [64, 63, 102, 90],
  [60, 63, 192, 6],
  [64, 63, 198, 90]
],
[
  [60, 63, 0, 6],
  [64, 63, 6, 42],
  [65, 63, 48, 48],
  [64, 63, 96, 96]
],
[
  [0, 63, 0, 48],
  [64, 63, 48, 48],
  [65, 63, 96, 48],
  [64, 63, 144, 48]
],
[
  [0, 63, 0, 48],
  [64, 63, 48, 48],
  [65, 63, 96, 48],
  [67, 63, 144, 48]
],
[
  [64, 63, 0, 48],
  [65, 63, 48, 48],
  [67, 63, 96, 48],
  [0, 63, 144, 48]
],
[
  [72, 63, 0, 768]
],
[
  [0, 63, 0, 336],
  [60, 63, 336, 24],
  [60, 63, 360, 24],
  [60, 63, 384, 48],
  [0, 63, 432, 432]
],
[
  [67, 63, 0, 576],
  [65, 63, 576, 768]
],
[
  [71, 63, 0, 24],
  [67, 63, 24, 24],
  [0, 63, 48, 336]
],
[
  [71, 48, 0, 24],
  [67, 48, 24, 24]
],
[
  [65, 48, 0, 24],
  [67, 48, 24, 24],
  [71, 48, 48, 24],
  [67, 48, 72, 24],
  [71, 48, 96, 24],
  [67, 48, 120, 24]
],
[
  [65, 63, 0, 48],
  [67, 63, 48, 48],
  [71, 63, 96, 384],
  [72, 63, 480, 96]
],
[
  [71, 63, 0, 24],
  [67, 63, 24, 72],
  [67, 63, 96, 24],
  [65, 63, 120, 24],
  [67, 63, 144, 48],
  [0, 63, 192, 72],
  [67, 63, 264, 312]
],
[
  [72, 63, 0, 384],
  [71, 63, 384, 384],
  [67, 63, 768, 384],
  [66, 63, 1152, 384]
],
[
  [67, 63, 0, 24],
  [0, 63, 24, 360]
],
[
  [67, 48, 0, 24],
  [71, 48, 24, 24],
  [72, 48, 48, 24],
  [71, 48, 72, 24]
],
[
  [71, 48, 0, 24],
  [72, 48, 24, 24],
  [71, 48, 48, 24],
  [72, 48, 72, 24],
  [71, 48, 96, 24],
  [0, 48, 120, 24]
],
[
  [64, 48, 0, 24],
  [66, 48, 24, 24],
  [64, 48, 48, 24],
  [66, 48, 72, 24],
  [64, 48, 96, 72],
  [64, 48, 168, 24]
],
[
  [0, 63, 0, 144],
  [67, 63, 144, 144]
],
[
  [64, 48, 0, 24],
  [66, 48, 24, 24],
  [64, 48, 48, 24],
  [66, 48, 72, 24],
  [55, 48, 96, 72],
  [64, 48, 168, 24],
  [66, 48, 192, 24],
  [64, 48, 216, 24],
  [66, 48, 240, 24],
  [64, 48, 264, 24]
],
[
  [66, 63, 0, 288]
],
[
  [64, 63, 0, 144],
  [64, 63, 144, 144],
  [64, 63, 288, 144],
  [64, 63, 432, 144],
  [64, 63, 576, 144],
  [66, 63, 720, 144],
  [67, 63, 864, 144],
  [69, 63, 1008, 144],
  [71, 63, 1152, 48]
],
[
  [64, 63, 0, 48],
  [66, 63, 48, 144],
  [66, 63, 192, 144],
  [66, 63, 336, 144],
  [66, 63, 480, 144],
  [66, 63, 624, 144],
  [67, 63, 768, 144],
  [69, 63, 912, 144],
  [71, 63, 1056, 96]
],
[
  [64, 63, 0, 48],
  [66, 63, 48, 48],
  [67, 63, 96, 144],
  [67, 63, 240, 144],
  [67, 63, 384, 144],
  [67, 63, 528, 144],
  [67, 63, 672, 144],
  [69, 63, 816, 144],
  [71, 63, 960, 48]
],
[
  [64, 63, 0, 48],
  [66, 63, 48, 48],
  [67, 63, 96, 48],
  [69, 63, 144, 144],
  [69, 63, 288, 144],
  [69, 63, 432, 144],
  [69, 63, 576, 144],
  [69, 63, 720, 144],
  [71, 63, 864, 144]
],
[
  [64, 63, 0, 48],
  [66, 63, 48, 48],
  [67, 63, 96, 48],
  [69, 63, 144, 48],
  [71, 63, 192, 144],
  [71, 63, 336, 144],
  [71, 63, 480, 144],
  [71, 63, 624, 144],
  [71, 63, 768, 144]
],
[
  [64, 48, 0, 24],
  [66, 48, 24, 24],
  [64, 48, 48, 24],
  [66, 48, 72, 24],
  [67, 48, 96, 48],
  [64, 48, 144, 24],
  [67, 48, 168, 24],
  [66, 48, 192, 24],
  [64, 48, 216, 24],
  [66, 48, 240, 24],
  [64, 48, 264, 24]
],
[
  [64, 48, 0, 24],
  [66, 48, 24, 24],
  [64, 48, 48, 24],
  [66, 48, 72, 24],
  [64, 48, 96, 72],
  [64, 48, 168, 24]
],
[
  [64, 63, 0, 288],
  [67, 63, 288, 288],
  [72, 63, 576, 288]
],
[
  [72, 63, 0, 576]
],
[
  [67, 48, 0, 24],
  [65, 48, 24, 24],
  [67, 48, 48, 24],
  [71, 48, 72, 24],
  [67, 48, 96, 24],
  [71, 48, 120, 24]
],
[
  [65, 48, 0, 24],
  [67, 48, 24, 24],
  [65, 48, 48, 24],
  [67, 48, 72, 24],
  [71, 48, 96, 24],
  [65, 48, 120, 312],
  [67, 48, 432, 144]
],
[
  [67, 63, 0, 24],
  [65, 63, 24, 24],
  [0, 63, 48, 48]
],
[
  [67, 48, 0, 24],
  [65, 48, 24, 24]
],
[
  [65, 48, 0, 24],
  [67, 48, 24, 24],
  [71, 48, 48, 24],
  [67, 48, 72, 24],
  [71, 48, 96, 24],
  [67, 48, 120, 24],
  [71, 48, 144, 24],
  [67, 48, 168, 24],
  [71, 48, 192, 24],
  [67, 48, 216, 24],
  [0, 48, 240, 336],
  [70, 63, 576, 96],
  [79, 63, 672, 288],
  [81, 63, 960, 48],
  [79, 63, 1008, 96],
  [83, 63, 1104, 48],
  [81, 63, 1152, 144],
  [79, 63, 1296, 48],
  [76, 63, 1344, 288],
  [79, 63, 1632, 48],
  [78, 63, 1680, 336],
  [0, 63, 2016, 240],
  [76, 63, 2256, 240],
  [77, 63, 2496, 576]
],
[
  [65, 48, 0, 24],
  [67, 48, 24, 24],
  [71, 48, 48, 24],
  [67, 48, 72, 24],
  [71, 48, 96, 24],
  [67, 48, 120, 24]
],
[
  [65, 48, 0, 24],
  [67, 48, 24, 24]
],
[
  [65, 48, 0, 24],
  [67, 48, 24, 24],
  [71, 48, 48, 24]
],
[
  [71, 48, 0, 24],
  [67, 48, 24, 24],
  [65, 48, 48, 24],
  [67, 48, 72, 24],
  [71, 48, 96, 24],
  [72, 48, 120, 24]
],
[
  [71, 48, 0, 24],
  [65, 48, 24, 24]
],
[
  [71, 48, 0, 24],
  [67, 48, 24, 24]
],
[
  [72, 63, 0, 384],
  [71, 63, 384, 384],
  [69, 63, 768, 384],
  [72, 63, 1152, 384]
],
[
  [65, 48, 0, 96],
  [64, 48, 96, 96],
  [65, 48, 192, 96],
  [64, 48, 288, 96],
  [64, 48, 384, 48],
  [64, 48, 432, 48],
  [64, 48, 480, 48],
  [65, 48, 528, 24],
  [64, 48, 552, 24]
],
[
  [77, 63, 0, 48],
  [76, 63, 48, 96],
  [76, 63, 144, 48],
  [72, 63, 192, 96]
],
[
  [74, 63, 0, 96],
  [74, 63, 96, 96],
  [67, 63, 192, 96]
],
[
  [67, 48, 0, 24],
  [74, 48, 24, 24],
  [76, 48, 48, 24],
  [74, 48, 72, 24],
  [0, 48, 96, 48],
  [67, 48, 144, 48],
  [0, 48, 192, 48],
  [67, 48, 240, 48],
  [0, 48, 288, 48],
  [67, 48, 336, 48],
  [67, 48, 384, 24],
  [74, 48, 408, 24],
  [76, 48, 432, 24],
  [74, 48, 456, 24]
],
[
  [74, 63, 0, 24],
  [76, 63, 24, 24],
  [74, 63, 48, 48]
],
[
  [67, 63, 0, 576],
  [67, 63, 576, 384],
  [65, 63, 960, 480]
],
[
  [65, 48, 0, 24],
  [67, 48, 24, 24],
  [70, 48, 48, 24],
  [67, 48, 72, 24],
  [70, 48, 96, 24],
  [67, 48, 120, 24]
],
[
  [65, 48, 0, 24],
  [67, 48, 24, 24]
],
[
  [65, 48, 0, 24],
  [67, 48, 24, 24],
  [70, 48, 48, 24]
],
[
  [67, 48, 0, 24],
  [70, 48, 24, 24]
],
[
  [70, 48, 0, 24],
  [67, 48, 24, 24]
]
] @=> int phraseList[][][];

players.cap() => int playersIn;
1 => int pulseIsPlaying;

fun int phraseLength(int ph[][])
{
   return ph[ph.cap()-1][2] + ph[ph.cap()-1][3];
}


fun   void playPhrase(int ph[][], StkInstrument voc)
{
  for (0 => int i; i < ph.cap(); ++i)
  {
   if (ph[i][0] > 0) {
     // convert midi-note to frequency
     Std.mtof(ph[i][0]) => voc.freq;
     // convert midi-vol to gain (and randomize it a bit)
     (mainGain*ph[i][1]+Std.rand2(0,9)-4)/127.0 => voc.gain;
   }
   else {
      // rest
      0 => voc.gain;
   }
   1 => voc.noteOn;
   // wait til next note...
   (ph[i][3]/bps)::second => now;
   1 => voc.noteOff;
    if (i+1 < ph.cap())
   {
      if ((ph[i+1][2] - ph[i][2]) > ph[i][3])
        ((ph[i+1][2] - (ph[i][2]+ph[i][3]))/bps)::second => now;
    }
  }    
}

// Repeat a particular phrase N times
fun void repeatPhrase(int ph[][], int n, int player)
{
  if (player >= players.cap())
  {
     <<< "Bad Player!" >>>;
  }    
   
  players[player] @=> StkInstrument voc;
  0 => voc.gain;
  20 => voc.freq;
  phraseLength(ph) => float totBeats;
  for (0 => int pp; pp < n; ++pp)
  {
    playPhrase(ph, voc);
  }      
}

// Performance by a single player
// play each phrase a random number of times
fun void doRileyPart(int player)
{
  for (0 => int p; p < phraseList.cap(); ++p)
  {
    Std.rand2(minRepeats,maxRepeats) => int nTimes;
    repeatPhrase(phraseList[p], nTimes, player);   
  }
  --playersIn; // decrement player counter, so pulse knows when to stop
}   

// Performance by the Pulse player
fun void doPulse()
{
  do {
     repeatPhrase(pulsePhrase, 1,0);
  } while (playersIn > 0);
  // Do a few more pulses to end the piece
  repeatPhrase(pulsePhrase, 8,0);
  0 => pulseIsPlaying; // Notification that we have stopped
}

// Performance by all non-pulse players
fun void doRileyParts()
{
  for (1 => int p; p < players.cap(); ++p)
    spork ~ doRileyPart(p);
}

for (0 => int i; i < players.cap(); ++i)
{
 players[i] => dac;
 0 => players[i].gain;
}

// Start the Pulse
spork ~ doPulse();

// Let him go for a bit...
(phraseLength(pulsePhrase)*4/bps)::second => now;

// Start up all the players
doRileyParts();

// Let time elapse until the pulse stops
do {
  1::second => now;
} while (pulseIsPlaying);

