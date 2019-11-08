// oscChuck.ck
// a very basic example of communication from/to Processing

//Create all of the oscillators and modulators
SinOsc soprano => Gain s => dac;
SinOsc alto => Gain a => dac;
SinOsc tenor => Gain t => dac;
SinOsc bass => Gain b => dac;
SinOsc rumble => Gain r => dac;
SinOsc drone => Gain d =>  dac;
Modulate mod => d;
Modulate smod => s;
Modulate amod => a;
Modulate tmod => t;
Modulate bmod => b;

//Make all of the chord oscillators silent
0 => soprano.freq;
0 =>alto.freq;
0 => tenor.freq;
0 => bass.freq;

//Establish a low rumble in the background
98.0 => rumble.freq;
.125 => rumble.gain;
.5 => drone.gain;
1 => soprano.gain;
1 => alto.gain;
1 => tenor.gain;
1 => bass.gain;

//Set oscillators to multiply all inputs
3 => d.op;
3 => s.op;
3 => a.op;
3 => t.op;
3 => b.op;

//Set mod attributes for all modulators
mod.vibratoRate(.0625);
mod.vibratoGain(.5);
mod.randomGain(.6);

//Set the random gain for the chord tones lower
smod.vibratoRate(.0625);
smod.vibratoGain(.5);
smod.randomGain(.4);

amod.vibratoRate(.0625);
amod.vibratoGain(.5);
amod.randomGain(.4);

tmod.vibratoRate(.0625);
tmod.vibratoGain(.5);
tmod.randomGain(.4);

bmod.vibratoRate(.0625);
bmod.vibratoGain(.5);
bmod.randomGain(.4);

//Array of drone notes
[587.33, 493.88, 440.0, 293.66] @=> float notes[];
//[293.66, 246.94, 220.0, 146.83] @=> float notes[];

//2d array of chord tones
[[293.66, 246.94, 196.00, 146.83],
[123.47, 174.61, 174.61, 246.94],
[138.59, 196.0, 196.0, 277.18],
[146.83, 146.83, 196.0, 369.99]] @=> float chords[][];

//array to hold one chord
float chord[4];

0 => int count;

// OSC sender
OscSend xmit;
"localhost" => string hostname;
6450 => int sendport;
xmit.setHost( hostname, sendport );

// OSC receiver
OscRecv recv;
6449 => recv.port;

// listening to receiver port
recv.listen();
// expected event "/hello", tag type int
recv.event("/hello, iii") @=> OscEvent @ evtHello;
// concurrent shred
spork ~ listenHello();

0 => int cnt;  // a counter just for fun
0 => int chordPlaying;

// main loop
while( true )
{
    // send counter as float every 1 second
    xmit.startMsg( "/count", "f" );
    cnt++ => xmit.addFloat; 
    1::second => now;
}

// "/hello" listener
fun void listenHello()
{
    while(true) {
        <<< "In Loop" >>>;
        evtHello => now;

        // get message if any
        if ( evtHello.nextMsg() != 0 )  
        { 
            evtHello.getInt() => int mesg;
            evtHello.getInt() => int sec;
            evtHello.getInt() => int msec;
            <<< mesg >>>;
            if (mesg == 1 && chordPlaying == 0){
                1 => chordPlaying;
                chords[Math.random2(0,3)] @=> chord;
                chord[3] => soprano.freq;
                chord[2] =>alto.freq;
                chord[1] => tenor.freq;
                chord[0] => bass.freq;
                <<< "Chord Played" >>>;
            }
            else if (mesg == 0 && sec == 0 && msec <= 50){
                0 => chordPlaying;
                notes[Math.random2(0,3)] => drone.freq;
                0 => soprano.freq;
                0 =>alto.freq;
                0 => tenor.freq;
                0 => bass.freq;
            }
            else{
                <<< "Nothing happened" >>>;
            }
        }

    }
}

