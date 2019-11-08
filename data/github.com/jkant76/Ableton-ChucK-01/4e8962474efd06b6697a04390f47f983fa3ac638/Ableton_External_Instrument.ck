// Ableton External Instrument by jkant
// jkant.blogspot.com
// check out this video demo: http://youtu.be/a7hk31zDIkY
// Using ChucK as an External Instrument in Ableton Live
// Setup: 
// MIDI IAC Driver Bus to receive MIDI messages from Ableton Live in ChucK
// Soundflower Driver (2ch) to receive audio from ChucK in Ableton Live
// Setup Audio Output to Soundflower 2ch in miniAudicle Preferences
// Set Soundflower monitoring to none in Soundlowerbed
// Setup External Instrument in Ableton Live, midi to IAC, audio from Soundflower 2ch

// IMPORTANT! Spork synth1.ck class BEFORE running this one!
synth1 s;

// MIDI in setup
MidiIn min;
MidiMsg msg;

// MIDI Port
// use Device Browser to check your loopback (IAC) number port
0 => int port;

// open the port
if ( !min.open(port) )
{
    <<< "Error: MIDI port did not open on port:", port >>>;
    me.exit(); 
}

// global variables
int synthNote, synthVelocity;
Event keyPress;

fun void playSynth()
{
    while(1)
    {
        keyPress => now;
        s.setReverb(.02);
        s.setFilter(Math.random2f(400.0,1000.0), Math.random2f(1.0, 10.0));
        s.setPan(Math.random2f(-1.0,1.0));
        s.playNote(synthNote);
        s.setGain(synthVelocity/127.0);
    }
}

spork ~ playSynth();

// loop
while(1)
{
    min => now; // wait for MIDI in Event
    
    while ( min.recv(msg) )
    {
        // print midi data
        <<< msg.data1, msg.data2, msg.data3 >>>;

        // parse MIDI messages
        if (msg.data1 == 144)
        {
            msg.data2 => synthNote;
            msg.data3 => synthVelocity;
            keyPress.broadcast();
        }
        else if (msg.data1 == 128)
        {
            0 => synthVelocity;
            keyPress.broadcast();
        }
    }
}
