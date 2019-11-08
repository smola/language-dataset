// making real music with functions - 15-03-2016
SqrOsc s => dac.left;
SqrOsc t => dac;
SqrOsc u => dac.right;

//set gains to avoid overloading
0.4 => s.gain => t.gain => u.gain;

//functions for octave and fifth
fun float octave(float originalFreq)
{
    return 2.0*originalFreq;
}
fun float fifth(float originalFreq)
{
    return 1.5*originalFreq;
}

//main program

for (100 => float freq; freq < 500; 0.5 +=> freq)
{
    freq => s.freq;
    octave(freq) => t.freq;
    fifth(freq) => u.freq;
    <<< s.freq(), t.freq(), u.freq() >>>;
    10::ms => now;
}