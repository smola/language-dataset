SinOsc sin => dac;

while (true) {

220*2 => sin.freq;

1::second => now;

220/2 => sin.freq;

1::second => now;
}

/*

// background; how many coders? how many musicians? electronic musicians? any other live coders?
//how to use audicle

add shred
replace
remove
VM monitor
console monitor

1. SinOsc sin => dac;

//commenting
//error message

2. add 1::second => now;

3. add 220 => sin.freq;

4. add .5 => sin.gain;

5. add interval with new frequency

6. create loop

7. run more than one shred --look at VM window to see

8. use dur 

9. use time
*/

