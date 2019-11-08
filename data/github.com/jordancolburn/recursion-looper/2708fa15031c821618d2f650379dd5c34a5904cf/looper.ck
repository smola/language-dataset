class Loop {
  LiSa loop;
  0 => int status;
  

  fun void init(Gain input) {
    60::second => loop.duration;
    0 => loop.play;
    1 => loop.loop;
    1 => loop.loopRec;
    1 => loop.maxVoices;

    input => loop => dac;
  }

  fun void record() {
  }

  fun void play() {
  }

  fun void stop() {
  }

  fun void toggleOverdub() {
  }

  fun void clear() {
  }

}

.07::second => dur buttonLatency;
.16::second => dur buttonLatencyPlayback;
2024::samp => dur audioLatency;


adc => Gain g1 => LiSa buttonLatencyLoop;
.85 => g1.gain;

60::second => buttonLatencyLoop.duration; 
1 => buttonLatencyLoop.loop; 
1 => buttonLatencyLoop.loopRec; 
0 => buttonLatencyLoop.feedback;
1 => buttonLatencyLoop.record;
1 => buttonLatencyLoop.play;
0 => buttonLatencyLoop.playPos;
buttonLatencyLoop.playPos() + buttonLatency => buttonLatencyLoop.recPos;

buttonLatencyLoop => Envelope blenv1 => LiSa loop1; 
buttonLatencyLoop => Envelope blenv2 => LiSa loop2; 
buttonLatencyLoop => Envelope blenv3 => LiSa loop3; 
buttonLatencyLoop => Envelope blenv4 => LiSa loop4; 
buttonLatencyLoop => Envelope blenv5 => LiSa loop5; 

adc => g1 => Envelope alenv1 => loop1 => Envelope postenv1 => dac; 
adc => g1 => Envelope alenv2 => loop2 => Envelope postenv2 => dac; 
adc => g1 => Envelope alenv3 => loop3 => Envelope postenv3 => dac; 
adc => g1 => Envelope alenv4 => loop4 => Envelope postenv4 => dac; 
adc => g1 => Envelope alenv5 => loop5 => Envelope postenv5 => dac; 

[loop1, loop2, loop3, loop4, loop5] @=> LiSa loops[];
[blenv1, blenv2, blenv3, blenv4, blenv5] @=> Envelope blenvs[];
[alenv1, alenv2, alenv3, alenv4, alenv5] @=> Envelope alenvs[];
[postenv1, postenv2, postenv3, postenv4, postenv5] @=> Envelope postenvs[];

0::second => dur baseDuration;
now => time loopStartTime;

1 => int loopsStopped;
0 => int buttonWait;
now => time loopRealtime1;
now => time loopRealtime2;
now => time loopRealtime3;
now => time loopRealtime4;
now => time loopRealtime5;
[loopRealtime1, loopRealtime2, loopRealtime3, loopRealtime4, loopRealtime5] @=> time loopRT[];

for( 0 => int i; i < loops.cap(); i++ )
{
	30::second => loops[i].duration; 
	1 => loops[i].loop; 
	1 => loops[i].loopRec; 
	1 => loops[i].feedback;
}


//define raspberry pi pin numbers for each of the 5 loops
[19, 16, 13, 20, 12] @=> int loopOSCConfig[];

[-1, -1, -1, -1, -1] @=> int loopState[];

0 => int editMode;

//setup fft to analyze input for led display in edit mode
//adc => FFT fft =^ RMS rms => blackhole;
//1024 => fft.size;
//Windowing.hann(1024) => fft.window;
//spork ~ setEdit();

//Create an OscSend object:
OscSend xmit;
//Set the host and port of this object:
xmit.setHost("localhost", 7111);

// create our OSC receiver
OscRecv recv;
7110 => recv.port;
recv.listen();

// create an address in the receiver, store in new variable
recv.event( "/button/realtime_tap, i" ) @=> OscEvent oeRealtime;
recv.event( "/button/tap, i i" ) @=> OscEvent oeTap;
recv.event( "/button/tap_and_hold, i i" ) @=> OscEvent oeHold;
spork ~ oscRealtimeThread();
spork ~ oscTapThread();
oscHoldThread();

updateStatus();

fun void ControlLoop( int tapCount, int hold, LiSa loop, int loopNum )
{
	if (hold == 0) {
		//start recording
		if (tapCount == 1 && loopState[loopNum] == -1) { 
			if (baseDuration == 0::second){
				now => loopStartTime;
				<<<"starting loop at time", loopStartTime>>>;
				1 => loop.record;
				//fade in audio
				.005::second => blenvs[loopNum].duration;
				blenvs[loopNum].keyOn();
				0 => loopsStopped;
			}
			else{
				waitUntilDownbeat();
				audioLatency => now;
				//fade in audio
				1 => loop.record;
				.01::second => alenvs[loopNum].duration;
				alenvs[loopNum].keyOn();
			}
			<<<"recording ", loopNum>>>; 
			-2 => loopState[loopNum];
			if (loopNum < 3) {
				stopOtherLoops(loopNum);
			}
		}
		//play and stop recording 
		else if (tapCount == 1 && (loopState[loopNum] == -2 || loopState[loopNum] == 0)) { 
			if (baseDuration == 0::second){
				<<<"First Loop">>>;
				loop.recPos() => loop.loopEnd; 
				loop.recPos() => loop.loopEndRec;
				buttonLatencyPlayback => loop.playPos; 
				0::second => loop.recPos; 
				1 => loop.play;
				.02::second => postenvs[loopNum].duration;
				postenvs[loopNum].keyOn();
				1 => loopState[loopNum];
				now - loopStartTime => baseDuration;
				now - buttonLatencyPlayback => loopStartTime;
				<<<"Base Duration Set", baseDuration>>>;
				//cross fade out audio
				.005::second => blenvs[loopNum].duration;
				blenvs[loopNum].keyOff();
				.005::second => now;
				0 => loop.record; 
			}
			else{
				waitUntilDownbeat();
				loop.loopStart() => loop.playPos; 
				1 => loop.play;
				if (loopState[loopNum] == -2){
					audioLatency => now;
				}
				1 => loopState[loopNum];
				loop.recPos() => loop.loopEnd; 
				loop.recPos() => loop.loopEndRec;
				.01::second => postenvs[loopNum].duration;
				postenvs[loopNum].keyOn();
				//cross fade out audio
				.1::second => alenvs[loopNum].duration;
				alenvs[loopNum].keyOff();
				.1::second => now;
				0 => loop.record; 
			}
			//The first three loops are in groups, only one can play at a time
			if (loopNum < 3) {
				stopOtherLoops(loopNum);
			}
			<<<"looping ", loopNum>>>; 
		}

		//stop playback instantly
		else if (tapCount == 1 && loopState[loopNum] == 1) { 
			<<<"stopped ", loopNum>>>; 
			stopLoop(loopNum, loop);
		} 
		//stop playback after next quantization duration
		else if (tapCount == 2 && loopState[loopNum] == 1) { 
			waitUntilDownbeat();
			<<<"stopped ", loopNum>>>; 
			stopLoop(loopNum, loop);
		} 
	}
	else{
		if (tapCount == 1 && loopState[loopNum] < 1){
			clearLoop(loopNum, loop);
		}
		else if (tapCount == 2){
			3::second => postenvs[loopNum].duration;
			postenvs[loopNum].keyOff();
			3::second => now;
			stopLoop(loopNum, loop);
		}
		else if (tapCount == 3){
			clearAllLoops();
			1 => buttonWait;
			1::second => now;	
			0 => buttonWait;
		}
	}

	// toggle overdub off 
	if (tapCount == 1 && hold == 0 &&loopState[loopNum] == 2) {
		//fade out audio
		.1::second => alenvs[loopNum].duration;
		alenvs[loopNum].keyOff();
		.1::second => now;
		//turn off overdub
		0 => loop.record;
		1 => loopState[loopNum];
		<<<"overdubbing off ", loopNum>>>;
	}
	//turn overdub on
	else if (tapCount == 1 && hold == 1 && loopState[loopNum] == 1) { 
			loop.playPos() - audioLatency => loop.recPos;
			1 => loop.record; 
			2 => loopState[loopNum];
			<<<"overdubbing ", loopNum>>>;
			//fade in audio
			.1::second => alenvs[loopNum].duration;
			alenvs[loopNum].keyOn();
	}
	updateStatus();
}


fun void stopOtherLoops(int loopNum) {
	for( 0 => int i; i <= 2; i++ ) {
		if (i != loopNum) {
			if (loopState[i] > 0) {
				stopLoop(i, loops[i]);
			}
			else if (loopState[i] == -2) {
				// ControlLoop(1, 0, loops[i], i);
			}
		}
	}
}

fun void stopLoop(int loopNum, LiSa loop){
	.05::second => postenvs[loopNum].duration;
	.05::second => alenvs[loopNum].duration;
	.05::second => blenvs[loopNum].duration;
	postenvs[loopNum].keyOff();
	alenvs[loopNum].keyOff();
	blenvs[loopNum].keyOff();
	.05::second => now;
	0 => loop.record; 
	0 => loop.play; 
	loop.loopEnd() => loop.playPos; 
	loop.loopEnd() => loop.recPos; 
	0 => loopState[loopNum];
	0 => int playingLoops;
	for( 0 => int i; i < loops.cap(); i++ ) {
		if (loopState[i] > 0){
			loopState[i] + playingLoops => playingLoops;
		}
	}
	if (playingLoops <= 0){
		//all loops stopped
		1 => loopsStopped;
	}
}

fun void waitUntilDownbeat() {
	if (loopsStopped == 0) {
		now - loopStartTime => dur elapsed;
		elapsed % baseDuration => dur remainder;
		baseDuration - remainder => dur durUntilDownbeat;
		durUntilDownbeat => now;
	}
	else {
		now => loopStartTime;
		0 => loopsStopped;
	}
}

fun void clearAllLoops() {
	for( 0 => int i; i < loops.cap(); i++ ) {
		clearLoop(i, loops[i]);
	}
	0::second => baseDuration;	
}

fun void clearLoop(int loopNum, LiSa loop) {
	stopLoop(loopNum, loop);
	.2::second => now;
	0 => loop.play;
	loop.clear();
	//60::second => loop.duration;
	0::second => loop.recPos;
	0::second => loop.playPos;
	60::second => loop.loopEndRec;
	60::second => loop.loopEnd;
	1 => loop.loop; 
	1 => loop.loopRec; 
	1 => loop.feedback;
	-1 => loopState[loopNum];

	// Reset base duration if all loops are cleared
	0 => int nonEmptyLoops;
	for( 0 => int i; i < loops.cap(); i++ ) {
		if (loopState[i] != -1) {
			nonEmptyLoops + 1 => nonEmptyLoops;
		}
	} 
	if (nonEmptyLoops == 0) {
		0::second => baseDuration;	
	}
}

fun void oscRealtimeThread()
{
    // infinite event loop
    while( true )
    {
	    // wait for event to arrive
	    oeRealtime => now;

	    // grab the next message from the queue. 
	    while ( oeRealtime.nextMsg() != 0 )
	    { 
		    // getFloat fetches the expected float (as indicated by "f")
		    oeRealtime.getInt() => int buttonIn;
		    for( 0 => int i; i < loops.cap(); i++ ) {
			if (loopOSCConfig[i] == buttonIn) {
			    if (loopState[i] <= 0 && buttonWait == 0) {
			        now => loopRT[i];
			        spork ~ ControlLoop(1, 0, loops[i], i);
				1 => buttonWait;
				.25::second => now;
				0 => buttonWait;
			    }
			}
		    }
	    }
    }
}

fun void oscTapThread()
{
    // infinite event loop
    while( true )
    {
	    // wait for event to arrive
	    oeTap => now;

	    // grab the next message from the queue. 
	    while ( oeTap.nextMsg() != 0 )
	    { 
		    oeTap.getInt() => int buttonIn;
		    oeTap.getInt() => int tapCount;
		    for( 0 => int i; i < loops.cap(); i++ ) {
			if (loopOSCConfig[i] == buttonIn) {
			    if (loopState[i] > 0 && now - loopRT[i] > .5::second) {
		                spork ~ ControlLoop(tapCount, 0, loops[i], i);
			    }
			}
		    }
	    }
    }
}

fun void oscHoldThread()
{
    // infinite event loop
    while( true )
    {
	    oeHold => now;

	    while ( oeHold.nextMsg() != 0 )
	    { 
		    oeHold.getInt() => int buttonIn;
		    oeHold.getInt() => int tapCount;
		    for( 0 => int i; i < loops.cap(); i++ ) {
			if (loopOSCConfig[i] == buttonIn) {
			    spork ~ ControlLoop(tapCount, 1, loops[i], i);
			}
		    }
	    }
    }
}

fun void updateStatus(){
	if(editMode != 1) {
		xmit.startMsg("/status", "i i i i i");
		for( 0 => int i; i < loopState.cap(); i++ ) {
			loopState[i] => xmit.addInt;
		}
	}
}

fun void setEdit(){
	while(false) {
		if(editMode == 1) {
			//xmit.startMsg("/edit f");
			// upchuck: take fft then rms
			//rms.upchuck() @=> UAnaBlob blob;
			//<<< blob.fval(0) >>>;
			//xmit.addFloat(blob.fval(0));
			//xmit.addFloat(blob.fval(0));
		}
		// advance time
		//fft.size()::samp => now;
	}
}
