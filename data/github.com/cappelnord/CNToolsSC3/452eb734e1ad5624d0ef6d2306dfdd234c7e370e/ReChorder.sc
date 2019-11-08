/*
	ReChorder
	(c) 2009 by Patrick Borgeat <patrick@borgeat.de>
	http://www.cappel-nord.de
	
	Part of CNToolsSC3
	http://github.com/cappelnord/CNToolsSC3
*/

ReChorder : Object
{
	var <>server;
	var <>bufferLength = 88200;
	
	var <>in = 0;
	var <>out = 0;
	var <>attack = 3;
	var <>release = 1;
	var <>rate = 1;
	var <>amp = 0.7;
	var <>pan = 0;
	var <>lagTime = 1;
	
	var <buffers;
	var <synths;
	var <recordSynth;
	
	*new {|server|
		^super.new.init(server);
	}
	
	init {|server|
	
		this.server = server;

		buffers = List.new;
		synths = List.new;

	}
	
	record {
		var newBuffer;
		if(recordSynth == nil, {
			newBuffer = Buffer.alloc(server, bufferLength);
			buffers.add(newBuffer);
			recordSynth = Synth.new(\grainReChorderRecord, [\in, in, \buffer, newBuffer]);
		},{
			"GrainReChorder is recording!".postln;
		});
	}
	
	stopRecording {
		if(recordSynth != nil, {
			recordSynth.release;
			recordSynth = nil;
		});
	}
	
	quick { // and dirrrrty ...	
		this.record;
		{
			this.stopRecording;
			this.play;
		}.defer(bufferLength * 2 / server.sampleRate);
	}
	
	play {
		synths.add(Synth.new(\grainReChorderPlay, [\out, out, \attack, attack, \release, release, \rate, rate, \amp, amp, \pan, pan, \buffer, buffers.last, \lagTime, lagTime]));
	}
	
	*storeSynthDefs {|server|
		
		// Record SynthDef
		SynthDef(\grainReChorderRecord, {|in = 0, buffer, gate = 1|
		
			var inSig = SoundIn.ar(in);
			var env = Env.new([0,1,0],[0.0001,0.15], releaseNode: 1);
			var recEnv = EnvGen.kr(env, gate, doneAction:2);
			var preEnv = recEnv * -1 + 1;
	
			RecordBuf.ar(inSig, buffer, recLevel: recEnv, preLevel: preEnv);		
		}).load(server);
		
		// Normal Playback SynthDef
		SynthDef(\grainReChorderPlay, {|out = 0 , pan, buffer, amp = 0.1, attack = 3, gate = 1, release = 1, rate = 1, lagTime = 2|
			
			var env = EnvGen.kr(Env([0,1,0],[attack, release], releaseNode: 1), gate, doneAction:2);
			var sig = PlayBuf.ar(1,buffer, rate.lag(lagTime), loop:1) * env;
			
			Out.ar(0, Pan2.ar(sig, pan, amp));
		
		}).load(server);

	}
	
	freeAll {
		this.free;
	}
	
	free {
	
		var buffersToDelete;
	
		synths.do{|item|
			item.release;
		};
		
		buffersToDelete = buffers.copy;
		
		synths = List.new;
		buffers = List.new;
		
		{
			buffersToDelete.do{|item|
				item.free;
				buffersToDelete.remove(item);
			};
		}.defer(release * 1.2); // some security space
	}
	
	set {|symbol, value|
		synths.do{|i|
			i.set(symbol,value);
		};
	}
	
	freeFirst {|releaseBuffer = true|
	
		var bufferToFree, synthToRelease;

		
		synthToRelease = synths.first;
		synthToRelease.release;
		
		synths.remove(synthToRelease);
		
		if(releaseBuffer,{
			bufferToFree = buffers.last;
			buffers.remove(bufferToFree);
			
			{
				bufferToFree.free;
			}.defer(release * 1.2); // some security space
		});
		
	}
	
	freeLast {|releaseBuffer = true| // copy of freeFirst
	
		var bufferToFree, synthToRelease;
		
		synthToRelease = synths.last;
		synthToRelease.release;
		
		synths.remove(synthToRelease);
		
		if(releaseBuffer,{
			bufferToFree = buffers.last;
			buffers.remove(bufferToFree);
			
			{
				bufferToFree.free;
			}.defer(release * 1.2); // some security space
		});
	}
	
	// maybe it would make more sense to use a freeSynth and freeBuffer method, but it works nice that way at the moment.
}

GrainReChorder : ReChorder
{
	*storeSynthDefs {|server|
		super.storeSynthDefs(server);
		
		// Grainy Playback SynthDef
		SynthDef(\grainReChorderGrain, {|out = 0, pan, buffer, amp =1, attack = 3, gate = 1, release = 1, rate = 1, freq = 30, overlap = 7.3, headFreq = 1, lagTime = 2|
			var env = EnvGen.kr(Env([0,1,0],[attack, release], releaseNode: 1), gate, doneAction:2);
			var sig = TGrains.ar(2, Impulse.ar(freq),buffer, rate.lag(lagTime),LFSaw.kr(headFreq, mul:0.5, add:0.5) , (1/freq) * overlap, pan, amp, 4) * env;
			Out.ar(0,sig);	
		}).load(server);
	}
	
		play {
		synths.add(Synth.new(\grainReChorderGrain, [\out, out, \attack, attack, \release, release, \rate, rate, \amp, amp, \pan, pan, \buffer, buffers.last, \lagTime, lagTime]));
	}
}

