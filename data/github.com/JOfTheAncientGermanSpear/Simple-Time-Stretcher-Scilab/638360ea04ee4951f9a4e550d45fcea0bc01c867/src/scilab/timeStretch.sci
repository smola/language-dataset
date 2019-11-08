function timeStretchedSignal = timeStretch(timeSignal, timeStretchFactor, ftLength)
// timeStretchedSignal = timeStretch(timeSignal, timeStretchFactor, ftLength)  Time-stretch a signal to timeStretchFactor times longer with phase vocoder
//      timeSignal is an input sound. ftLength is the FT size, defaults to 1024.  
//      Calculate the 25%-overlapped STFT, squeeze it by a factor of 1/timeStretchFactor, 
//      inverse spegram.
// as timeStretchFactor factor increases, resultant signal length increases
// also as timeStretchFactor increases, the sound has to be played at a higher Fs to be same time length
// hence the sound will be at a higher pitch even though it will be the same time-length
// was originally Dan Ellis' pvoc.m from http://www.ee.columbia.edu/~dpwe/resources/matlab/pvoc/

[numOutputs, numInputs] = argn(0);
if numInputs < 3
  ftLength = 1024;
end

// With hann windowing on both input and output, 
// we need 25% window overlap for smooth reconstruction
hop = ftLength/4;
// Effect of hanns at both ends is a cumulated cos^2 window (for
// pitchChange = 1 anyway); need to scale magnitudes by 2/3 for
// identity input/output
//scf = 2/3;
// 2011-02-07: this factor is now included in istft.m
scaleFactor = 1.0;

// Calculate the basic STFT, magnitude scaled
origSTFT = scaleFactor * stft(timeSignal', ftLength, ftLength, hop);

// Calculate the new timebase samples
[rows, numberOfFTs] = size(origSTFT);
t = 0:1/timeStretchFactor:(numberOfFTs-2);
// Have to stay two cols off end because (a) counting from zero, and 
// (b) need col n AND col n+1 to interpolate

// Generate the new spectrogram
shiftedSTFT = stftInterpolate(origSTFT, t, hop);

// Invert to a waveform
timeStretchedSignal = istft(shiftedSTFT, ftLength, hop)';

endfunction