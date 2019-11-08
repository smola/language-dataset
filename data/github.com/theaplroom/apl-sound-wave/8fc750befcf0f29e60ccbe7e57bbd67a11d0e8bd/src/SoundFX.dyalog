:Namespace SoundFX
    ⎕IO←0

      AM←{
        ⍝ Amplitude modulator
        ⍝ rate = modulation frequency in Hz.
        ⍝ phase = relative phase of the signal in degrees
        ⍝ depth = depth of modulation [0,1]
        ⍝ sr    = sample rate in samples per second
        ⍝ ⍵ ←→ signal
        ⍝ ⍺ ←→ rate phase depth sr
        ⍝ ← ←→ signal
          ⎕IO←0
          rate phase depth sr←⍺
          m←1+depth×Sine rate phase((≢⍵)÷sr)sr
          Normalize m×[0]⍵
      }

      AR←{
        ⍝ ⍵ ←→ signal
        ⍝ ⍺ ←→ at rt type sr
        ⍝      at = attack time in seconds
        ⍝      rt = release time in seconds
        ⍝      k  = 0 = linear, 0< exponential
        ⍝ ← ←→ signal
          at rt k sr←⍺
          y←1 rt k sr Fade ⍵
          0 at k sr Fade y
      }

      Autopan←{
        ⍝ Pans the input signal back and forth between the left and right channels.
        ⍝ ⍵ ←→ sig
        ⍝ ⍺ ←→ rate phase depth sr
        ⍝ ← ←→ (samples)
        ⍝ sig   = input signal (samples)
        ⍝ rate  = LFO frequency in Hz.
        ⍝ phase = relative phase angle in degrees between channels
        ⍝ depth = depth of modulation index (normally in the range [0,1])
        ⍝ sr    = sample rate in samples per second
          rate phase depth sr←⍺
          y←⍪⍺ AM ⍵
          y,rate 0 depth sr AM ⍵
      }

      Crossfade←{
        ⍝ Performs a linear crossfade between two vectors of samples.
        ⍝ The vectors must be of the same length.
        ⍝ ⍺ ←→ overlap sr
        ⍝ ⍵ ←→ a b = input signals (samples)
        ⍝ ← ←→ samples
        ⍝ overlap = duration of crossfade in seconds
        ⍝ sr      = sample rate in samples per second
          overlap sr←⍺
          a b←⍵
          ovlen←⌊/(≢¨a b),⌊overlap×sr
          tot←(+/≢¨a b)-ovlen
          a1←tot↑1 overlap 0 sr Fade a
          b1←(-tot)↑0 overlap 0 sr Fade b
          Normalize a1+b1
      }

      DTMF←{
        ⍝ ⍵ ←→ text vector of keys
        ⍝ ⍺ ←→ sr = sample rate Hz
          ⎕IO←0
          ⍺←8192
          rows←Sine¨697 770 852 941,¨⊂0 0.3 ⍺
          cols←Sine¨1209 1336 1477 1633,¨⊂0 0.3 ⍺
          tones←↑,rows∘.+cols
          keys←'123A456B789C*0#D'⍳⍵
          sig←,(⌈⍺×0.5)↑[1](⊂keys)⌷tones
          Normalize sig
      }

      Envelope←{
        ⍝ Generates a multi-segment linear envelope,
        ⍝ which can be applied to a signal with vca.
        ⍝   amp = vector containing the amplitudes at the end of each segment
        ⍝         (starting amplitude is always 0, not included in amp)
        ⍝   dur = vector containing the durations of the segments in seconds
        ⍝   sr  = sample rate in samples per second
        ⍝
        ⍝   ⍺ ←→ amp dur
        ⍝   ⍵ ←→ sr
        ⍝   ← ←→ envelope wave
          ⎕IO←0
          amp dur←⍺
          sr←⍵
          (¯2-/0,amp){+\⍵/⍺÷⍵}⌊dur×sr
      }

      Fade←{
        ⍝ ⍵   ←→ signal
        ⍝ ⍺   ←→ dir dur k sr
        ⍝ dir ←→ 0 = in, 1 = out
        ⍝ dur ←→ duration in seconds
        ⍝ k   ←→ 0 = linear, 0< exponential
        ⍝ ← ←→ signal
          ⍺←0 1
          dir dur k sr←⍺
          sig←⍵
          n←⌊dur×sr
          p←(i←⍳n⌊c←≢sig)÷n
          p←(2*∘-k∘×∘⌽)⍣(k>0)⊢p
          i←(c-1)-⍣dir⊢i
          ((⊂i)⌷sig)←((⊂i)⌷sig)×[0]p
          sig
      }

      FLT←{
        ⍝ A resonant bandpass filter
        ⍝ ⍵ ←→ sig = input signal
        ⍝ ⍺ ←→ F D sr
        ⍝ ← ←→ filtered signal
        ⍝      F  = the center frequency of the filter in Hz.
        ⍝      D  = the half-bandwidth of the filter in Hz.
        ⍝      sr = the sample rate in samples per second (global)
          F D sr←⍺
          I2←-2×(*○¯2×D÷sr)×2○○4×F÷sr
          I3←*○¯4×D÷sr
          y←(,1)(1 I2 I3)#.DSP.Filter ⍵
          Normalize y
      }

    :Section FM_Instruments

      FM_Instr←{
        ⍝ Implements an FM instrument. The amp argument specifies the amplitudes of
        ⍝ a single multi-segment linear envelope, which is applied both to output
        ⍝ amplitude and modulation index. The modulating frequency is obtained
        ⍝ by c:m ratio. See FM_* instruments below for examples.
        ⍝ ⍵ ←→  cf cm mi amp dur sr
        ⍝ ← ←→  signal
        ⍝   cf  = carrier frequency in Hz.
        ⍝   cm  = c:m ratio
        ⍝   mi  = modulation index
        ⍝   amp = array of ending amplitudes of envelope segments (start is always 0)
        ⍝   dur = array of segment durations in seconds
        ⍝   sr  = sample rate in samples per second
          ⎕IO←0
          cf cm mi amp dur sr←⍵
          mf←cf÷cm
          linen←amp dur Envelope sr
          t←(⍳≢linen)÷sr
          c←○2×cf×t
          m←mi×linen×2○○2×mf×t
          linen×1○c+m
      }

      FM_Basson←{
      ⍝ ⍵ ←→ freq phs dur sr
          ⎕IO←0
          freq phs dur sr←⍵,(≢⍵)↓440 0 1 44100
          FM_Instr freq 5 1.5(0.25 0.5 1 1 0.33 0.16 0)(0.033 0.033 0.033 dur 0.02 0.02 0.02)sr
      }

      FM_Bell←{
        ⍝ ⍵ ←→ freq phs dur sr
          ⎕IO←0
          freq phs dur sr←⍵,(≢⍵)↓440 0 1 44100
          FM_Instr freq 0.7143 10(1 0.5 0.25 0.125 0.06)(0.01 3 3 3 3)sr
      }

      FM_Brass←{
      ⍝ ⍵ ←→ freq phs dur sr
          ⎕IO←0
          freq phs dur sr←⍵,(≢⍵)↓440 0 1 44100
          FM_Instr freq 1 5(1 0.75 0.7 0)(0.1 0.1 dur 0.1)sr
      }

      FM_Clarinet←{
      ⍝ ⍵ ←→ freq phs dur sr
          ⎕IO←0
          freq phs dur sr←⍵,(≢⍵)↓440 0 1 44100
          FM_Instr freq 1.5 4(0.25 0.5 1 1 0.33 0.16 0)(0.033 0.033 0.033 dur 0.02 0.02 0.02)sr
      }

      FM_WoodDrum←{
        ⍝ ⍵ ←→ freq phs dur sr
          ⎕IO←0
          freq phs dur sr←⍵,(≢⍵)↓440 0 1 44100
          FM_Instr freq 0.7143 2(1 0.01 0)(0.001 0.025 0.2)sr
      }

      FM_Sample←{
      ⍝ Play wave as an instrument by resampling
      ⍝ ⍵ ←→ freq phs dur sr
          ⎕IO←0
          sig osr←⍺
          freq phs dur sr←⍵,(≢⍵)↓440 0 1 44100
          r←440(⌊0.5+freq)×sr osr
          n←sr×dur
          n↑r #.DSP.Resample sig
      }

    :EndSection


      Leslie←{
        ⍝ Simulates the effect of a rotary loudspeaker. See LeslieDemo for a
        ⍝ demonstration of the use of this function.
        ⍝   ⍺←lfo td vd sr
        ⍝   ⍵←sig
        ⍝   x   = mono input signal
        ⍝   lfo = speaker rotation rate in revolutions per second
        ⍝   td  = tremolo depth (~0.3?)
        ⍝   vd  = vibrato depth (~0.003?)
        ⍝   sr  = sample rate in samples per second
          ⎕IO←0
          lfo td vd sr←⍺
          sig←⍵
          v←⍪lfo vd vd sr Vibrato sig
          t←⍪lfo td sr Tremolo v
          lr←t(-,+)0.7×v
          ⍉↑Normalize¨↓⍉lr
      }

      Noise←{
        ⍝ white noise generator
        ⍝ ⍵ ←→ dur sr
        ⍝ ← ←→ (samples)
          2×0.5-(⌈×/⍵)(?⍴)0
      }

    Normalize←{⍵÷⌈/⌈⌿|⍵}

      Oscillator←{
        ⍝ Generic oscillator. Creates a periodic signal from an arbitrary
        ⍝ user-supplied wave table. The table contains one cycle of the desired
        ⍝ waveform. The first and last items in the wave table must be identical.
        ⍝ Tables for sine, sawtooth, and square waveforms are available as variables
        ⍝ in the workspace.
        ⍝
        ⍝ ⍺ ←→ wave = a user-supplied vector of one cycle of the desired waveform
        ⍝ ⍵ ←→ freq phs dur sr
        ⍝      freq = frequency of the desired signal in Hz
        ⍝      phs  = starting phase in degrees, [0,360]
        ⍝      dur  = signal duration in seconds
        ⍝      sr   = desired sample rate
        ⍝ ← ←→ vector of samples of the desired signal
          ⎕IO←0
          freq phs dur sr←⍵,(≢⍵)↓440 0 1 44100
          lwave←≢⍺
          si←lwave×freq÷sr
          n←⌈dur×sr
          start←lwave×phs÷360
          ⍺[lwave|⌈start+si×⍳n]
      }

      Reverb←{
        ⍝ convolves a signal with a synthetic "impulse response" to obtain a reverb effct
        ⍝ ⍵ ←→ signal
        ⍝ ⍺ ←→ duration sr
        ⍝ ← ←→ samples
        ⍝ signal = mono samples vector
        ⍝ dur    = duration of impulse response in seconds
        ⍝ k      = decay constant (k=8 typical)
        ⍝ sr     = sample rate in samples per second
          dur k sr←⍺
          impulse←1 dur k sr Fade Noise dur sr
          #.DSP.Convolve ⍵ impulse
      }

      Sequencer←{
        ⍝ ⍺⍺ ←→ oscillator
        ⍝ ⍵ ←→ midi dur sr
          ⎕IO←0
          midi dur sr←⍵
          freq←MIDI2Freq⍪midi
          notes←freq,0,(⍪dur),sr
          ⊃,/0.01 0.1 0 sr∘AR¨⍺⍺¨↓notes
      }

      MIDI2Freq←{
        ⍝ Converts MIDI note number to frequency in Hz.
        ⍝   ⍵ ←→ vector or matrix of MIDI note numbers (0-127)
        ⍝   ← ←→ vector or matrix of the corresponding frequencies
          (440÷32)×2*(⍵-9)÷12
      }


      Sawtooth←{
        ⍝ ⍵ ←→ freq phs dur sr
          ⎕IO←0
          freq phs dur sr←⍵,(≢⍵)↓440 0 1 44100
          1-⍨2×1|0.5+(phs÷360)+freq×(⍳⌈dur×sr)÷sr
      }

      Sine←{
        ⍝ ⍵ ←→ freq phs dur sr
          ⎕IO←0
          freq phs dur sr←⍵,(≢⍵)↓440 0 1 44100
          1○○(phs÷180)+2×(⍳⌈dur×sr)×freq÷sr
      }

      Square←{
        ⍝ ⍵ ←→ freq phs dur sr
          ⎕IO←0
          freq phs dur sr←⍵,(≢⍵)↓440 0 1 44100
          1-2×1>2|2×(phs÷360)+freq×(⍳⌈dur×sr)÷sr
      }

      Tremolo←{
        ⍝ Apply amplitude modulation (tremolo) to a signal
        ⍝ ⍵ ←→ signal
        ⍝ ⍺ ←→ lfo mi sr
        ⍝      lfo = frequency of the modulating signal in Hz.
        ⍝      mi  = depth of modulation (modulation index), [0,1]
          ⎕IO←0
          lfo mi sr←⍺
          n←≢⍵
          t←n÷sr
          m←1+mi×Sine lfo 0 t sr
          (Normalize m)×[0]⍵
      }


      VCA←{
        ⍝ Uses a control signal to regulate the gain of the input signal.
        ⍝ The control signal should be in the range [0,1].
        ⍝ If the control signal goes negative,
        ⍝ the negative samples will be set to zero.
        ⍝ If the input signal and the control signal are not of the same length,
        ⍝ the input signal will be adjusted to the length of the control signal.
        ⍝ ⍵ ←→ (samples) (sample rate)
        ⍝ ⍺ ←→ control signal [ 0 , 1 ]
          (0⌈⍺)×(≢⍺)↑⍵
      }

      VCF←{
        ⍝ Applies a control signal to a resonant filter
        ⍝ to continuously update its center frequency.
        ⍝ ⍵ ←→ signal
        ⍝ ⍺ ←→ fl fh d ctrl sr
        ⍝      fl   = lowest center frequency of the filter in Hz.
        ⍝      fh   = highest center frequency of the filter in Hz.
        ⍝      d    = half-bandwidth of the filter in Hz.
        ⍝      ctrl = control signal [ 0,1 ]
        ⍝      sr   = sample rate in samples per second (global)
        ⍝ ← ←→ filtered signal
          fl fh d ctrl sr←⍺
          y←0 0,(≢ctrl)↑⍵
          F←fl+ctrl×fh-fl
          a←⍪-2×(*○¯2×d÷sr)×2○○2×F÷sr
          a,←*○¯4×d÷sr
          _←(↓a){0⊣y[⍵]-←+/⍺×y[⍵-1 2]}¨2↓⍳≢y
          Normalize 2↓y
      }

      Vibrato←{
        ⍝ Adapted from:
        ⍝ Udo Zoelzer, ed. "DAFX: Digital Audio Effects". Wiley, 2002, pp. 68-69.
        ⍝ ISBN-10: 9780471490784 
        ⍝ ISBN-13: 978-0471490784 
        ⍝ ASIN: 0471490784
        ⍝ ⍵ ←→ signal
        ⍝ ⍺ ←→ Modfreq Width Delay sr
          ⎕IO←1
          Modfreq Width Delay sr←⍺
          sig←⍵
          DELAY WIDTH←⌈Delay Width×sr
          WIDTH>DELAY:⎕←'error: delay greater than basic delay'
          M←Modfreq÷sr
          LEN←≢sig
          L←2+DELAY+WIDTH×2
          Delayline←sig↑⍨-L+≢sig
          MOD←1+DELAY+WIDTH×2○○2×M×⍳LEN
          i frac←↓0 1⊤MOD
          inds←(⍳LEN)+L-i
          y←(1-frac)×[1](⊂inds+1)⌷Delayline
          y+←frac×[1](⊂inds)⌷Delayline
          Normalize y
      }

:EndNamespace
