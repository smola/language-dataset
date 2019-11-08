int midi[0];

0 => midi["C-2"];   1 => midi["C#-2"];  1 => midi["Db-2"];  2 => midi["D-2"];
3 => midi["D#-2"];  3 => midi["Eb-2"];  4 => midi["E-2"];   5 => midi["F-2"];
6 => midi["F#-2"];  6 => midi["Gb-2"];  7 => midi["G-2"];   8 => midi["G#-2"];
8 => midi["Ab-2"];  9 => midi["A-2"];   10 => midi["A#-2"]; 10 => midi["Bb-2"];
11 => midi["B-2"];  12 => midi["C-1"];  13 => midi["C#-1"]; 13 => midi["Db-1"];
14 => midi["D-1"];  15 => midi["D#-1"]; 15 => midi["Eb-1"]; 16 => midi["E-1"];
17 => midi["F-1"];  18 => midi["F#-1"]; 18 => midi["Gb-1"]; 19 => midi["G-1"];
20 => midi["G#-1"]; 20 => midi["Ab-1"]; 21 => midi["A-1"];  22 => midi["A#-1"];
22 => midi["Bb-1"]; 23 => midi["B-1"];  24 => midi["C0"];   25 => midi["C#0"];
25 => midi["Db0"];  26 => midi["D0"];   27 => midi["D#0"];  27 => midi["Eb0"];
28 => midi["E0"];   29 => midi["F0"];   30 => midi["F#0"];  30 => midi["Gb0"];
31 => midi["G0"];   32 => midi["G#0"];  32 => midi["Ab0"];  33 => midi["A0"];
34 => midi["A#0"];  34 => midi["Bb0"];  35 => midi["B0"];   36 => midi["C1"];
37 => midi["C#1"];  37 => midi["Db1"];  38 => midi["D1"];   39 => midi["D#1"];
39 => midi["Eb1"];  40 => midi["E1"];   41 => midi["F1"];   42 => midi["F#1"];
42 => midi["Gb1"];  43 => midi["G1"];   44 => midi["G#1"];  44 => midi["Ab1"];
45 => midi["A1"];   46 => midi["A#1"];  46 => midi["Bb1"];  47 => midi["B1"];
48 => midi["C2"];   49 => midi["C#2"];  49 => midi["Db2"];  50 => midi["D2"];
51 => midi["D#2"];  51 => midi["Eb2"];  52 => midi["E2"];   53 => midi["F2"];
54 => midi["F#2"];  54 => midi["Gb2"];  55 => midi["G2"];   56 => midi["G#2"];
56 => midi["Ab2"];  57 => midi["A2"];   58 => midi["A#2"];  58 => midi["Bb2"];
59 => midi["B2"];   60 => midi["C3"];   61 => midi["C#3"];  61 => midi["Db3"];
62 => midi["D3"];   63 => midi["D#3"];  63 => midi["Eb3"];  64 => midi["E3"];
65 => midi["F3"];   66 => midi["F#3"];  66 => midi["Gb3"];  67 => midi["G3"];
68 => midi["G#3"];  68 => midi["Ab3"];  69 => midi["A3"];   70 => midi["A#3"];
70 => midi["Bb3"];  71 => midi["B3"];   72 => midi["C4"];   73 => midi["C#4"];
73 => midi["Db4"];  74 => midi["D4"];   75 => midi["D#4"];  75 => midi["Eb4"];
76 => midi["E4"];   77 => midi["F4"];   78 => midi["F#4"];  78 => midi["Gb4"];
79 => midi["G4"];   80 => midi["G#4"];  80 => midi["Ab4"];  81 => midi["A4"];
82 => midi["A#4"];  82 => midi["Bb4"];  83 => midi["B4"];   84 => midi["C5"];
85 => midi["C#5"];  85 => midi["Db5"];  86 => midi["D5"];   87 => midi["D#5"];
87 => midi["Eb5"];  88 => midi["E5"];   89 => midi["F5"];   90 => midi["F#5"];
90 => midi["Gb5"];  91 => midi["G5"];   92 => midi["G#5"];  92 => midi["Ab5"];
93 => midi["A5"];   94 => midi["A#5"];  94 => midi["Bb5"];  95 => midi["B5"];
96 => midi["C6"];   97 => midi["C#6"];  97 => midi["Db6"];  98 => midi["D6"];
99 => midi["D#6"];  99 => midi["Eb6"];  100 => midi["E6"];  101 => midi["F6"];
102 => midi["F#6"]; 102 => midi["Gb6"]; 103 => midi["G6"];  104 => midi["G#6"];
104 => midi["Ab6"]; 105 => midi["A6"];  106 => midi["A#6"]; 106 => midi["Bb6"];
107 => midi["B6"];  108 => midi["C7"];  109 => midi["C#7"]; 109 => midi["Db7"];
110 => midi["D7"];  111 => midi["D#7"]; 111 => midi["Eb7"]; 112 => midi["E7"];
113 => midi["F7"];  114 => midi["F#7"]; 114 => midi["Gb7"]; 115 => midi["G7"];
116 => midi["G#7"]; 116 => midi["Ab7"]; 117 => midi["A7"];  118 => midi["A#7"];
118 => midi["Bb7"]; 119 => midi["B7"];  120 => midi["C8"];  121 => midi["C#8"];
121 => midi["Db8"]; 122 => midi["D8"];  123 => midi["D#8"]; 123 => midi["Eb8"];
124 => midi["E8"];  125 => midi["F8"];  126 => midi["F#8"]; 126 => midi["Gb8"];
127 => midi["G8"];

class MidiValues
{
    fun static int pitch(int value)
    {
        return value % 12;
    }

    fun static int octave(int value)
    {
        return (value - pitch(value)) / 12;
    }

    fun static int adjustedOctave(int value)
    {
        octave(value) => int result;

        if(pitch(value) > 6)
        {
            result++;
        }

        return result;
    }

    fun static int build(int fromOctave, int fromPitch)
    {
        <<< "Build ", fromOctave, fromPitch >>>;
        return fromOctave * 12 + fromPitch;
    }
}
