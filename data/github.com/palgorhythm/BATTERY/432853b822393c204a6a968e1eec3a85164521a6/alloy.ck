MidiIn min;
MidiMsg msg;

OscOut osc;
osc.dest("10.10.10.1",6969);
oscOut("/song",[2]);

//IDEAS: 1. drum and bass song
// 2. song where each bd hit triggers fast rhythm that changes
// 3. deerhoof cover
// 4. EVIDENCE - monk : DO THIS FIRST
// 5. morton feldman vibes
// 6. use shakers ugen to trigger random environmental sounds
//7. use voicForm to do crazy shit


0 => int bassindex;
0 => int snareindex;
0 => int tomindex;
0 => int bSection;
0 => int Achordindex;
0 => int Bchordindex;
0 => int sectionIndex;
0 => float a;


int hitTom;
now=> time tomTime; //had added + 1000::second before, why???
int hitBass;
now  => time bassTime;
int hitSnare;
now => time snareTime;

[["F2","A3","B3","E4"],["D#2","G3","A3","D4"],["G#1","A#3","B3","D#4"],["A#1","A3","D4","E4"],["E1","A#3","B3","D#3"]] @=> string AchordStrings[][];
int Achords[AchordStrings.size()][AchordStrings[0].size()]; //all chords have to be same length

[["F1","A3","B3","E4"],["B1","A#3","D#4","F4"],["A#1","A3","D4","E4"],["F#1","A#3","D4","F4"],
["G#1","D4","D#4","G"],["D#1","A#3","D4","A4"],["E1","B3","D#4","G#4"],["A1","C4","G4","B4"],
["C#1","E4","B4","D#5"],["C1","D#4","A#4","D5"],["A#0","E4","F4","A4"],["D1","C#4","G#4","A4"],
["G1","A#3","F4","A4"],["F#1","A3","E4","G#4"],["C1","B3","F#4","G4"],["F#1","C4","A#4","F5"],
["C1","B3","F#4","D5"],["F#1","A3","E4","G#4"],["A#1","A3","E4","F4"],["C2","C#4","F4","G#4"],
["B1","A#3","D#4","F4"],["C2","B3","F#4","G4"],["F#1","C4","F4","A#4"],["F1","B3","E4","A4"],
["G#1","C4","D4","G4"],["D#1","D4","F4","A4"],["G#0","C4","D4","G4"],["D#1","D4","G4","A4"],
["E1","D#4","G#4","A#4"],["A1","C4","G4","B4"],["C#1","G#3","B3","D#4"],["E1","A3","E4","F4"],
["D#1","D4","G4","A4"],["F#1","F4","A#4","C5"],["B1","D#4","F4","A#"],["E1","D#4","G#4","A#4"]] @=> string BchordStrings[][];
int Bchords[BchordStrings.size()][BchordStrings[0].size()]; //all chords have to be same length

for(0 => int i; i<Achords.size(); i++) //convert to MIDI notes
{
    changeOctave(notes2nums(AchordStrings[i],1),"down",1) @=> Achords[i];
    changeOctave([Achords[i][0]],"up",1)[0] => Achords[i][0];
}

for(0 => int i; i<Bchords.size(); i++) //convert to MIDI notes
{
    changeOctave(notes2nums(BchordStrings[i],1),"down",1) @=> Bchords[i];
    changeOctave([Bchords[i][0]],"up",1)[0] => Bchords[i][0];
}

PulseOsc pulse; SawOsc saw1;SawOsc saw2;SawOsc saw3;
[pulse,saw1,saw2,saw3] @=> Osc OSCarray[];
ADSR E[4];
PRCRev R[4];
BiQuad F[4];

for( 0 => int i; i<4; i++)
{ 
    OSCarray[i] => E[i] => R[i] => dac;
    1.0 => OSCarray[i].gain;
    E[i].set(20::ms, 10::ms, .9, 500::ms);
    0.01 => R[i].mix;
    .99 => F[i].prad; 
    1 => F[i].eqzs;
    (5-i)*.02 => F[i].gain;
}


//MIDI port
0 => int port;

if( !min.open(port))
{
    <<<"ERROR: midi port didn't open on port:", port>>>;
    me.exit();
}

spork ~ ddrumTrig();
10::ms => now;
spork ~ ddrumTrig();
10::ms => now;
spork ~ ddrumTrig();
10::ms => now;
spork ~ voiceGate();
10::ms => now;
oscOut("/songSection",[0]);

while(true)
{
    1::second => now;
    if(sectionIndex > 3) 
    {
        oscOut("/songSection",[1]);
        1 => bSection;
    }
    else
    {
        oscOut("/songSection",[0]);
        0 => bSection;
    }
}

fun void voiceGate()
{
    while(true)
    {
        if(hitTom==1 && (now > tomTime + 300::ms) )
        {
            0 => hitTom;
        }
        if(hitBass==1 && (now > bassTime + 1000::ms) )
        {
            0 => hitBass;
        }
        if(hitSnare==1 && (now > snareTime + 100::ms) )
        {
            0 => hitSnare;
        }
        5::ms => now;
    }
}

fun void ddrumTrig()
{
    while(true)
    {
        min => now;
        //<<<(msg.data2)>>>;
        
        while(min.recv(msg))
        {
            //<<< msg.data1, msg.data2, msg.data3 >>>;
            if( msg.data3!=0 && msg.data2 == 36 && hitBass==0) //kick drum
            {
                1 => hitBass; 
                
                for( 0 => int i; i<4; i++)
                { 
                    //((sectionIndex % 4) + 1) * 350 => a;
                    if(bSection == 0)
                    {
                        ((Bchordindex+1.0) % Bchords.size()) * 1000.0 => a;
                        E[i].set(20::ms, 10::ms, .9, a::ms);
                        //<<<a>>>;
                    }
                    else
                    {
                        ((Achordindex+1.0) % Achords.size()) * 1000.0 => a;
                        //<<<a>>>;
                        E[i].set(20::ms, 10::ms, .9, a::ms);
                    }
                    
                }
                
                //<<<a>>>;
                
                if(bSection == 0)
                {
                    for(0 => int i; i < Achords[Achordindex].size(); i++)
                    {
                        E[i].keyOff();
                    }
                    
                    oscOut("/chords",[Achords[Achordindex][0]]);
                    
                    for(0 => int i; i < Achords[Achordindex].size(); i++)
                    {
                        
                        Std.mtof(Achords[Achordindex][i]) => OSCarray[i].freq;
                        E[i].keyOn();
                    }
                    
                    if(Achordindex == Achords.size()-1) sectionIndex+1 => sectionIndex;
                    
                    (Achordindex + 1) % Achords.size() => Achordindex;
                    
                    
                    100::ms => now;
                    
                    
                    for(0 => int i; i < Achords[Achordindex].size(); i++)
                    {
                        E[i].keyOff();
                    }
                    
                    100::ms => now;    
                }
                else
                {
                    for(0 => int i; i < Bchords[Bchordindex].size(); i++)
                    {
                        E[i].keyOff();
                    }
                    
                    oscOut("/chords",[Bchords[Bchordindex][0]]);
                    
                    for(0 => int i; i < Bchords[Bchordindex].size(); i++)
                    {
                        
                        Std.mtof(Bchords[Bchordindex][i]) => OSCarray[i].freq;
                        E[i].keyOn();
                    }
                    
                    if(Bchordindex == Bchords.size()-1) sectionIndex+1 => sectionIndex;
                    
                    (Bchordindex + 1) % Bchords.size() => Bchordindex;
                    
                    
                    100::ms => now;
                    
                    
                    for(0 => int i; i < Bchords[Bchordindex].size(); i++)
                    {
                        E[i].keyOff();
                    }
                    
                    100::ms => now;  
                    
                }
                
            }   
        }
    }    
}


///UTIL FUNCTIONS///

function int[] notes2nums(string notes[], int numcopies)
{
    ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"] @=> string A[];
    string numTOnote[127];
    int noteTOnum[127];
    
    //create array of midi notes indexed by integers 0-127
    for(0 => int i; i<127;i++)
    {
        i % 12 => int mod;
        -2 + i/12 => int counter;
        A[mod] + counter => numTOnote[i];
    }
    
    //create array indexed by midi notes (the inverse of B)
    for(0 => int i; i<127; i++)
    {
        i => noteTOnum[numTOnote[i]];
    }
    
    int numbers[notes.size()*numcopies];
    for(0 => int i; i<notes.size()*numcopies;i++)
    {
        i % notes.size() => int index;
        noteTOnum[notes[index]] => numbers[i];
    }
    return numbers;
}

function int[] changeOctave(int noteNums[], string choice, int x)
{
    int result[noteNums.size()];
    
    if(choice == "up")
    {
        for(0 => int i; i < noteNums.size(); i++)
        {
            noteNums[i] + 12*x => result[i];
        }
    }
    else if(choice == "down")
    {
        for(0 => int i; i < noteNums.size(); i++)
        {
            noteNums[i] - 12*x => result[i];
        }
    }
    else
    {
        <<<"ERROR">>>;
    }
    return result;
}

function int[] transpose(int noteNums[], string choice, int halfSteps)
{
    int result[noteNums.size()];
    
    if(choice == "up")
    {
        for(0 => int i; i < noteNums.size(); i++)
        {
            noteNums[i] + halfSteps => result[i];
        }
    }
    else if(choice == "down")
    {
        for(0 => int i; i < noteNums.size(); i++)
        {
            noteNums[i] - halfSteps => result[i];
        }
    }
    else
    {
        <<<"ERROR">>>;
    }
    return result;
}

fun void oscOut(string addr, int val[]){
    osc.start(addr);
    
    for(0 => int i; i<val.size(); i++)
    {
        osc.add(val[i]);
    }
    osc.send();
}