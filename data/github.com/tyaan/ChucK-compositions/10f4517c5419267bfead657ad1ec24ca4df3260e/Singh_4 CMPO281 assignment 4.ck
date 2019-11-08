<<<"Tyaan Singh - assignment 4">>>;

180 => float bpm;
60/bpm => float beat;
beat*4 => float bar;

SndBuf kick => dac;
SndBuf snr => PRCRev snrRev => dac;
SndBuf hat => dac;
SndBuf chime => NRev chimeRev => dac;
SndBuf click => dac;

SawOsc saw => ADSR sawEnv => dac;
SinOsc sin => sawEnv => dac;
Noise noise => dac;

me.dir() => string path;
"/audio/kick_03.wav" => string kickFile;
"/audio/snare_03.wav" => string snrFile;
"/audio/hh_04.wav" => string hatFile;
"/audio/chime_01.wav" => string chimeFile;
"/audio/click_03.wav" => string clickFile;

path+kickFile => kickFile;
path+snrFile => snrFile;
path+hatFile => hatFile;
path+chimeFile => chimeFile;
path+clickFile => clickFile;

kickFile => kick.read;
snrFile => snr.read;
hatFile => hat.read;
chimeFile => chime.read;
clickFile => click.read;

0 => kick.rate;
0 => snr.rate;
0 => hat.rate;
0 => chime.rate;
0 => click.rate;

0.4 => kick.gain;
0.25 => snr.gain;
0.05 => hat.gain;
0.2 => chime.gain;
0.3 => click.gain;

0.03 => snrRev.mix;
0.1 => chimeRev.mix;

0.1 => saw.gain;
sawEnv.set(30::ms, 60::ms, 0, 0::ms);
[38, 36, 41, 33]@=> int sawArray[];

0 => noise.gain;

0 => sin.gain;
[65, 64, 69, 57]@=> int sinArray[];

0 => int i;


while(true){
    if(i%8==0||i%8==3){
        0 => kick.pos;
        1 => kick.rate;
    }
    if((i%8==6 || i%8==3 || i%32==31)&& i>127){
        if(i%8==6){1.3 => snr.rate; 0 => snr.pos; 0.25 => snr.gain;}
        else{1 => snr.rate; 0 => snr.pos; 0.08 => snr.gain;}
    }
    if(i%4==0||i%4==1||i%4==2){
        0 => hat.pos;
        1 => hat.rate;
        if(i%4==0){ 
            hat.samples()/4 => hat.pos;
        }
    }
    if((i%64==0||i%64==32)){
        0 => chime.pos;
        if(i%64==0){0.5 => chime.rate;}
        else{1 => chime.rate;}
    }
    if((i%8==0||i%8==2||i%8==4||i%8==5||i%8==7)&& i>63){
        0 => click.pos;
        Std.rand2f(1, 4) => click.rate;
    }
    
    
    Std.mtof(sawArray[(i%32)/8]) => saw.freq;
    if(i<64){ saw.freq()*2 => saw.freq; }
    if((i%8<4 || i%64>59)&&(i<64 || i>127)){
        sawEnv.keyOn();
    }
    if((i%16>11 || i%64>55)&& (i>63 && i<128)){
        Std.mtof(sawArray[3]+12) => saw.freq;
        sawEnv.keyOn();
    }
    
    Std.mtof(sinArray[(i%32)/8]) => sin.freq;
    if(i>119){ 0.1 => sin.gain; }
    
    
    if(i>95 && i<128){
        i => float j;
        (j-96)/600 => noise.gain;
    }else{
        0 => noise.gain;
    }
    
    if(i>119&& i<128){
        0 => kick.rate;
        0 => hat.rate;
        0 => noise.gain;
    }
    
    if(i==256){ break; }
    
    (beat/2)::second => now;
    i++;
}
