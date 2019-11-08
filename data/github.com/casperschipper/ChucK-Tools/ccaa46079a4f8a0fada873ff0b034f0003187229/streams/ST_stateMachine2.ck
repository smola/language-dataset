class ST_stateMachine extends Stream {
    null @=> Stream @ states[];
    
    fun ST_stateMachine init(Stream arg[]) {
        arg @=> states;
        return this;
    }
    
    fun float next() {
        states[index].nextInt() => int tmp;
        tmp % states.cap() => index;
        return index $ float;
    }
}

        
ST_stateMachine stream[5];
for (int i;i<stream.cap();i++) {
    stream[i].init([
    st.choice(1,2),
    st.seq([0,1,0,3,4]),
    st.tchoice([4,1],st.rv(0,5)),
    st.seq([3,4,3,4,4,4,3]),
    st.line(st.seq(0,5),st.rv(0,5))
    ]);
}

StepSynth synth => Safe safe => dac;


cs.sync(0.2::second);

synth.init(
st.mup(
    st.seq([-1,1]),
    st.mup(
    st.mup(
        st.index([0.1,0.001,0.003,0.002,0.004],stream[0]),
        st.index([0.1,0.5,0.25,0.5],stream[1])
    ),
    st.index([0.1,0.02,0.03,0.4,0.9],stream[2])
)
),
st.mup(
    st.mup(
        st.index([1,2,4,40,10],stream[0]),
        st.index([1,2,3,4],stream[1])
    ),
    st.index([1,2,3,40],stream[2])
)
);




hour => now;