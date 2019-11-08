// rRobot.ck
// Eric Heep, October 2015
// ~
// example code that uses the Gamelan
// framework to control the robots in the Machine Lab

// groove receiver
OscRecv recv;
6449 => recv.port;
recv.listen();

// robot OSC
OscSend xmit;
xmit.setHost("chuckServer.local", 50000);

// create an address in the receiver, store in new variable
recv.event( "/plork/synch/clock, i i" ) @=> OscEvent oe;

// get pane
AudicleGroove.pane() @=> AudiclePane @ pane;

// dat groove
[[1.0, 0.2, 0.3, 0.2, 0.4, 0.1, 0.2, 0.1],
[0.5, 0.1, 0.3, 0.2, 0.4, 0.1, 0.2, 0.1],
[0.8, 0.1, 0.3, 0.2, 0.5, 0.1, 0.2, 0.1],
[0.4, 0.1, 0.3, 0.2, 0.3, 0.1, 0.2, 0.1]] @=> float mygains[][];

// vars
int x, y, val;

while (true) {
    // wait for event to arrive
    oe => now;

    // grab the next message from the queue.
    while (oe.nextMsg() != 0) {
        // get x and y
        oe.getInt() => x;
        oe.getInt() => y;
        // set glow
        pane.setglow( x, y, 1 );
        // get value
        pane.getvalue( x, y ) => val;
        // play the thing
        if (val > 0) {
            spork ~ play(val, (127 * mygains[y][x]) $ int);
        }
    }
}

fun void play(int val, int vel) {
    if (val == 1) {
        note("/drumBot", 0, vel);
    }
    if (val == 2) {
        note("/drumBot", 1, vel);
    }
    if (val == 3) {
        for (int i; i < 10; i++) {
            note("/clappers", i, vel);
            10::ms => now;
        }
    }
    if (val == 4) {
        for (10 => int i; i < 20; i++) {
            note("/clappers", i, vel);
            10::ms => now;
        }
    }
    if (val == 5) {
        note("/ganapati", 1, vel);
    }
    if (val == 6) {
        note("/ganapati", 2, vel);
    }
    if (val == 7) {
        note("/devibot", 4, vel);
    }
    if (val == 8) {
        note("/devibot", 5, vel);
    }
}

fun void note(string addr, int num, int vel) {
    xmit.startMsg(addr, "i i");
    xmit.addInt(num);
    xmit.addInt(vel);
}
