// Copyright (c) 2011, XMOS Ltd, All rights reserved
// This software is freely distributable under a derivative of the
// University of Illinois/NCSA Open Source License posted in
// LICENSE.txt and at <http://github.xcore.com/>

#include <xs1.h>
#include "stdio.h"
#include "print.h"
#include "pwmPoint.h"
#include "pwmWide.h"

#define unsafearrays

extern int changeZero;
extern int stableZero;
extern int loopEven;
extern int loopOdd;
extern int loopAround;


#define stableOpcode(diff)         (stableZero - ((diff) << 1))

#define changeOpcode(numBytes)     (changeZero - (numBytes))

#if 0
static void explain(int i, unsigned addr, unsigned w, int pc) {
    printf("%3d  %08x  %08x ", i, addr, w);
    if (w > stableZero - 20 && w <= stableZero) {
        printf("  Stable%d", (stableZero-w)>>1);
    }
    if (w > changeZero - 80 && w <= changeZero) {
        printf("  Change%d", (changeZero-w)>>2);
    }
    if (w == loopAround) {
        printf("  Loop Around");
    }
    if (i ==pc) printf(" ***\n");
    printf("\n");
}

static void explainAll(unsigned programSpace[], int n, int pc) {
    for(int i = 0; i < n; i++) {
        explain(i, makeAddress(programSpace, i), programSpace[i], pc);
    }
}

#endif

#define MAX 16

const int multiplierOneTable[4] = {
    0x01010101, 0x01010100, 0x01010000, 0x01000000,
};
const int multiplierTable[16] = {
    0x00000000, 0x00000001, 0x00000101, 0x00010101,
    0xDEADBEEF, 0x00000000, 0x00000100, 0x00010100,
    0xDEADBEEF, 0xDEADBEEF, 0x00000000, 0x00010000,
    0xDEADBEEF, 0xDEADBEEF, 0xDEADBEEF, 0x00000000,
};

#define MAXHALFCYCLE 80
#define PROGRAMSPACESIZE (256+MAXHALFCYCLE)

#ifdef unsafearrays
#pragma unsafe arrays
#endif
void pwmControl1(in port syncport, streaming chanend c, streaming chanend toPWM) {
    unsigned pc;
    unsigned currenttime;
    unsigned int portval = 0;
    unsigned startPC;
    unsigned int ct3;
    unsigned int synctime, newsynctime, oldsynctime;
    unsigned currentByte;
    int addressOffset;

    int numBytes = 4;
    int t1, t2;
    timer t;
    struct pwmpoint points[8];
    unsigned programSpace[PROGRAMSPACESIZE];

    addressOffset = makeAddress(programSpace, 0) - 256;

    c :> currentByte;
    c :> currenttime;
    programSpace[0] = currentByte * 0x01010101;
    programSpace[1] = currenttime;
    pc = 4;
    startPC = pc-1;
    ct3 = 0;

    schkct(toPWM, 0);                             // Wait for PWM thread to be ready.
    toPWM <: makeAddress(programSpace, 0) - 240;  // Set PWM thread going

    oldsynctime = currenttime - 8000;
    synctime = currenttime;

    while(1) {
        t :> t1;
//        slave {
            for(int i = 0; i < 8; i++) {
                c :> points[i].time;
            }
            c :> newsynctime ;
//        }
        sortPoints(points);
//#pragma loop unroll(2)
        for(int currentpoint = 0; currentpoint != 8; currentpoint++) {
            unsigned nexttime = points[currentpoint].time;
            unsigned nt3 = nexttime & 3;
            unsigned diff;
            nexttime -= nt3;                    // nexttime is guaranteed a multiple of 4.
            diff = nexttime - currenttime;      // diff is guaranteed a multiple of 4.
            if (diff != 0) {
                diff = (diff >> 2) - 1;
                portval |= currentByte * multiplierOneTable[ct3];
                programSpace[pc++] = portval;
                if (diff >= MAX) {
                    programSpace[pc] = currentByte * 0x01010101;
                    pc++;
                    programSpace[pc] = diff;
                    pc++;
                    programSpace[pc] = loopAround;
                    pc++;
                    programSpace[pc] = changeOpcode(numBytes);
                    pc += 2;    // leave room for nextPC, nextInstr, stable, loopcount

                    // Now patch into previous instruction
                    programSpace[startPC] = pc*4 + addressOffset;
                    startPC = pc-1;
                    numBytes = 4;
                    if (pc >= PROGRAMSPACESIZE - MAXHALFCYCLE) {
                        pc = 0;
                    }
                } else if (diff >= 4) {
                    programSpace[pc] = currentByte * 0x01010101;
                    pc += 2;
                    programSpace[pc] = stableOpcode(diff);
                    pc++;
                    programSpace[pc] = changeOpcode(numBytes);
                    pc += 2;    // leave room for nextPC, nextInstr, stable, loopcount

                    // Now patch into previous instruction
                    programSpace[startPC] = pc*4 + addressOffset;
                    numBytes = 4;
                    startPC = pc-1;
                } else {
                    switch(diff) {
                    case 3:
                        portval = currentByte * 0x01010101;
                        programSpace[pc++] = portval;
                        programSpace[pc++] = portval;
                        programSpace[pc++] = portval;
                        numBytes += 16;
                        break;
                    case 2:
                        portval = currentByte * 0x01010101;
                        programSpace[pc++] = portval;
                        programSpace[pc++] = portval;
                        numBytes += 12;
                        break;
                    case 1:
                        portval = currentByte * 0x01010101;
                        programSpace[pc++] = portval;
                        numBytes += 8;
                        break;
                    case 0:
                        numBytes += 4;
                        break;
                    default:
//                        __builtin_unreachable();
                        break;
                    }
                }
                portval = currentByte * multiplierTable[nt3];
            } else {
                int x = multiplierTable[ct3 << 2 | nt3];
                portval |= currentByte * x;
            }
            currenttime = nexttime;
            ct3 = nt3;
            currentByte ^= points[currentpoint].value;
        }
        t :> t2;
//        printintln(t2-t1);//        printf("%d\n", t2-t1);
#pragma xta endpoint "loop"
        syncport @ oldsynctime :> void;
        oldsynctime = synctime;
        synctime = newsynctime;
    }
}

extern void doPWM8(buffered out port:32 p8, streaming chanend toPWM);

void pwmWide1(buffered out port:32 p8, in port syncport, streaming chanend c) {
    streaming chan toPWM;
    par {
        doPWM8(p8, toPWM);
        pwmControl1(syncport, c, toPWM);
    }
}
