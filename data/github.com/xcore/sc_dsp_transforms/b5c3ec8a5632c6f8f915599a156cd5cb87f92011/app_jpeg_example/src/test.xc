// Copyright (c) 2011, XMOS Ltd, All rights reserved
// This software is freely distributable under a derivative of the
// University of Illinois/NCSA Open Source License posted in
// LICENSE.txt and at <http://github.xcore.com/>

#include "stdio.h"
#include "xs1.h"
#include "huffman.h"
#include "huffTables.h"
#include "fdctint.h"

const unsigned char quant[64] = {
        16, 11, 10, 16, 24, 40, 51, 61,
        12, 12, 14, 19, 26, 58, 60, 55,
        14, 13, 16, 24, 40, 57, 69, 56,
        14, 17, 22, 29, 51, 87, 80, 62,
        18, 22, 37, 56, 68, 109, 103, 77,
        24, 35, 55, 64, 81, 104, 113, 92,
        49, 64, 78, 87, 103, 121, 120, 101,
        72, 92, 95, 98, 112, 100, 103, 99
};


#define SOF0 0xFFC0

#define SOI 0xFFD8
#define EOI 0xFFD9
#define DHT 0xFFC4
#define DQT 0xFFDB
#define SOS 0xFFDA
#define RST0 0xFFD0

extern void header2(int header);

void header4(int header) {
    header2(header>>8);
    header2(header & 0xff);
}


void hufftableprint(const unsigned char lengths[], const short codes[], int len, int isAC) {
    int allL[17];
    int Lh = 3;
    for(int i = 0; i < 17; i++) {
        allL[i] = 0;
    }
    for(int i = 0; i < len; i++) {
        allL[(int)lengths[i]] ++;
    }
    for(int i = 1; i < 17; i++) {
        Lh += allL[i]+1;
    }
    header4(DHT);
    header4(Lh);
    header2(isAC<<4);
    for(int i = 1; i < 17; i++) {
        header2(allL[i]);
    }
    for(int i = 1; i < 17; i++) {
        if (len > 100) {
            for(int k = 0; k < 16; k++) {
                for(int j = 0; j <= 10; j++) {
                    int index = k | j << 4;
                    if (lengths[index] == i) {
                        header2(j | k << 4);
                    }
                }
            }
        } else {
            for(int k = 0; k < len; k++) {
                if (lengths[k] == i) {
                    header2(k);
                }
            }
        }
    }
}

extern int ySize, xSize;
extern unsigned char image[];

void jpegheader(void) {
    header4(SOI);
    header4(SOF0);
    header4(11); // length
    header2(8); // Depth
    header4(ySize); // Y
    header4(xSize); // X
    header2(1); // Number

    header2(0); //
    header2(0x11); // KX/Y ratios
    header2(0); // Quantisation table index

    header4(DQT);
    header4(67);
    header2(0x00);
    header2(quant[0]);
    for(int i = 0; i < 63; i++) {
        header2(quant[(int)ordering[i]]);
    }

    hufftableprint(dclengths, dccodes, 12, 0);
    hufftableprint(aclengths, accodes, 176, 1);

    header4(SOS);
    header4(8); // length
    header2(1);

    header2(0); // Scan components
    header2(0x00); // Entropy encoding tables

    header2(0); // Scan start
    header2(63); // Scan end
    header2(0x00); // Ahl
}

void jpegtrailer() {
    header4(RST0);
    header4(EOI);
}

void jpegPrinter(streaming chanend codes) {
    unsigned char c[20000];
    int cnt = 0;
    unsigned char ct;
    while (!stestct(codes)) {
        unsigned char x;
        codes :> x;
        c[cnt++] = x;
        if (x == 0xff) {
            c[cnt++] = 0;
        }
//        printf(".");
    }
    ct = sinct(codes);
    soutct(codes, ct);
    jpegheader();
    for(int i = 0; i < cnt; i++) {
        header2(c[i]);
    }
    jpegtrailer();
//    printf("%d codes\n", cnt);
}

static void p(int x[], int div) {
    for(int i = 0; i < 8; i++) {
        for(int j = 0; j < 8; j++) {
            printf("%8d ", x[i*8+j]/div);
        }
        printf("\n");
    }
    printf("\n");
}

extern void readImage(void);

int call = 0;

static void fill(streaming chanend blocks, streaming chanend toHuff, int block[], int index) {
    int cnt = 0, addr;
    for(int z = 0; z < xSize; z+= (xSize >> 3)) {
        int bits = image[index+z];
//        printf("%d %d %1x\n", z, index+z, bits);
#pragma loop unroll(8)
        for(int k = 0; k < 8; k++) {
            block[cnt++] = (bits & 1)* 255;
            bits >>= 1;
        }
    }
  //  p(block, 1);
    asm("out res[%0], %1" :: "r"(blocks), "r" (block));
    switch(call) {
    case 0:
        call = 1;
        break;
    case 1:
        blocks :> addr;
        toHuff <: addr;
        call = 2;
        break;
    case 2:
        toHuff :> addr;
        blocks :> addr;
        toHuff <: addr;
        break;
    }
}

void feed(streaming chanend blocks, streaming chanend toHuff) {
    int block0[64];
    int block1[64];
    int block2[64];
    int block3[64];
    int addr;
    timer t;
    unsigned t0, t1;
    int pixels = ySize * xSize;

    t :> t0;
    for(int y = 0; y < xSize * (ySize >> 3); y+= xSize) {
        for(int x = 0; x < (xSize>>3); x+=4) {
            fill(blocks, toHuff, block0, x+y);
            fill(blocks, toHuff, block1, x+1+y);
            fill(blocks, toHuff, block2, x+2+y);
            fill(blocks, toHuff, block3, x+3+y);
        }
    }
    toHuff :> addr;
    blocks :> addr;
    toHuff <: addr;
    toHuff :> addr;
    t :> t1;
    printf("Compression: %d us for %d pixels @ 100 MIPS: %d pixels/sec (50 MIPS)\n", (t1-t0)/100, pixels, pixels/2*(100000000/(t1-t0)));
    printf("grey QVGA: %d fps (1 thread, 50 MIPS)\n", (pixels/2*(100000000/(t1-t0)))/320/240);
    printf("grey  VGA: %d fps (1 thread, 50 MIPS)\n", (pixels/2*(100000000/(t1-t0)))/640/480);
    endDCT(blocks);
    endHuff(toHuff);
}

int main(void) {
    streaming chan toHuffman, toCodes;
    streaming chan toDCT;
/*    int x[65] = {
        52,55,61, 66, 70, 61,64,73,
        63,59,55, 90,109, 85,69,72,
        62,59,68,113,144,104,66,73,
        63,58,71,122,154,106,70,69,
        67,61,68,104,126, 88,68,70,
        79,65,60, 70, 77, 68,58,75,
        85,71,64, 59, 55, 61,65,83,
        87,79,69, 68, 65, 76,78,94, 0xdeadbeef
    };*/
    int quant2[65];
    readImage();
    quantDCT(quant2, quant);
    par {
        forwardDCT(toDCT, quant2);
        processHuff(toHuffman, toCodes);
        jpegPrinter(toCodes);
        feed(toDCT, toHuffman);
    }
    return 0;
}
