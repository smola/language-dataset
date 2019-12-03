#include "fir12_par.h"

#include <stdio.h>

#include "fir12.h"

void fir12_1(const int coefficients[], int offset, int N, streaming chanend cin, streaming chanend cout, int data[]) {
    int dat;
    int w = 0;
    long long result, res;

    for(int i = 0; i < N+12; i++) {
        data[i] = 0;                 // no unsafe arrays here - checks bound
    }

#pragma unsafe arrays
    while(1) {
        cout <: data[w];
        cin :> dat;
        data[w] = dat;
        if (w < 12) {
            data[N+w] = dat;
        }
        result = fir12coffset(coefficients, data, w, N, offset);
        cout :> res;
        cin <: result+res;
        w--;
        if (w < 0) {
            w = N-1;
        }
    }
}

void fir12_e(const int coefficients[], int offset, int N, streaming chanend cin, int data[]) {
    int dat;
    int w = 0;

    for(int i = 0; i < N+12; i++) {
        data[i] = 0;                 // no unsafe arrays here - checks bound
    }

#pragma unsafe arrays
    while(1) {
        cin :> dat;
        data[w] = dat;
        if (w < 12) {
            data[N+w] = dat;
        }
        cin <: fir12coffset(coefficients, data, w, N, offset);
        w--;
        if (w < 0) {
            w = N-1;
        }
    }
}

void fir_par4_48(int coefficients[], int N, streaming chanend cin,
                 int data0[], int data1[], int data2[], int data3[]) {
    streaming chan a, b, c;
    if (N%48 != 0) {
        return;
    }
    coefficients[N-1];                // Check bound on coefficients
    par {
        fir12_1(coefficients, 0*N/4, N/4, cin, a, data0);
        fir12_1(coefficients, 1*N/4, N/4, a, b, data1);
        fir12_1(coefficients, 2*N/4, N/4, b, c, data2);
        fir12_e(coefficients, 3*N/4, N/4, c, data3);
    }
}

void fir_par3_36(int coefficients[], int N, streaming chanend cin,
                 int data0[], int data1[], int data2[]) {
    streaming chan a, b;
    if (N%36 != 0) {
        return;
    }
    coefficients[N-1];                // Check bound on coefficients
    par {
        fir12_1(coefficients, 0*N/3, N/3, cin, a, data0);
        fir12_1(coefficients, 1*N/3, N/3, a, b, data1);
        fir12_e(coefficients, 2*N/3, N/3, b, data2);
    }
}

void fir_par2_24(int coefficients[], int N, streaming chanend cin,
                 int data0[], int data1[]) {
    streaming chan a;
    if (N%24 != 0) {
        return;
    }
    coefficients[N-1];                // Check bound on coefficients
    par {
        fir12_1(coefficients, 0*N/2, N/2, cin, a, data0);
        fir12_e(coefficients, 1*N/2, N/2, a, data1);
    }
}

void fir_par1_12(int coefficients[], int N, streaming chanend cin,
                 int data0[]) {
    if (N%12 != 0) {
        return;
    }
    coefficients[N-1];                // Check bound on coefficients
    fir12_e(coefficients, 0, N, cin, data0);
}
