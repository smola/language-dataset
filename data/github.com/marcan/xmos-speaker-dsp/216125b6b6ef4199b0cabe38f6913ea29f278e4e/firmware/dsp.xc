#include <xs1.h>
#include <print.h>
#include <stdio.h>
#include "mixer.h"
#include "devicedefines.h"
#include "dsp.h"

#define LDW(dest, array, index) asm("ldw %0, %1[%2]":"=r"(dest):"r"(array),"r"(index))
#define STW(array, index, value) asm("stw %0, %1[%2]"::"r"(value),"r"(array),"r"(index))

static struct biquad biquads[DSP_EXT_CH][DSP_FILTERS+1] = {
	{
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,0,0,0,0,0},
	},
	{
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,0,0,0,0,0},
	},
	{
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,0,0,0,0,0},
	},
	{
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,0,0,0,0,0},
	},
	{
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},
		{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,ONE,0,0,0,0},{0,0,0,0,0,0,0},
	},
};

static int audiobuf[DSP_EXT_CH][DELAY_BUF];
static int delay[DSP_EXT_CH];
static int p_write[DSP_EXT_CH];

static int dsp_out_samples[DSP_EXT_CH];

#pragma unsafe arrays
static void giveSamplesToDevice(chanend c)
{
#pragma loop unroll
	for (int i = 0; i < DSP_EXT_CH; i++)
		outuint(c, dsp_out_samples[i]);
}
#pragma unsafe arrays
static void getSamplesFromMixer(chanend c)
{
#pragma loop unroll
	for (int i = 0; i < DSP_EXT_CH; i++)
		STW(dsp_out_samples, i, inuint(c));
}

void dsp_sub(int ch, chanend c, const struct biquad bq[])
{
	int result;
	while(1) {
		int p;
		inuint(c);
		p = (p_write[ch] + 1) & MAX_DELAY;
		STW(p_write, ch, p);
		result = biquad_cascade(dsp_out_samples[ch], DSP_FILTERS, bq, HEADROOMBITS);
		STW(audiobuf[ch], p, result);
		result = audiobuf[ch][(p - delay[ch]) & MAX_DELAY];
		STW(dsp_out_samples, ch, result);
		outuint(c, 0);
	}
}

void dsp_main(chanend c_dsp, chanend c1, chanend c2, chanend c3, chanend c4)
{
	int result;
	set_thread_fast_mode_on();
	while (1) {
		int p;
		if (testct(c_dsp)) {
			int ch, i, cmd;
			cmd = inct(c_dsp);
			switch (cmd) {
				case SET_DSP_BIQUAD:
					ch = inuint(c_dsp);
					i = inuint(c_dsp);
					biquads[ch][i].xn1 = 0;
					biquads[ch][i].xn2 = 0;
					biquads[ch][i].b0 = inuint(c_dsp);
					biquads[ch][i].b1 = inuint(c_dsp);
					biquads[ch][i].b2 = inuint(c_dsp);
					biquads[ch][i].a1 = inuint(c_dsp);
					biquads[ch][i].a2 = inuint(c_dsp);
					break;
				case SET_DSP_DELAY:
					ch = inuint(c_dsp);
					STW(delay, ch, inuint(c_dsp));
					break;
			}
		}
		inuint(c_dsp);
		giveSamplesToDevice(c_dsp);
		getSamplesFromMixer(c_dsp);
		p = (p_write[0] + 1) & MAX_DELAY;
		STW(p_write, 0, p);
		outuint(c1, 0);
		outuint(c2, 0);
		outuint(c3, 0);
		outuint(c4, 0);
		result = biquad_cascade(dsp_out_samples[0], DSP_FILTERS, biquads[0], HEADROOMBITS);
		STW(audiobuf[0], p, result);
		result = audiobuf[0][(p - delay[0]) & MAX_DELAY];
		STW(dsp_out_samples, 0, result);
		inuint(c1);
		inuint(c2);
		inuint(c3);
		inuint(c4);
	}
}

#pragma unsafe arrays
void dsp(chanend c_dsp)
{
	chan c1, c2, c3, c4;
	par {
		dsp_main(c_dsp, c1, c2, c3, c4);
		dsp_sub(1, c1, biquads[1]);
		dsp_sub(2, c2, biquads[2]);
		dsp_sub(3, c3, biquads[3]);
		dsp_sub(4, c4, biquads[4]);
	}
}
