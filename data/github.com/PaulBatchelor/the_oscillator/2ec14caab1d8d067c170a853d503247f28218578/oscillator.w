@* Introduction.

This document will describe an implementation of a classic table-lookup
oscillator with linear interpolation. This algorithm has it's direct origins
with the Csound opcode {\it oscili}. It is very well possible that the algorithm
goes back even farther to the original MUSIC N languages developed by Max
Matthews.

The motivation for such a writeup comes from prior work debugging oscillators
in Soundpipe. This algorithm has been the source of many hard to catch bugs.
This algorithm is a largely fixed-point algorithm, full of many confusing
and unintuitive bitwise operations and
"magic" constants. Furthermore, the original comments describing some of
these constants were ambiguous. (In Soundpipe, they were originally
interpretted the wrong way! More on this later.) To conquer, one must
understand. This document will aim to provide a satisfactory explanation of
what exactly is going on.

This document is what is known as a {\it literate program}. Text here is written
in a special kind of markup known as CWEB. This markup compiles down to both
C and TeX code. The program, as such, is tightly coupled with the documentation
used to describe it.

The core sections of the algorithm will be listed below.

@c
@<The Data Structures@>@/
@<Creation of F...@>@/
@<Destruction of F...@>@/
@<Generate Sine Table@>@/
@<Oscillator Initialization@>@/
@<Oscillator Computation@>@/
@<The Main Loop@>@/

@* The Main Loop.
This is the main loop. It is a good entry point for understanding this code.
This program will generate a 5 seconds of a 440Hz sine wave and write it
to a file called "sine.wav". The audio file is written using the open source
library libsndfile.

The sections of the main loop are as follows:

\item{$\bullet$} |@<Main Setup@>| initializes all the data, and sets constants.
\item{$\bullet$} |@<Inner...@>| is the portion of the program that
computes the audio samples and writes them to disk.
\item{$\bullet$} |@<Cleanup@>| frees memory allocated and closes the
filehandle.

@<The Main Loop@>=
int main(int argc, char *argv[])
{
    ftable ft;
    oscillator osc;
    unsigned int sr;
    unsigned int bufsize;
    unsigned int s;
    float *buf;
    SNDFILE *snd;
    SF_INFO info;
    @/@<Main Setup@>@/
    @<Inner...@>@/
    @<Cleanup@>@/
    return 0;
}

@ Before the program can begin writing audio samples, it needs to set up
a few things:

\item{$\bullet$} |@<Allocate B...@>|: allocate a buffer to store audio samples.
\item{$\bullet$} |@<Open Audio...@>|: open up an audio file to write to.
\item{$\bullet$} |@<Initialize Func...@>|: Initialize function table to be
referenced by lookup oscillator.
\item{$\bullet$} |@<Initialize Oscil...@>|: Initialize oscillator.

These will be expanded upon in further sections.

@<Main Setup@>=

sr = 44100;
@/@<Allocate...@>@/
@<Open Audio...@>@/
@<Initialize Function...@>@/
@<Initialize Oscil...@>@/

@ To simplify things, computed samples are saved in a giant buffer, which
is then written to the audio file in one go. In this case, the buffer size
is the entire duration of the audio file, which is set to be 5 seconds. This
buffer is allocated via |malloc()|.

@<Allocate Buffer@>=
bufsize = sr * 5;
buf = malloc(sizeof(float) * bufsize);

@ A file called "sine.wav" is created and opened via libsndfile. Error handling
is not implemented here to simplify the code.

@<Open Audio File@>=
memset(&info, 0, sizeof(SF_INFO));
info.samplerate = sr;
info.channels = 1;
info.format = SF_FORMAT_WAV | SF_FORMAT_PCM_24;
snd = sf_open("sine.wav", SFM_WRITE, &info);

@ The table of size 8192 is created in order to be used by the oscillator.

First, memory is allocated and initialized via |ftbl_create|. Consult the
section |@<Creation of Fun...@>| to see what happens in this function.

Next, A single sinusoidal period is sampled onto the ftable with the function
|gen_sine()|. See the section |@<Generate Sine Table@>| to see how this works.

@<Initialize Function Table@>=
ftbl_create(&ft, sr, 8192);
gen_sine(&ft);

@ This portion of code initializes the parameters for the oscillator. The
frequency and amplitude are also set to 440 and 0.8, respectively.
@<Initialize Oscillator@>=
osc_init(&osc, &ft, 0.f);
osc.freq = 440.f;
osc.amp = 0.8f;

@ Put words here. @<Inner Loop Computation@>=
for(s = 0; s < bufsize; s++) {
    buf[s] = osc_compute(&osc);
}
sf_write_float(snd, buf, bufsize);

@ Put words here. @<Cleanup@>=
ftbl_destroy(&ft);
free(buf);
sf_close(snd);

@* The Data Structures.
@d MAXLEN 0x1000000L
@d PHMASK 0x0FFFFFFL

@<The Data Structures@>=
#include <sndfile.h>
#include <stdint.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

@<Function Table@>@/
@<Oscillator Data@>@/

@ The function table data struct holds the data used by the table lookup
oscillator, along with a few constants used by the oscillator algorithm.
For more information on these values, see {|@<Creation of Function Table@>|}.
\item{$\bullet$} size: the ftable size.
\item{$\bullet$} lobits: lower bits constant.
\item{$\bullet$} lomask: a value that turns all the bits on from 0 to
{\tt lobits}.
\item{$\bullet$} lodiv: The inverted value lobits, used for scaling.
\item{$\bullet$} sicvt: a constant for getting phase increment amount.
\item{$\bullet$} tbl: the actual table data.

@<Function Table@>=
typedef struct {
    size_t size;
    uint32_t lobits;
    uint32_t lomask;
    float lodiv;
    float sicvt;
    float *tbl;
} ftable;

@ The oscillator data struct has the following variables.
\item{$\bullet$} freq: the oscillator frequency.
\item{$\bullet$} amp: the amplitude
\item{$\bullet$} iphs: the initial phase, scaled in the range 0-1
\item{$\bullet$} lphs: last phase, scaled as a signed 32-bit integer
\item{$\bullet$} tbl: an ftable data struct |@<Function Table@>|, which
holds the lookup table.
\item{$\bullet$} inc: the phase increment amount.

@<Oscillator Data@>=
typedef struct {
    float freq, amp, iphs;
    int32_t lphs;
    ftable *tbl;
    int inc;
} oscillator;

@* Function Table.

@ This is where the function table data is allocated and constants initalized.
Many of these values are constants in bitwiddling parts of the algorithm.

\item{$\bullet$} lobits: the number lower bits. This is derived with the
equation $\log_2({\tt MAXLEN} / size)$, where {\tt MAXLEN} is the maximum
length an ftable can have (this is set to be $2^{28}$). This values tells
how much to rightshift a number. The smaller the value, the more bits to
right shift. Because lobits can only be a whole integer value, this places a
limitation that the ftable size can only be a power of 2. For this reason,
a handmade log2 function can be used instead the standard C library log2
function.
\item{$\bullet$} lomask: a value that turns all the bits on from 0 to
{\tt lobits}. Derived using the equation: $2^{lobits} - 1$.
\item{$\bullet$} lodiv: This inverts lobits. It used to scale the phase
position between 0 and 1. $1 / 2^{lobits}$
\item{$\bullet$} sicvt: a constant, whose cryptic name comes from the original
csound source code. This variable stands for Samplting Inrement ConVert.
This is a floating point variable which converts the fixed-point table lookup
amount to seconds. This value gets scaled by the oscillator frequency to
get the phase increment. This variable is derived using ${{\tt MAXLEN}/srate}$.

@<Creation of Function Table@>=
void ftbl_create(ftable *ft, unsigned int sr, size_t size)
{
    size_t tmp;
    ft->size = size;
    ft->tbl = malloc(sizeof(float) * (size + 1));
    memset(ft->tbl, 0, sizeof(float) * (size + 1));
    ft->sicvt = 1.0 * MAXLEN / sr;
    tmp = MAXLEN / size;
    ft->lobits = 0;
    while(tmp >>= 1) ft->lobits++;
    ft->lomask = (1<<ft->lobits) - 1;
    ft->lodiv = 1.0 / (1<<ft->lobits);
}

@ Put words here. @<Destruction of Function Table@>=
void ftbl_destroy(ftable *ft)
{
    free(ft->tbl);
}

@ This function samples a single sinusoidal period into a table.
@<Generate Sine Table@>=

void gen_sine(ftable *ft)
{
    size_t i;
    float step = 2 * M_PI / ft->size;
    for(i = 0; i < ft->size; i++){
        ft->tbl[i] = sin(i * step);
    }
}


@* The Table Lookup Oscillator.
@ This function initializes the oscillator data struct |@<Oscillator Data@>|.
It sets some reasonable defaults and zeros some variables. In addition, it
also sets the initial phase of the oscillator in a variable called |iphs|.
This variable is a value between 0 and 1.
@<Oscillator Initialization@>=

void osc_init(oscillator *osc, ftable *ft, float iphs)
{
    osc->freq = 440.0;
    osc->amp = 0.2;
    osc->tbl = ft;
    osc->iphs = fabs(iphs);
    osc->inc = 0;
    if (osc->iphs >= 0){
        osc->lphs = ((int32_t)(osc->iphs * MAXLEN)) & PHMASK;
    }
}

@ This function computes one sample of audio from the oscillator. It is the
core of the table lookup oscillator algorithm.

@<Oscillator Computation@>=

float osc_compute(oscillator *osc)
{
    @<Set Up Local Variables@>@/
    @<Calculate Phase Increment Amount@>@/

    @<Calculate fractional interpolation and sample points@>@/

    @<Compute Oscillator Sample@>@/

    @<Oscillator Wrap Up@>@/
    return out;
}

@ Local variables are set up and initialized. The table (|ftp|),
last phase |phs|, table data |ft|, and |sicvt| are set.

@<Set Up Local Variables@>=
ftable *ftp;
float fract, v1, v2, *ftab, *ft;
float out;
int32_t phs;
float sicvt = osc->tbl->sicvt;

ftp = osc->tbl;
phs = osc->lphs;
ft = osc->tbl->tbl;

@ The increment amount tells how much further to move the read pointer in
the table. This increment amount is based on the current oscillator frequency
and the variable |sicvt|.

@<Calculate Phase Increment Amount@>=
osc->inc = (int32_t)(osc->freq * sicvt);

@
The meat of the algorithm lies in this section. First, the fractional
interpolation amount is computed. Next, the values in the oscillator are
"looked up" from the wavetable (earning the name {\it table-lookup oscillator}).

{\it Linear interpolation} is a way of finding a value in between two discrete
values. It can be described in the following equation:

$$
y = (1 - \alpha) x_1 + \alpha x_2
$$

Where $x_1$ and $x_2$ are two values, and $\alpha$ is a fractional value
between 0 and 1. The $\alpha$ values determines the distribution balance of
the two values. When $\alpha = 0$, it is $x_1$, and when $\alpha = 1$, the
value is $x_2$.

These three are derived in the section of code below. In the original code,
$\alpha$ is |fract|, $x_1$ is |v1|, and $x_2$ is |v2|.

The fractional value is obtained by taking the lower bits portion of the current
fixed-point phase position, and normalizing it to be a floating-point value
between 0 and 1.

@<Calculate fractional interpolation and sample points@>=
fract = (phs & ftp->lomask) * ftp->lodiv;
ftab = ft + (phs >> ftp->lobits);
v1 = ftab[0];
v2 = ftab[1];

@ This is where the linear interpolation is computed
and scaled by the amplitude. The C expression used to the compute linear
interpolation has been re-arranged to minimize the number of operations used:

$$
\eqalign{
(1 - \alpha)x_1 + \alpha x_2 \cr
x_1 - \alpha x_1 + \alpha x_2 \cr
x_1 + \alpha x_2 - \alpha x_1 \cr
x_1 + (x_2 - x_1)\alpha
}
$$

Replacing the mathematical values with the C variable names, one gets the
expression: $${\tt v1 + (v2 - v1) * fract}$$

The new expression removes a multiply. Back when in the MUSIC N days when
things took days and weeks to render, every little optimization counted.
This value is then scaled by the oscillators amplitude |amp|.

@<Compute Oscillator Sample@>=
out = (v1 + (v2 - v1) * fract) * osc->amp;

@ Put words here. @<Oscillator Wrap Up@>=
phs += osc->inc;
phs &= PHMASK;
osc->lphs = phs;
