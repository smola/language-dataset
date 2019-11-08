# (c) 2016 Greg Lee

### Floating point math library ###

(load "@lib/native.l")

(gcc "fp" NIL

# String to/from floating point number conversions

   (r2sC (S1 X) r2s 'S '(NIL (257) . 0) S1 X)
   
   (s2r (X) s2r 'I X)

   (d2sC (S1 X) d2s 'S '(NIL (257) . 0) S1 X)
   
   (s2d (X) s2d 'N X)

# Conversions between integer, fixed, and floating point types

   (r2d (X) r2d 'N X)
   
   (d2r (X) d2r 'I X)
   
   (l2d (X) l2d 'N X)
   
   (d2l (X) d2l 'N X)
   
   (l2r (X) l2r 'I X)
   
   (r2l (X) r2l 'N X)
   
   (fix2d (X) fix2d 'N X *Scl)
   
   (d2fix (X) d2fix 'N X *Scl)
   
   (fix2r (X) fix2r 'I X *Scl)
   
   (r2fix (X) r2fix 'N X *Scl)

# Single precision math

   (rneg (X) rneg 'I X)
   
   (radd (X Y) radd 'I X Y)
   
   (rsub (X Y) rsub 'I X Y)
   
   (rmul (X Y) rmul 'I X Y)
   
   (rdiv (X Y) rdiv 'I X Y)
   
# Double precision math

   (dneg (X) dneg 'N X)
   
   (dadd (X Y) dadd 'N X Y)
   
   (dsub (X Y) dsub 'N X Y)
   
   (dmul (X Y) dmul 'N X Y)
   
   (ddiv (X Y) ddiv 'N X Y)
   
# Functions from math.h

   (dacos (X) dacos 'N X)
   
   (dasin (X) dasin 'N X)
   
   (datan (X) datan 'N X)
   
   (datan2 (X Y) datan2 'N X Y)
   
   (dcos (X) dcos 'N X)
   
   (dcosh (X) dcosh 'N X)
   
   (dsin (X) dsin 'N X)
   
   (dsinh (X) dsinh 'N X)
   
   (dtan (X) dtan 'N X)
   
   (dtanh (X) dtanh 'N X)
   
   (dexp (X) dexp 'N X)
   
   (dfrexpC (X E) dfrexp 'N X E)
      
   (dldexp (X Y) dldexp 'N X Y)
   
   (dlog (X) dlog 'N X)
   
   (dlog10 (X) dlog10 'N X)
   
   (dmodfC (X IP) dmodf 'N X IP)
      
   (dpow (X Y) dpow 'N X Y)
   
   (dsqrt (X) dsqrt 'N X)
   
   (dceil (X) dceil 'N X)
   
   (dfabs (X) dfabs 'N X)
   
   (dfloor (X) dfloor 'N X)
   
   (dfmod (X Y) dfmod 'N X Y)
   
# Constants from math.h

   (rhuge () rhuge 'I)
   
   (dm_e () dm_e 'N)
   
   (dlog2e () dlog2e 'N)
   
   (dlog10e () dlog10e 'N)
   
   (dln2 () dln2 'N)
   
   (dln10 () dln10 'N)
   
   (dpi () dpi 'N)
   
   (dpi_2 () dpi_2 'N)
   
   (dpi_4 () dpi_4 'N)
   
   (d1_pi () d1_pi 'N)
   
   (d2_pi () d2_pi 'N)
   
   (d2_sqrtpi () d2_sqrtpi 'N)
   
   (dsqrt2 () dsqrt2 'N)
   
   (dsqrt1_2 () dsqrt1_2 'N)
         
   (dhuge_val () dhuge_val 'N) )
      
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>

// union cast macro from Matt Gallagher
#define UNION_CAST(x, destType) \
 (((union {__typeof__(x) a; destType b;})x).b)


// conversions

// float <-> string

char *r2s( char *s, char *fmt, int i )
{
   sprintf( s, fmt, UNION_CAST( i, float ) );
   return s;
}

int s2r( char *s )
{
   float x;
   sscanf( s, "%f", &x );
   return UNION_CAST( x, int );
}

// double <-> string

char *d2s( char *s, char *fmt, long int i )
{
   sprintf( s, fmt, UNION_CAST( i, double ) );
   return s;
}

long int s2d( char *s )
{
   double x;
   sscanf( s, "%lf", &x );
   return UNION_CAST( x, long int );
}

// float <-> double

long int r2d( int i )
{
   float f = UNION_CAST( i, float );
   double d = ( double ) f;
   return UNION_CAST( d, long int );
}

int d2r( long int i )
{
   double d = UNION_CAST( i, double );
   float f = ( float ) d;
   return UNION_CAST( f, int );
}

// long int <-> double

long int l2d( long int i )
{
   double d = i;
   return UNION_CAST( d, long int );
}

long int d2l( long int i )
{
   long int l = UNION_CAST( i, double );
   return l;
}

// long int <-> float

int l2r( long int i )
{
   float f = i;
   return UNION_CAST( f, int );
}

long int r2l( int i )
{
   long int l = UNION_CAST( i, float );
   return l;
}

// fixed point <-> double

long int fix2d( long int i, int j )
{
   double d = i;
   d = d*pow( 10.0, -j );
   return UNION_CAST( d, long int );
}

long int d2fix( long int i, int j )
{
   double d = UNION_CAST( i, double );
   d = d*pow( 10.0, j );
   long int l = ( long int ) d;
   return l;
}

// fixed point <-> float

int fix2r( long int i, int j )
{
   float f = i;
   f = f*pow( 10.0, -j );
   return UNION_CAST( f, int );
}

long int r2fix( int i, int j )
{
   float f = UNION_CAST( i, float );
   f = f*pow( 10.0, j );
   long int l = ( long int ) f;
   return l;
}

// single precision arithmetic
int rneg( int i )
{
   float result = - UNION_CAST( i, float );
   return UNION_CAST( result, int );
}

int radd( int i, int j )
{
   float result = UNION_CAST( i, float ) + UNION_CAST( j, float );
   return UNION_CAST( result, int );
}

int rsub( int i, int j )
{
   float result = UNION_CAST( i, float ) - UNION_CAST( j, float );
   return UNION_CAST( result, int );
}

int rmul( int i, int j )
{
   float result = UNION_CAST( i, float ) * UNION_CAST( j, float );
   return UNION_CAST( result, int );
}

int rdiv( int i, int j )
{
   float result = UNION_CAST( i, float ) / UNION_CAST( j, float );
   return UNION_CAST( result, int );
}

// double precision arithmetic
long int dneg( long int i )
{
   double result = - UNION_CAST( i, double );
   return UNION_CAST( result, long int );
}

long int dadd( long int i, long int j )
{
   double result = UNION_CAST( i, double ) + UNION_CAST( j, double );
   return UNION_CAST( result, long int );
}

long int dsub( long int i, long int j )
{
   double result = UNION_CAST( i, double ) - UNION_CAST( j, double );
   return UNION_CAST( result, long int );
}

long int dmul( long int i, long int j )
{
   double result = UNION_CAST( i, double ) * UNION_CAST( j, double );
   return UNION_CAST( result, long int );
}

long int ddiv( long int i, long int j )
{
   double result = UNION_CAST( i, double ) / UNION_CAST( j, double );
   return UNION_CAST( result, long int );
}

// functions in math.h
long int dacos( long int i )
{
   double result = acos( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dasin( long int i )
{
   double result = asin( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int datan( long int i )
{
   double result = atan( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int datan2( long int i, long int j )
{
   double result = atan2( UNION_CAST( i, double ), UNION_CAST( j, double ) );
   return UNION_CAST( result, long int );
}

long int dcos( long int i )
{
   double result = cos( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dcosh( long int i )
{
   double result = cosh( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dsin( long int i )
{
   double result = sin( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dsinh( long int i )
{
   double result = sinh( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dtan( long int i )
{
   double result = tan( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dtanh( long int i )
{
   double result = tanh( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dexp( long int i )
{
   double result = exp( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dfrexp( long int i, int *exp )
{
   double x = frexp( UNION_CAST( i, double ), exp );
   return UNION_CAST( x, long int );
}

long int dldexp( long int i, int exponent )
{
   double result = ldexp( UNION_CAST( i, double ), exponent );
   return UNION_CAST( result, long int );
}

long int dlog( long int i )
{
   double result = log( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dlog10( long int i )
{
   double result = log10( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dmodf( long int i, long int *int_part )
{
   double x = modf( UNION_CAST( i, double ), ( double * ) int_part );
   return UNION_CAST( x, long int );
}

long int dpow( long int i, long int j )
{
   double result = pow( UNION_CAST( i, double ), UNION_CAST( j, double ) );
   return UNION_CAST( result, long int );
}

long int dsqrt( long int i )
{
   double result = sqrt( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dceil( long int i )
{
   double result = ceil( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dfabs( long int i )
{
   double result = fabs( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dfloor( long int i )
{
   double result = floor( UNION_CAST( i, double ) );
   return UNION_CAST( result, long int );
}

long int dfmod( long int i, long int j )
{
   double result = fmod( UNION_CAST( i, double ), UNION_CAST( j, double ) );
   return UNION_CAST( result, long int );
}

// constants from math.h
int rhuge( void )
{
   float result = HUGE;
   return UNION_CAST( result, int );
}

long int dm_e( void )
{
   double result = M_E;
   return UNION_CAST( result, long int );
}

long int dlog2e( void )
{
   double result = M_LOG2E;
   return UNION_CAST( result, long int );
}

long int dlog10e( void )
{
   double result = M_LOG10E;
   return UNION_CAST( result, long int );
}

long int dln2( void )
{
   double result = M_LN2;
   return UNION_CAST( result, long int );
}

long int dln10( void )
{
   double result = M_LN10;
   return UNION_CAST( result, long int );
}

long int dpi( void )
{
   double result = M_PI;
   return UNION_CAST( result, long int );
}

long int dpi_2( void )
{
   double result = M_PI_2;
   return UNION_CAST( result, long int );
}

long int dpi_4( void )
{
   double result = M_PI_4;
   return UNION_CAST( result, long int );
}

long int d1_pi( void )
{
   double result = M_1_PI;
   return UNION_CAST( result, long int );
}

long int d2_pi( void )
{
   double result = M_2_PI;
   return UNION_CAST( result, long int );
}

long int d2_sqrtpi( void )
{
   double result = M_2_SQRTPI;
   return UNION_CAST( result, long int );
}

long int dsqrt2( void )
{
   double result = M_SQRT2;
   return UNION_CAST( result, long int );
}

long int dsqrt1_2( void )
{
   double result = M_SQRT1_2;
   return UNION_CAST( result, long int );
}

long int dhuge_val( void )
{
   double result = HUGE_VAL;
   return UNION_CAST( result, long int );
}

/**/

# picolisp routines that provide default format arguments for float to string functions

(de r2s (X . @)
   (if (args)
      (r2sC (next) X) 
      (r2sC "%f" X) ) )

(de d2s (X . @)
   (if (args)
      (d2sC (next) X) 
      (d2sC "%lf" X) ) )

# picolisp routines for floating point math functions with multiple arguments

(de r+ (X . @) 
   (let ("Result" X)
      (while (args) 
        (setq "Result" (radd "Result" (next) ) ) )
      "Result") )

(de r- (X . @)
   (if (args)
      (let ("Result" X)
         (while (args) 
           (setq "Result" (rsub "Result" (next) ) ) )
        "Result")
      (let ("Result" (rneg X) )
         "Result") ) )
   
(de r* (X . @)
   (let ("Result" X)
      (while (args) 
        (setq "Result" (rmul "Result" (next) ) ) )
      "Result") )

(de r/ (X . @)
   (let ("Result" X)
      (while (args) 
        (setq "Result" (rdiv "Result" (next) ) ) )
      "Result") )

(de d+ (X . @)
   (let ("Result" X)
      (while (args) 
	     (setq "Result" (dadd "Result" (next) ) ) )
      "Result") )

(de d- (X . @)
   (if (args)
      (let ("Result" X)
         (while (args) 
	        (setq "Result" (dsub "Result" (next) ) ) )
	     "Result")
      (let ("Result" (dneg X) )
         "Result") ) )

(de d* (X . @)
   (let ("Result" X)
      (while (args) 
	     (setq "Result" (dmul "Result" (next) ) ) )
      "Result") )

(de d/ (X . @)
   (let ("Result" X)
      (while (args) 
	     (setq "Result" (ddiv "Result" (next) ) ) )
      "Result") )

# picolisp routine to return significand and exponent as list from frexp

(de dfrexp (X) 
   (prog2
      (setq "E" (native "@" "calloc" 'N 1 4))
      (list (dfrexpC X "E") (car (struct "E" '(I))))
      (native "@" "free" NIL "E")))
   
#picolisp routine to return fractional and integer parts as list from modf

(de dmodf (X)
   (prog2
      (setq "IP" (native "@" "calloc" 'N 1 8))
      (list (dmodfC X "IP") (car (struct "IP" '(N)))) 
      (native "@" "free" NIL "IP")))
   
# End of file

