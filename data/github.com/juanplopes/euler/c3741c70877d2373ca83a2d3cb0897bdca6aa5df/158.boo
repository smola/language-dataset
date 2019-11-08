/*
I cheated. I mean, tried results upto 6 and found the formula. Let me explain.

Bruteforced all possible combinations and counted the decreasing-only, 1-incr,
2-incr, etc. for all of them. The result is shown below.

n  p0      p1       p2       p3       p4       p5
1: 26
2: 325     325
3: 2600    10400    2600
4: 14950   164450   164450   14950
5: 65780   1710280  4341480  1710280  65780
6: 230230  13123110 69529460 69529460 13123110 230230

The first column is obviously a Comb(26, n). But we are interested in the second
column value.

I infered it is the value of p0 * (0, 1, 4, 11, 26, 57)[n], i.e. the Eulerian
number sequence (A000295), that is defined by a(n) = 2**n - n - 1. So I just
calculated the eulerian sequence and the combination and found the maximum value

I feel ashamed, but at least I didn't copied it :D
*/
import System
import System.Linq.Enumerable

E = array(long, 27)
for n in range(27): 
    E[n] =  2**n - n - 1

C = matrix(long, 27, 27)
for n in range(27):
    for k in range(27):
        if n==k or k==0: C[n,k] = 1 
        elif k>n:        C[n,k] = 0
        else:            C[n,k] = C[n-1,k-1] + C[n-1,k]

answer = 0L
for i in range(3, 27):
    answer = Math.Max(answer, C[26,i]*E[i])
    
print answer
assert answer == 409511334375