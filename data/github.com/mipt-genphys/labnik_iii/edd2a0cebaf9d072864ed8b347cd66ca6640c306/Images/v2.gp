set terminal postscript; set output "4.ps"
#set terminal texdraw; set output "1.txt"

set nokey

set samples 500

#set label "<R>=560,7" at 580,0.06
#set label "S^2=89,89" at 580,0.055
#set label "S=9,48" at 580,0.05

w=2*pi
g=0.1*w

k=1
v=2*k
a=2

sh(x)=(exp(x)-exp(-x))/2
ch(x)=(exp(x)+exp(-x))/2


#f(x)=-exp(-g*x)*sin(w*x)

#g(x)=exp(-g*x)*(cos(w*x)+(g/w)*sin(w*x))

#f(x)=-a*exp(-v*x)*sh(k*x)

#f(x)=exp(-v*x)*(ch(k*x)+(v/k)*sh(k*x))

g(x)=x*exp(-x)

#g(x)=(1+x)*exp(-x)


#set multiplot

#plot [0:4][-1:1] f(x)

plot [0:5][0:1] g(x)

# with lines lt 2 lw 1

#set nomultiplot

pause -1

