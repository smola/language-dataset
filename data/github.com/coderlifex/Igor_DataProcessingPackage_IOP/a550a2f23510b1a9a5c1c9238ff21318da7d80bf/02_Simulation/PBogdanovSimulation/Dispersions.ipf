#pragma rtGlobals=1		// Use modern global access method.


//simple Bilayer Splitting after Anderson


Function BareDispBB(t0, t1, t2, tc, FermiLevel, kx, ky)
Variable t0, t1, t2, tc, FermiLevel, kx, ky
return -2*t0*(cos(kx)+cos(ky)) - 4*t1*cos(kx)*cos(ky) - 2*t2 *(cos(2*kx) +cos(2*ky)) - FermiLevel - 1/2*tc*((cos(kx)-cos(ky))/2) 
End

Function BareDispAB(t0, t1, t2, tc, FermiLevel, kx, ky)
Variable t0, t1, t2, tc, FermiLevel, kx, ky
return -2*t0*(cos(kx)+cos(ky)) - 4*t1*cos(kx)*cos(ky) - 2*t2 *(cos(2*kx) +cos(2*ky)) - FermiLevel + 1/2*tc*((cos(kx)-cos(ky))/2)
End

//Bilayer Splitting after Harrison and Markiewicz


Function DispBB(t0, t1, t2, delta, V0, Gamma, FermiLevel, kx, ky)
Variable t0, t1, t2, delta, V0, Gamma, FermiLevel, kx, ky
Variable R = sqrt(1 + Gamma^2 * (2 - (cos(kx)-cos(ky)) ))
Variable F = 1/R/(1+R) 
// Approximate parameters: delta = 0.137, Gamma = 1, V0 = 1
return -2*t0*(cos(kx)+cos(ky)) - 4*t1*cos(kx)*cos(ky) - 2*t2 *(cos(2*kx) +cos(2*ky)) - FermiLevel -V0 * (1-delta)^2 *F* (cos(kx)-cos(ky))^2
End

Function DispAB(t0, t1, t2, delta, V0, Gamma, FermiLevel, kx, ky)
Variable t0, t1, t2,delta, V0, Gamma, FermiLevel, kx, ky
Variable tc = 0
Variable R = sqrt(1 + Gamma^2 * (2 - (cos(kx)-cos(ky)) ))
Variable F = 1/R/(1+R) 
// Approximate parameters: delta = 0.137, Gamma = 1, V0 = 1
return -2*t0*(cos(kx)+cos(ky)) - 4*t1*cos(kx)*cos(ky) - 2*t2 *(cos(2*kx) +cos(2*ky)) - FermiLevel -V0 * (1+delta)^2 *F* (cos(kx)-cos(ky))^2
End