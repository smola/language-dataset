; Copyright (C) 1998-2017 University of Oxford
;
; This source code is licensed under the GNU General Public License (GPL),
; Version 3.  See the file COPYING for more details.


;+
; NAME:
;     mie_single
;
; PURPOSE:
;     Calculates the scattering parameters of a series of particles using the
;     Mie scattering theory.
;
; CATEGORY:
;     EODG Mie routines
;
; CALLING SEQUENCE:
;     mie_single, Dx, Cm [, Dqv=Dqv] [, /DLM] [, mthread=mthread] $
;     [, /SILENT], Dqxt, Dqsc, Dqbk, Dg [, Xs1] [, Xs2] [, SPM]
;
; INPUTS:
;     Dx:      Particle size parameter(s)
;     Cm:      Complex refractive index of the particle(s)
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;     Dqv:     Cosines of scattering angles at which to compute the intensity
;              functions etc.
;     DLM:     If set the IDL DLM version of the algorithm will be used instead
;              of the IDL coded version.
;     mthread: Controls the number of threads which will be utilised by the DLM
;              version of the algorithm. If not set by default the code will use
;              1 thread. The behaviour of the code for different values of this
;              keyword is as follows:
;              mthread<=0 : !CPU.TPOOL_NTHREADS
;              mthread> 0 : Code will utilise the number of threads specified by
;                           mthread.
;              * BE AWARE * : Adding more threads than the number of physical
;                cores (hyperthreads do not count as physical cores) on the
;                system will not speed up the calculation.
;
;              * HAS NO EFFECT UNLESS DLM IS ALSO SET*
;     SILENT:  If set all warning messages issued by the code will be suppressed.
;
; OUTPUTS:
;     Dqxt:    Extinction efficiency
;     Dqsc:    Scattering efficiency
;     Dqbk:    Backscattering efficiency
;     Dg:      Asymmetry parameter
;
; OPTIONAL OUTPUTS:
;     Xs1:     First amplitude function - amplitude of light polarized in the
;              plane perpendicular to the directions of incident light.
;              propagation and observation. Xs1 is complex arrays of the same
;              dimension as Dqv and is only calculated if Dqv is specified.
;     Xs2:     Second amplitude function - amplitude of light polarized in the
;              plane parallel to the directions of incident light propagation
;              and observation. Xs2 is complex arrays of the same dimension as
;              Dqv and is only calculated if Dqv is specified.
;     SPM:     Scattering phase matrix elements F11 (SPM[0,*,*]), F33
;              (SPM[1,*,*]), F12 (SPM[2,*,*]), F34 (SPM[3,*,*]), where the 2nd
;              dimension is the same dimension as Dqv and the 3rd dimension is
;              the same dimension as Dx. Only calculated if Dqv is
;              specified.
;
; KEYWORD OUTPUTS:
;
; RESTRICTIONS:
;     The backscatter efficiency differs by 4 pi from the standard definition.
;     The user is directed to Bohren and Huffman - Absorption and Scattering of
;     Light by Small Particles (Wiley-VCH 1983) - Sec 4.6: "Radar Backscattering
;     Cross Section".
;
; MODIFICATION HISTORY:
;     G. Thomas, 1998: mie_uoc.pro (translation of mieint.f to IDL)
;     R. Grainger, 2001: mie_uoc_d.pro (Added support for arrays of particle
;         sizes and included calculation of phase function)
;     G. Thomas, Sep 2003: (Put into EODG routines format)
;     G. Thomas, Feb 2004: (Introduced explicit double precision numerical
;         values into all computational expressions)
;     G. Thomas, Apr 2005: (NmX assignment changed to type long)
;     G. Thomas, Apr 2005: (Added DLM keyword)
;     G. Thomas, Apr 2005: (Changed code to ensure Qbsc is always calculated for
;         the backscatter direction)
;     G. Thomas, Jun 2005: (Added calculation of phase function after calling
;         mie_dlm_single, since the DLM no longer returns it)
;     G. Thomas, Nov 2006: (Changed the calculation of the backscatter
;         efficiency to be done directly from the A and B values, rather than
;         from the intensity at 180 degrees. Also, fixed a small bug with
;         passing Dqv to the DLM)
;     A. Smith, 12 Oct 2007: (Fixed bug in non-DLM calculation of Dph)
;     G. Thomas, Jun 2007: (Bug fix in calculation of backscatter efficiency)
;     A. Smith, Nov 2008: (Added warning message for +ve Cm )
;     G. Thomas, Jul 2011: (Added mthread keyword, to make use of the
;         parallelised version of the Mie DLM code)
;     G. Thomas, Jul 2011: (Can't get parallelised mie DLM to work correctly;
;         S1 and S2 arrays corrupted in the Fortran. Giving up - mthread keyword
;         code commented out)
;     G. Thomas, Aug 2012 (Another bug fix to backscatter efficiency)
;     G. McGarragh, 29 Jul 2015 (Changed scattering phase function output to
;         scattering phase matrix output.)
;     G. McGarragh, 29 Jul 2015 (DLM output of the phase matrix SPM was fixed so
;         no need to calculate it here any more.)
;-

pro mie_single, Dx, Cm, Dqv=Dqv, DLM=DLM, mthread=mthread, SILENT=SILENT, $
                Dqxt, Dqsc, Dqbk, Dg, Xs1, Xs2, SPM

    Imaxx = 120000l
    RIMax = 2.5
    Itermax = long(Imaxx * RIMax)
    Imaxnp = 10000l ; Change this as required
    Sizes = n_elements(Dx)

    if imaginary(cm) gt 0d0 and not(keyword_set(SILENT)) then $
        message, /continue, 'Warning: Imaginary part of refractive index '+ $
            'should be negative for absorbing particles. Set /SILENT to '+ $
            'hide this message.'

    if keyword_set(DLM) then begin
;   If the DLM keyword is set, use the DLM version of the code
    DxArr = dblarr(Sizes)
    DxArr[*] = Dx
    DCm = dcomplex(Cm)

;   Put the mthread keyword into the right form for the DLM call...
    if keyword_set(mthread) gt 0 then begin
        if mthread lt 1 then mthrd = !CPU.TPOOL_NTHREADS $
        else mthrd = mthread
    endif else mthrd = 1

    if n_elements(Dqv) gt 0 then begin
        Inp = n_elements(Dqv)
        mie_dlm_single, DxArr, DCm, Dqv=double(Dqv), Dqxt, Dqsc, Dqbk, $
                     Dg, Xs1, Xs2, F11, F33, F12, F34; , mthread=mthrd
        SPM = dblarr(4,Inp,Sizes)
        for i = 0,Sizes-1 do begin
            AA = 2d0 / (DxArr[i]^2 * Dqsc[i])
            SPM[0,*,*] = F11
            SPM[1,*,*] = F33
            SPM[2,*,*] = F12
            SPM[3,*,*] = F34
        endfor

;       mie_dlm_single, DxArr, DCm, Dqv=double(Dqv), Dqxt, Dqsc, Dqbk, $
;                    Dg, Xs1, Xs2, mthread=mthrd
;       Cannot get the DLM to return the phase function. Very mysterious...
;       So must calculate the elements of SPM below.
;       SPM = dblarr(4,Inp,Sizes)
;       for i = 0,Sizes-1 do begin
;           AA = 2d0 / (DxArr[i]^2 * Dqsc[i])
;           SPM[0,*,i] =  AA * double(Xs1[*,i]*conj(Xs1[*,i]) + $
;                                     Xs2[*,i]*conj(Xs2[*,i]))
;           SPM[1,*,i] =  AA * double(Xs1[*,i]*conj(Xs2[*,i]) + $
;                                     Xs2[*,i]*conj(Xs1[*,i]))
;           SPM[2,*,i] = -AA * double(Xs1[*,i]*conj(Xs1[*,i]) - $
;                                     Xs2[*,i]*conj(Xs2[*,i]))
;           SPM[3,*,i] = -AA * double((Xs1[*,i]*conj(Xs2[*,i]) - $
;                                      Xs2[*,i]*conj(Xs1[*,i])) * $
;                                      complex(0.0d, 1.0d))
;       endfor
    endif else begin
        mie_dlm_single, DxArr, DCm, Dqxt, Dqsc, Dqbk, Dg;, mthread=mthrd
    endelse

    endif else begin ;No DLM? Do everything in IDL

    if n_elements(Dqv) gt 0 then begin
        tmp = where(Dqv eq -1.0,bktheta)
        if bktheta eq 0 then Dqv2 = [Dqv,-1d] else Dqv2 = Dqv
        Inp  = n_elements(Dqv)
        Inp2 = n_elements(Dqv2)
        ph = dblarr(Inp)
        Xs1 = complexarr(Inp,Sizes)
        Xs2 = complexarr(Inp,Sizes)
        SPM = dblarr(4,Inp,Sizes)
    endif else begin
        Inp = 1
        Inp2 = Inp
        Dqv2 = [-1d0]
    endelse

    Dqxt = dblarr(Sizes)
    Dqsc = dblarr(Sizes)
    Dqbk = dcomplexarr(Sizes)
    Dg = dblarr(Sizes)

    for Size = 0l, Sizes - 1 do begin

;       if (Dx(Size) gt Imaxx) then $
;           message, 'Error: Size Parameter Overflow in Mie'
        Ir = 1.D0 / Cm
        Y =  Dx(Size) * Cm

        if (Dx(Size) lt 0.02) then NStop = 2 else begin
            case 1 OF
                (Dx(Size) le 8.0)    : $
                    NStop = Dx(Size) + 4.00*Dx(Size)^(1./3.) + 2.0
                (Dx(Size) lt 4200.0) : $
                    NStop = Dx(Size) + 4.05*Dx(Size)^(1./3.) + 2.0
                else                 : $
                    NStop = Dx(Size) + 4.00*Dx(Size)^(1./3.) + 2.0
            endcase
        end
        NmX = long(max([NStop,abs(Y)]) + 15.)
        D = dcomplexarr(Nmx+1)

        for N = Nmx-1,1,-1 do begin
            A1 = (N+1) / Y
            D(N) = A1 - 1/(A1+D(N+1))
        end

        Sm = dcomplexarr(Inp2)
        Sp = dcomplexarr(Inp2)
        Pi0 = dcomplexarr(Inp2)
        Pi1 = dcomplex(replicate(1.D0,Inp2),replicate(0.D0,Inp2))

        Psi0 = cos(Dx(Size))
        Psi1 = sin(Dx(Size))
        Chi0 =-sin(Dx(Size))
        Chi1 = cos(Dx(Size))
        Xi0 = dcomplex(Psi0,Chi0)
        Xi1 = dcomplex(Psi1,Chi1)

        Dg(Size) = 0.D0
        Dqsc(Size) = 0.D0
        Dqxt(Size) = 0.D0
        Dqbk(Size) = 0.D0
        Tnp1 = 1D0

        for N = 1l,NStop do begin
            DN = double(N)
            Tnp1 = Tnp1 + 2D0
            Tnm1 = Tnp1 - 2D0
            A2 = Tnp1 / (DN*(DN+1.D0))
            Turbo = (DN+1.D0) / DN
            Rnx = DN/Dx(Size)
            Psi = double(Tnm1)*Psi1/Dx(Size) - Psi0
            Chi = Tnm1*Chi1/Dx(Size)       - Chi0
            Xi = dcomplex(Psi,Chi)
            A = ((D[N]*Ir+Rnx)*Psi-Psi1) / ((D[N]*Ir+Rnx)*  Xi-  Xi1)
            B = ((D[N]*Cm+Rnx)*Psi-Psi1) / ((D[N]*Cm+Rnx)*  Xi-  Xi1)
            Dqxt(Size) = Tnp1 * double(A + B)                 + Dqxt(Size)
            Dqsc(Size) = Tnp1 * double(A*conj(A) + B*conj(B)) + Dqsc(Size)
            if (N gt 1) then Dg(Size) = Dg(Size) $
                + (dN*dN - 1) * double(ANM1*conj(A) + BNM1 * conj(B)) / dN $
                + TNM1 * double(ANM1*conj(BNM1)) / (dN*dN - dN)
            Anm1 = A
            Bnm1 = B

            S = Dqv2 * Pi1
            T = S - Pi0
            if arg_present(Dqbk) or n_elements(dph) gt 0 then begin
                Taun = N*T - Pi0
                Sp = (A2 * (A + B)) * (Pi1 + Taun) + Sp
                Sm = (A2 * (A - B)) * (Pi1 - Taun) + Sm
            endif
            Pi0 = Pi1
            Pi1 = S + T*Turbo

            Psi0 = Psi1
            Psi1 = Psi
            Chi0 = Chi1
            Chi1 = Chi
            Xi1 = dcomplex(Psi1,Chi1)

        end; for NStop

        if (Dg(Size) gt 0) then Dg(Size) = 2D0 * Dg(Size) / Dqsc(Size)

;       The following lines are not needed unless Dqv was set
        if n_elements(Dqv) gt 0 then begin
            Xs1[*,Size] = ((Sp[0:Inp-1] + Sm[0:Inp-1]) / 2D0)
            Xs2[*,Size] = ((Sp[0:Inp-1] - Sm[0:Inp-1]) / 2D0)
            SPM[0,*,Size] =  double(Xs1[*,Size]*conj(Xs1[*,Size]) + $
                                    Xs2[*,Size]*conj(Xs2[*,Size])) / Dqsc(Size)
            SPM[1,*,Size] =  double(Xs1[*,Size]*conj(Xs2[*,Size]) + $
                                    Xs2[*,Size]*conj(Xs1[*,Size])) / Dqsc(Size)
            SPM[2,*,Size] = -double(Xs1[*,Size]*conj(Xs1[*,Size]) - $
                                    Xs2[*,Size]*conj(Xs2[*,Size])) / Dqsc(Size)
            SPM[3,*,Size] = -double((Xs1[*,Size]*conj(Xs2[*,Size]) - $
                                     Xs2[*,Size]*conj(Xs1[*,Size])) * $
                                              complex(0.0d, 1.0d)) / Dqsc(Size)
        endif

        if arg_present(Dqbk) then $
            Dqbk(Size) = ( Sp[Inp2-1] + Sm[Inp2-1] ) / 2d0

    endfor ; end of size loop

    Dqsc = 2D0 * Dqsc / Dx^2
    Dqxt = 2D0 * Dqxt / Dx^2
    if arg_present(Dqbk) then $
        Dqbk = 4d0* double(Dqbk*conj(Dqbk)) / Dx^2

    endelse ; end of if DLM keyword
end
