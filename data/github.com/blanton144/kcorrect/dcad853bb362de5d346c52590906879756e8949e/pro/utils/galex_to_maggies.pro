;+
; NAME:
;   galex_to_maggies
; PURPOSE:
;   convert GALEX catalog input to Galactic-extinction corrected AB maggies 
; CALLING SEQUENCE:
;   galex_to_maggies,galex,maggies,ivar
; INPUTS:
;   galex - [N] GALEX "mcat" style input, with:
;            .ALPHA_J2000
;            .DELTA_J2000
;            .NUV_A_IMAGE
;            .NUV_B_IMAGE
;            .NUV_KRON_RADIUS
;            .NUV_FLUX_AUTO
;            .NUV_SKYBG
;            .NUV_WEIGHT
;            .NUV_MAG_AUTO
;            .NUV_ZERO_POINT
;            .NUV_EXTINCTION
;            .NUV_FLUX_APER_N [in ORDER!]
;            .FUV_A_IMAGE
;            .FUV_B_IMAGE
;            .FUV_KRON_RADIUS
;            .FUV_FLUX_AUTO
;            .FUV_SKYBG
;            .FUV_WEIGHT
;            .FUV_MAG_AUTO
;            .FUV_ZERO_POINT
;            .FUV_EXTINCTION
;            .FUV_FLUX_APER_N [in ORDER!]
;  
; OUTPUTS:
;   maggies - [2, N] output in AB maggies in FUV and NUV filters
;   ivar - [2, N] inverse variance of maggies
;
; OPTIONAL INPUTS:
;   aper - set to 1-7 to use GALEX aperture magnitudes
;           (diameters 2, 3, 5, 8, 12, 17, 23 pixels)
;   apcor - [2] set for FUV, NUV to apply an aperture correction (x). 
;            Default 1.0
;   zero_point - [2] set to pass different zero point FUV, NUV
;            Default: FUV 18.82, NUV: 20.08
;   galext - Use the galex default ratio for afuv/ebv and anuv/ebv
;        
; COMMENTS:
;   It also ALWAYS applies a minimum error of [0.02, 0.02] in [FUV, NUV]
;
;   If the GALEX structure has a .E_BV entry, we use that for the
;   GALEX extinction, but we call dust_getval() to read the SFD maps
;   otherwise.
; REVISION HISTORY:
;   07-Apr-2005  Mike Blanton, NYU
;   23-June-2005 David Schiminovich, Columbia
;           added some fixes for errors, etc
;   20-July-2005 David Schiminovich, Columbia
;           added aperture photometry
;-
;------------------------------------------------------------------------------
pro galex_to_maggies, galex, maggies, ivar,aper=aper,apcor=apcor, $
                      galext=galext,zero_point=zero_point, nodust=nodust

common com_galex_to_maggies, nuv_leff, fuv_leff, extvoebv, nuv_extoextv, $
  fuv_extoextv

;
; Prepare for aperture photometry
;

if (n_elements(zero_point) eq 2) then begin
    fuv_zero_point=zero_point[0]
    nuv_zero_point=zero_point[1]
endif else begin
    fuv_zero_point=18.82
    nuv_zero_point=20.08
endelse

if (n_elements(aper) ne 1) then aper=0
aper=fix(aper)
if (aper ge 1) and (aper le 7) then begin
    diamarr=[2,3,5,8,12,17,23]
    diam=diamarr[aper-1]
    tstr=tag_names(galex)

    pnuv=tag_indx(galex[0],'NUV_FLUX_APER_1')
    pfuv=tag_indx(galex[0],'FUV_FLUX_APER_1')

    if (pnuv eq -1) or (pfuv eq -1) then begin
        print,'GALEX structure does not contain aperture magnitudes!'
        stop
    endif

    if (n_elements(apcor) ne 2) then apcor=replicate(1.0,2)

endif 

;
; Handle Galactic reddening
;
if(NOT keyword_set(nodust)) then begin
    if (keyword_set(galext) ne 1) then galext=0
    if (galext eq 1)  then begin
; Use GALEX default values A_FUV/EBV = 8.29, A_NUV/EBV = 8.18
        extvoebv=3.10
        nuv_extoextv=8.18/extvoebv
        fuv_extoextv=8.29/extvoebv
    endif else begin
        if(n_elements(nuv_leff) eq 0) then begin
            nuv_leff=k_lambda_eff(filterlist='galex_NUV.par')
            fuv_leff=k_lambda_eff(filterlist='galex_FUV.par')
        endif
        extvoebv=3.10
        nuv_extoextv=(ext_ccm(nuv_leff))[0]
        fuv_extoextv=(ext_ccm(fuv_leff))[0]
    endelse
    
    iebv=tag_indx(galex[0], 'e_bv')
    if(iebv eq -1) then begin
        glactc, galex.alpha_j2000, galex.delta_j2000, 2000., gl, gb, 1, /deg
        ebv=dust_getval(gl,gb,/interp,/noloop)
    endif else begin
        ebv=galex.e_bv
    endelse

;Blanton values used here are a_fuv=8.15, anuv=9.17
    
    nuv_extinction=ebv*nuv_extoextv*extvoebv
    fuv_extinction=ebv*fuv_extoextv*extvoebv
endif else begin
    nuv_extinction=0.
    fuv_extinction=0.
endelse

;
; Convert fluxes to maggies, calculate errors
;

minfuverror=0.02
minnuverror=0.02

maggies=fltarr(2,n_elements(galex))
ivar=fltarr(2,n_elements(galex))

;
; Aperture photometry
;

if (aper ge 1) and (aper le 7) then begin

    maggies[0,*]=0.
    ivar[0,*]=0.
    igood=where(galex.fuv_mag_auto ne -999. and galex.fuv_mag_auto ne -99., $
                ngood)
    fuv_flux_aper=galex[igood].(pfuv+aper-1)

    if(ngood gt 0) then begin
        delta=0.048
        area_aper=2.25*!PI*diam^2 ; in sq arcsec

        aper_fluxerr=sqrt((delta*fuv_flux_aper)^2+ $
                          ((fuv_flux_aper+galex[igood].fuv_skybg*area_aper)* $
                           galex[igood].fuv_weight)/galex[igood].fuv_weight^2)
        maggies[0,igood]=10.^(-0.4*((-2.5)*alog10(fuv_flux_aper)+ $
                                    fuv_zero_point-fuv_extinction[igood]))* $
          apcor[0]
        ivar[0,igood]= $
          1./(((aper_fluxerr/fuv_flux_aper)^2+minfuverror^2)* $
              (maggies[0,igood])^2)
    endif 

    
    maggies[1,*]=0.
    ivar[1,*]=0.
    igood=where(galex.nuv_mag_auto ne -999. and galex.nuv_mag_auto ne -99., $
               ngood)
    nuv_flux_aper=galex[igood].(pnuv+aper-1)

    if(ngood gt 0) then begin
        delta=0.048
        area_aper=2.25*!PI*diam^2 ; in sq arcsec

        aper_fluxerr=sqrt((delta*nuv_flux_aper)^2+ $
                          ((nuv_flux_aper+galex[igood].nuv_skybg*area_aper)* $
                           galex[igood].nuv_weight)/galex[igood].nuv_weight^2)
        maggies[1,igood]=10.^(-0.4*((-2.5)*alog10(nuv_flux_aper)+ $
                                    nuv_zero_point-nuv_extinction[igood]))* $
          apcor[1]
        ivar[1,igood]= $
          1./(((aper_fluxerr/nuv_flux_aper)^2+minnuverror^2)* $
              (maggies[1,igood])^2)
    endif 


endif

;
; Auto mags
;

if (aper eq 0) then begin

; use auto (Kron) magnitudes

    maggies[0,*]=0.
    ivar[0,*]=0.
    igood=where(galex.fuv_mag ne -999. and galex.fuv_mag ne -99. and $
                galex.fuv_magerr ne 0., ngood)
    if(ngood gt 0) then begin
        delta=0.048
        area_auto=2.25*!PI*galex[igood].fuv_a_image* $
          galex[igood].fuv_b_image*galex[igood].fuv_kron_radius^2	
        auto_fluxerr=sqrt((delta*galex[igood].fuv_flux_auto)^2+ $
                          ((galex[igood].fuv_flux_auto+ $
                            galex[igood].fuv_skybg*area_auto)* $
                           galex[igood].fuv_weight)/galex[igood].fuv_weight^2)
        maggies[0,igood]=10.^(-0.4*(galex[igood].fuv_mag_auto+fuv_zero_point- $
                                    fuv_extinction[igood]))
        ivar[0,igood]= $
          1./(((auto_fluxerr/galex[igood].fuv_flux_auto)^2+minfuverror^2)* $
              maggies[0,igood]^2)
    endif 

    maggies[1,*]=0.
    ivar[1,*]=0.
    igood=where(galex.nuv_mag ne -999. and galex.nuv_mag ne -99. and $
                galex.nuv_magerr ne 0., ngood)
    if(ngood gt 0) then begin
        delta=0.028
        area_auto=2.25*!PI*galex[igood].nuv_a_image* $
          galex[igood].nuv_b_image*galex[igood].nuv_kron_radius^2	
        auto_fluxerr=sqrt((delta*galex[igood].nuv_flux_auto)^2+ $
                          ((galex[igood].nuv_flux_auto+ $
                            galex[igood].nuv_skybg*area_auto)* $
                           galex[igood].nuv_weight)/galex[igood].nuv_weight^2)
        
;; work with flux err, not mag err.  Some problem with Mark's magerr formula
        
        maggies[1,igood]=10.^(-0.4*(galex[igood].nuv_mag_auto+$
                                    nuv_zero_point-nuv_extinction[igood]))
        ivar[1,igood]= 1./(((auto_fluxerr/galex[igood].nuv_flux_auto)^2+ $
                            minnuverror^2)*maggies[1,igood]^2)
    endif
endif
end

