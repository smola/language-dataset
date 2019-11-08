; docformat = 'rst'
;
;+
;
; :Categories:
;    IFSRED
;
; :Returns:
;    Fits file for mosaicing OSIRIS data with DRP.
;
; :Params:
;    outfile: in, required, type=str
;      Output directory.
;    xcent: in, required, type=dblarr
;      X-coordinate of galaxy centers, as measured in QL2.
;    ycent: in, required, type=dblarr
;      Y-coordinate of galaxy centers, as measured in QL2.
;
; :Keywords:
;
; :Author:
;    David S. N. Rupke::
;      Rhodes College
;      Department of Physics
;      2000 N. Parkway
;      Memphis, TN 38104
;      drupke@gmail.com
;
; :History:
;    ChangeHistory::
;      2013apr29, DSNR, created
;      2016sep08, DSNR, documented, copyrighted, licensed, generalized
;
; :Copyright:
;    Copyright (C) 2016 David S. N. Rupke
;
;    This program is free software: you can redistribute it and/or
;    modify it under the terms of the GNU General Public License as
;    published by the Free Software Foundation, either version 3 of
;    the License or any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;    General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see
;    http://www.gnu.org/licenses/.
;
;-
pro osiris_mosshift,outdir,xcent,ycent

  offsets = transpose([[ycent[0] - ycent],[xcent[0] - xcent]])
  mwrfits,offsets,outdir+'mosaic_all.fits',/silent,/create

end
