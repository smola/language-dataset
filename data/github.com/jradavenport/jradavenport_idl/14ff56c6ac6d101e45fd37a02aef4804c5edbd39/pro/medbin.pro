;+
;     MEDBIN
; a super sloppy XY median-binning code
; could be uber sped up w/ Histogram
;
;
; medbin,xx,yy,xout,yout,bin,start,stop
;
; xx,yy = inputdata
; xout,yout = output vectors
; bin = binsize
; start = smallest value along x-range
; stop = largest value along x-range
;
; program bins in x-range, then computes the median x and y values
; within each bin
;-
pro medbin,xin,yin,xout,yout,bin,bin0,bin1,nmin=nmin,std=std,npts=npts

if not keyword_set(bin0) then bin0 = min(xin)
if not keyword_set(bin1) then bin1 = max(xin)

nbin = (bin1-bin0)/bin+1

xout = [-1]
yout = [-1]
std = [-1]
npts = [-1]

if not keyword_set(nmin) then nmin = 5. ; min # of stars in each bin to calc median for

for n=0L,nbin-1 do begin
   z = where(xin ge bin0+bin*n and xin lt bin0+bin*(n+1.))
   if n_elements(z) lt nmin then continue

   npts = [npts, n_elements(z)]
   xout = [xout,median(xin[z],/even)]
   yout = [yout,median(yin[z],/even)]
   std = [std,stddev(yin[z],/nan)]
endfor

xout = xout[1:*] ; trim first dumb entry
yout = yout[1:*]
std = std[1:*]
  return
end
