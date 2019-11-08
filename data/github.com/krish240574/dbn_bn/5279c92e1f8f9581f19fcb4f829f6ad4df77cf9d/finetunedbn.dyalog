 bn_finetunedbm ctr;one;tmp1;kk;topmosthhat;errderivative;dout;da_bn;dgamma;dbeta;dx;da;dw;db;ii;thhat;ddxa;ddxh
 ⍝ forward-prop
 ⍝ calculate hhat till the top layer
 2 bn_calclatesthhat g_numlayers
 lhhat←(1,g_isz)⍴,⊃g_hhatarr[g_numlayers;]

 topmosthhat←g_binlabels[ctr;] ⍝ ahh-haa !
 one←(topmosthhat=1)/⍳⍴topmosthhat
 errderivative←(1 1⍴,(one-1)-g_yhat[ctr;one])

 g_b[g_numlayers;]←((1,g_isz)⍴g_b[g_numlayers;])+g_lr×errderivative
 g_w[g_numlayers;;]←g_w[g_numlayers;;]+g_lr×errderivative+.×⍉lhhat
 ii←g_numlayers
 :While ii≥1
     ddxh←(1,g_isz)⍴,⊃⍉(g_w[ii;;]+.×⍉errderivative)
     ktemp←(1,g_isz)⍴,⊃g_hhatarr[ii;]
     ktemp[;(0<(,ktemp))/⍳g_isz]←1
     ktemp[;(0≥(,ktemp))/⍳g_isz]←0
     ⍝ddxa←(1,g_isz)⍴,⊃(((ddxh)+.×(⍉g_hhatarr[ii;]))×(1,g_isz)⍴(1-g_hhatarr[ii;]))
     ddxa←(1,g_isz)⍴,⊃((ddxh)+.×(⍉g_hhatarr[ii;]))×(1,g_isz)⍴ktemp ⍝ drelu
     g_b[ii;]←((1,g_isz)⍴,⊃g_b[ii;])+g_lr×ddxa

     :If ii=1
         thhat←(1,g_isz)⍴g_hhatarr[1;]
     :Else
         thhat←(1,g_isz)⍴g_hhatarr[ii-1;]
     :EndIf
     g_w[ii;;]←g_w[ii;;]+g_lr×(ddxa+.×⍉thhat)
     :If ii≤2
         g_u[ii;]←((1,g_isz)⍴g_u[ii;])+g_lr×errderivative
     :EndIf
     :If ii=1
         g_d[ii;]←((1,g_numclasses)⍴g_d[ii;])+g_lr×errderivative
     :EndIf
     ii←ii-1
 :EndWhile
⍝ 

⍝ kk←g_numlayers
⍝ :While kk≥1
⍝     ⍝ relU backward
⍝     tmp1←,⊃g_hhatarr[kk;]
⍝     tmp1[(tmp1≤0)/⍳g_isz]←0
⍝     dout←(1,g_isz)⍴tmp1
⍝     da_bn←dout
⍝     ;
⍝     ⍝ BN backkward
⍝     (da dgamma dbeta)←bn_bw dout
⍝
⍝     ⍝ affine backward
⍝     dx←da+.×⍉g_w[kk;;]
⍝     dw←g_hhatarr[kk;]+.×⍉da
⍝     db←(1,g_isz)⍴+/da
⍝     :If kk≤g_numlayers
⍝         g_w[kk;;]←g_w[kk;;]+g_lr×dw
⍝     :EndIf
⍝     g_b[kk;]←((1,g_isz)⍴,⊃g_b[kk;])-g_lr×db
⍝     g_gamma←g_gamma-g_lr×dgamma
⍝     g_beta←g_beta-g_lr×dbeta
⍝     kk←kk-1
⍝
⍝ :EndWhile
 z←0
