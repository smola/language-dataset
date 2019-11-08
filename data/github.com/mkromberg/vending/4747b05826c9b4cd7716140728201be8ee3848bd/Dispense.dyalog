 r←Dispense button;i;m;change

 :If (≢coffeeTypes)<i←coffeeTypes⍳⊂button
     ('Select one of: ',⍕coffeeTypes)⎕SIGNAL 502
 :ElseIf price>insertedValue←VM.coinsInserted+.×coinValues
     'Insufficient change inserted'⎕SIGNAL 503
 :ElseIf ~∧/m←materialsRequired[i;]≤VM.materialLevels
     ('Not enough: ',⍕(~m)/materialNames)⎕SIGNAL 504
 :ElseIf 0=⍴change←(VM.coinsInserted/coinValues)CalculateChange insertedValue-price
     'Unable to give change'⎕SIGNAL 505
 :Else
     ⍝ We can dispense!
     VM.MaterialLevels-←materialsRequired[i;]
     VM.(coinCounts+←coinsInserted)
     VM.coinCoints-←¯1+{≢⍵}⌸coinValues,change
     VM.coinsInserted[]←0
     r←('1 cup of ',button)change
 :EndIf
