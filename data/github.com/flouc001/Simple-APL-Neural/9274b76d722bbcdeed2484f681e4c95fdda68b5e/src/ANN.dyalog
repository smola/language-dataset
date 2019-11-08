:Namespace ANN
⍝ Artificial Neural Network
⍝ Feed-Forward with Back Propagation

      New←{
         ⍝ ⍵ ←→ dimensions
         ⍝ ← ←→ network
          n←⎕NS''
          n.weights←2{⍺ ⍵(?⍴)0}/⍵
          n.learning_rate←0.5
          n.training_iterations←1000
          n.error_threshold←0.001
          n
      }

      Process←{
        ⍝ ⍺ ←→ network
        ⍝ ⍵ ←→ input
        ⍝ ← ←→ output
          ⊃⌽⍺.weights outputs ⍵
      }

      Train←{
        ⍝ ⍺ ←→ network
        ⍝ ⍵ ←→ patterns (vec of input target pairs)
          w e←⊃{w e←(⊃⍵)trainSingle ⍺ ⋄ w(e,⊃⌽⍵)}/⍵,⊂⍺.weights ⍬
          ⍺.weights←w
          ⍺.training_iterations-←1
          ⍺.training_iterations<0:
          ⍺.error_threshold>mse e:
          ⍺ ∇ ⍵
      }
    
    mse←+.*∘2÷≢

    sigmoid←{÷1+*-⍵}

      outputs←{
        ⍝ ⍺ ←→ weights
        ⍝ ⍵ ←→ input
        ⍝ ← ←→ outputs
          1↓⊃{⍵,⊂sigmoid(⊃⌽⍵)+.×⍺}/⌽(⊂⊂⍵),⍺
      }


      trainSingle←{
        ⍝ ⍺ ←→ weights
        ⍝ ⍵ ←→ (inputs) (target)
        ⍝ ← ←→ (adjusted weights) (errors)
          in t←⍵
          n←1   ⍝ learning rate
          out←⍺ outputs in
          ∆o←out×1-out
          ∆e←t-⊃⌽out
          ∆←∆e×⊃⌽∆o
          ∆bp←⊃{w o←⍺
              ⍵,⍨⊂o×w+.×⊃⌽⍵
          }/(↓⍉↑(1↓⍺)(¯1↓∆o)),⊂⊂∆
          w←⍺+n×((⊂in),¯1↓out)∘.×¨∆bp
          w ∆e
      }

:EndNamespace
