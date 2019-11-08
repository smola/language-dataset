\\ Dr Antti J Ylikoski 2016-01-10--2016-06-05
\\
\\ From George F Luger: ARTIFICIAL INTELLIGENCE, 4th Edition
\\ Pearson--Addison-Wesley, ISBN 0-201-64866-0
\\
\\ Exercise 9.  Consider the bidirectional associative memory
\\ (BAM) of Section 10.6.3.  (Change the association pairs given in
\\ our example and create the weight matrix for the novel
\\ associations.  Select new vectors and test your BAM associator.)
\\
\\ The BAM associator in this file, works 100% correctly according to
\\ Luger.  See the numbers on Page 460 of Luger.
\\
\\ The weights matrix of the BAM ANN is similar to the linear 
\\ associator, and it can be worked out in advance.
\\
\\ Hence:
\\
\\ W = Y1 * X1 + Y2 * X2 + ... Yt * Xt (See Luger)
\\
\\ Usage:
\\
\\ First load Ramil Farkshatov's defstruct package:
\\
\\ (load "defstruct.shen")
\\
\\ Secondly, load Ramil Farkshatov's FOR loop macro:
\\
\\ (load "for.shen")
\\
\\ Load the array aux functions package:
\\
\\ (load "array.shen")
\\
\\ After that, load this file:
\\
\\ (load "BAM.shen")
\\ 
\\ and run the exercise functions.


\\ Define the Bi-directional Associative Memory ANN
\\ Note by author: Here we add some fields to the defstruct--
\\ namely, the name of the ANN neuron;
\\ and also, the list of its output neurons


(defstruct bam
  (neuron-name string)
  (nr-inputs number)
  (inputs-vec vector)
  (weights-vec vector)
  (activation-level number)
  (treshold-function (number --> number))
  (neuron-output number)
  (nr-outputs number)
  (outputs-vec vector))


\\ The list of all the neurons that are used, are in the oblist,
\\ somewhat analogously to the MACLISP OBLIST
\\

(set *oblist* [])

\\ Define add-neuron-to-oblist
\\

(define add-neuron-to-oblist
  { bam --> (list A) }
  N ->
    (do
      (set *oblist* (append (value *oblist*) [N]))
      []))


\\ The BAM ANN treshold function
\\

(define treshold
  { number --> number --> number }
  Activation F-1 -> 1 where (> Activation 0)
  Activation F-1 -> -1 where (< Activation 0)
  Activation F-1 -> F-1)



\\ Calling (transfer-function N) where N is a neuron, is equivalent
\\ with firing the neuron N, ie. computing its output


(define transfer-function
  { bam --> number }
  N ->
    (let
      AL (activation-level N)
      _  (bam-activation-level-> N AL)
      F-1 (bam-neuron-output N) \\ Net at time T - 1
      F  (bam-treshold-function N)
      _  (bam-neuron-output-> N (F (bam-activation-level N) F-1))
      (bam-neuron-output N)))


(define activation-level
  { bam --> number }
  N -> (activation-level-h N 1 0))


(define activation-level-h
  { bam --> number --> number --> number }
  N Counter Sum ->
    Sum where (> Counter (bam-nr-inputs N))
  N Counter Sum ->
    (activation-level-h
      N (+ 1 Counter)
        (+ Sum (* (<-vector (bam-inputs-vec N) Counter)
                  (<-vector (bam-weights-vec N) Counter)))))



\\ Now construct the BAM ANN as in Luger, Figure 10.24, Page 460.
\\
\\ Design consideration: With loops, or manually?
\\ Design decision: Do it manually, for the clarity, and
\\ the educational value.
\\
\\ Make the net with the neurons X1, X2, X3, and X4
\\ and Y1, Y2, and Y3.
\\

(set *X1* (mk-bam
            "X1"        \\ name
            3           \\ # of inputs
            (vector 3)  \\ inputs vector
            (vector 3)  \\ weights vector
            0           \\ Dummy for activation level
            (function treshold) \\ treshold function
            0           \\ Dummy for neuron output
            3           \\ # of outputs
            (vector 3)  \\ outputs vector
           ))


(set *X2* (mk-bam
            "X2"        \\ name
            3           \\ # of inputs
            (vector 3)  \\ inputs vector
            (vector 3)  \\ weights vector
            0           \\ Dummy for activation level
            (function treshold) \\ treshold function
            0           \\ Dummy for neuron output
            3           \\ # of outputs
            (vector 3)  \\ outputs vector
           ))


(set *X3* (mk-bam
            "X3"        \\ name
            3           \\ # of inputs
            (vector 3)  \\ inputs vector
            (vector 3)  \\ weights vector
            0           \\ Dummy for activation level
            (function treshold) \\ treshold function
            0           \\ Dummy for neuron output
            3           \\ # of outputs
            (vector 3)  \\ outputs vector
           ))


(set *X4* (mk-bam
            "X4"        \\ name
            3           \\ # of inputs
            (vector 3)  \\ inputs vector
            (vector 3)  \\ weights vector
            0           \\ Dummy for activation level
            (function treshold) \\ treshold function
            0           \\ Dummy for neuron output
            3           \\ # of outputs
            (vector 3)  \\ outputs vector
           ))


(set *Y1* (mk-bam
            "Y1"        \\ name
            4           \\ # of inputs
            (vector 4)  \\ inputs vector
            (vector 4)  \\ weights vector
            0           \\ Dummy for activation level
            (function treshold) \\ treshold function
            0           \\ Dummy for neuron output
            4           \\ # of outputs
            (vector 4)  \\ outputs vector
           ))


(set *Y2* (mk-bam
            "Y2"        \\ name
            4           \\ # of inputs
            (vector 4)  \\ inputs vector
            (vector 4)  \\ weights vector
            0           \\ Dummy for activation level
            (function treshold) \\ treshold function
            0           \\ Dummy for neuron output
            4           \\ # of outputs
            (vector 4)  \\ outputs vector
           ))


(set *Y3* (mk-bam
            "Y3"        \\ name
            4           \\ # of inputs
            (vector 4)  \\ inputs vector
            (vector 4)  \\ weights vector
            0           \\ Dummy for activation level
            (function treshold) \\ treshold function

            0           \\ Dummy for neuron output
            4           \\ # of outputs
            (vector 4)  \\ outputs vector
           ))


(add-neuron-to-oblist (value *X1*))
(add-neuron-to-oblist (value *X2*))
(add-neuron-to-oblist (value *X3*))
(add-neuron-to-oblist (value *X4*))
(add-neuron-to-oblist (value *Y1*))
(add-neuron-to-oblist (value *Y2*))
(add-neuron-to-oblist (value *Y3*))



\\ Build the ANN weight matrices to the both directions,
\\ W and its transpose W-T
\\
\\ The vector associations <Xi, Yi>, to be stored in the
\\ linear associator network, Luger p. 460:


(set x-y (@v (@p (@v 1 -1 -1 -1 <>) (@v 1 1 1 <>))
             (@p (@v -1 -1 -1 1 <>) (@v 1 -1 1 <>))
             <>))

(set y1x1 (outer-product (snd (<-vector (value x-y) 1))
                         (fst (<-vector (value x-y) 1))))

(set y2x2 (outer-product (snd (<-vector (value x-y) 2))
                         (fst (<-vector (value x-y) 2))))

(set w (array-sum (value y1x1) (value y2x2)))
(set w-t (transpose (value w)))

(output-array (value w))
(output-array (value w-t))


\\ The weight matrices are as follows.
\\ Input weight Number #m of Neuron Y #n, is W(n, m)
\\ Therefore, the weights vector of Neuron Y #n, is (<-vector W n)
\\
\\ Input weight Number #i of Neuron X #j, is W-T(j, i)
\\ Therefore, the weights vector of Neuron X #j, is (<-vector W-T j)
\\
\\ Outputs of Neurons X #j are fed into the inputs of Neurons
\\ Y #n.
\\ Outputs of Neurons Y #n are fed into the inputs of Neurons
\\ X #j.
\\
\\ This constitutes the BAM, the Bi-directional Associative Memory.
\\
\\ First fill in the weight vectors of the X and Y neurons.
\\


(bam-weights-vec-> (value *Y1*) (<-vector (value w) 1))
(bam-weights-vec-> (value *Y2*) (<-vector (value w) 2))
(bam-weights-vec-> (value *Y3*) (<-vector (value w) 3))


(bam-weights-vec-> (value *X1*) (<-vector (value w-t) 1))
(bam-weights-vec-> (value *X2*) (<-vector (value w-t) 2))
(bam-weights-vec-> (value *X3*) (<-vector (value w-t) 3))
(bam-weights-vec-> (value *X4*) (<-vector (value w-t) 4))


\\ Then, fill in the connections.
\\ The inputs of a neuron are a vector of numbers.
\\ The weights of a neuron are a vector of numbers.
\\
\\ The outputs of a neuron are a vector of 
\\ (@p neuron-ID, input-number) pairs.
\\ The inputs and the outputs are bidirectionally
\\ symmetric.
\\
\\ Again, the design decision: With (for ...) loops, or, 
\\ manually?  And again, for the clarity and the educational value,
\\ build the net manually.
\\
\\ The neurons are fired, by filling in the inputs, and calling
\\ (transfer-function N)
\\
\\ All that we need to do here, is to fill in the output vectors, 
\\ as follows.
\\
\\ See Figure 10.24 in Luger, Page 460


(bam-nr-outputs-> (value *X1*) 3)

(bam-outputs-vec-> (value *X1*)
  (@v (@p "Y1" 1) \\ Neuron Y1, 1st input
      (@p "Y2" 1) \\ Neuron Y1, 1st input
      (@p "Y3" 1) \\ Neuron Y3, 1st input
  <>)
)


(bam-nr-outputs-> (value *X2*) 3)

(bam-outputs-vec-> (value *X2*)
  (@v (@p "Y1" 2) \\ Neuron Y1, 2nd input
      (@p "Y2" 2) \\ Neuron Y2, 2nd input
      (@p "Y3" 2) \\ Neuron Y3, 2nd input
  <>)
)


(bam-nr-outputs-> (value *X3*) 3)

(bam-outputs-vec-> (value *X3*)
  (@v (@p "Y1" 3) \\ Neuron Y1, 3rd input
      (@p "Y2" 3) \\ Neuron Y2, 3rd input
      (@p "Y3" 3) \\ Neuron Y3, 3rd input
  <>)
)


(bam-nr-outputs-> (value *X4*) 3)

(bam-outputs-vec-> (value *X4*)
  (@v (@p "Y1" 4) \\ Neuron Y1, 4th input
      (@p "Y2" 4) \\ Neuron Y2, 4th input
      (@p "Y3" 4) \\ Neuron Y3, 4th input
  <>)
)


(bam-nr-outputs-> (value *Y1*) 4)

(bam-outputs-vec-> (value *Y1*)
  (@v (@p "X1" 1) \\ Neuron X1, 1st input
      (@p "X2" 1) \\ Neuron X2, 1st input
      (@p "X3" 1) \\ Neuron X3, 1st input
      (@p "X4" 1) \\ Neuron X4, 1st input
  <>)
)


(bam-nr-outputs-> (value *Y2*) 4)

(bam-outputs-vec-> (value *Y2*)
  (@v (@p "X1" 2) \\ Neuron X1, 2nd input
      (@p "X2" 2) \\ Neuron X2, 2nd input
      (@p "X3" 2) \\ Neuron X3, 2nd input
      (@p "X4" 2) \\ Neuron X4, 2nd input
  <>)
)


(bam-nr-outputs-> (value *Y3*) 4)

(bam-outputs-vec-> (value *Y3*)
  (@v (@p "X1" 3) \\ Neuron X1, 3rd input
      (@p "X2" 3) \\ Neuron X2, 3rd input
      (@p "X3" 3) \\ Neuron X3, 3rd input
      (@p "X4" 3) \\ Neuron X4, 3rd input
  <>)
)

\\ Now the BAM ANN has been filled with the weights, and the 
\\ outputs information.
\\
\\ Next define the functions to fire, and therefore test,
\\ the BAM ANN.
\\

\\ First the function to send, or, to distribute, the outputs of
\\ a neuron into the inputs of the receiving neurons:

(define send-outputs
  { bam --> (list A) }
  N ->
    (let
      Nr (bam-nr-outputs N)
      Vec (bam-outputs-vec N)
      Val (bam-neuron-output N)
      _ (for (Indx 1 1 Nr)
          (let 
            OutV (<-vector Vec Indx) \\ Output # Indx
            NeuronName (fst OutV) \\ Neuron to send the output to
            InputNr (snd OutV) \\ To which one of the outputs
            Neuron (find-neuron NeuronName)
            InputsVec (bam-inputs-vec Neuron)
            _ (vector-> InputsVec InputNr Val)
            _ (bam-inputs-vec-> Neuron InputsVec)
            []
          ) \\ end (let ...)
        ) \\ end (for ...)
      [] ) \\ end (let ...)
) \\ end function



(define find-neuron
  { string --> bam }
  NeuronName ->
    (let 
      ListOfNeurons (value *oblist*)
      (find-neuron-h NeuronName ListOfNeurons)
    ) \\ end (let ...)
) \\ end function


(define find-neuron-h
  { string --> (list bam) --> bam }
  NeuronName ListOfNeurons ->
    (let 
      Nr (hd ListOfNeurons) \\ the neuron to process
      - (if (= Nr [])
            (error "Neuron not found")
            skip)
      NrName (bam-neuron-name Nr)
      (if (= NrName NeuronName)
          Nr
          (find-neuron-h NeuronName (tl ListOfNeurons)))))



\\ ------------------------------------------------------------
\\ The functions to test the computed linear associator ANN:
\\
\\
\\ First, test the Xi ==> Yi mapping.  See Luger, Page 461
\\ This comes out 100% correct:


(define test-bam-x
  { --> (list A) }
  ->
    (let
      _ (output "~%Give a sequence of 4 Xi neuron values: ")
      InL (lineread)
      _ (output "~%Numbers: ~A~%" InL)
      I (list2vec InL)
      _ (output "~%Vector: ~A~%" I)
      \\ These 4 values are the Xi values, ie. they are assigned 
      \\ into the Xi neuron outputs, and then the Xi neuron
      \\ outputs are distributed to the Xi outputs, which are Yi
      \\ neurons, and thereafter the Yi neurons are
      \\ fired.
      _ (bam-neuron-output-> (value *X1*) (<-vector I 1))
      _ (bam-neuron-output-> (value *X2*) (<-vector I 2))
      _ (bam-neuron-output-> (value *X3*) (<-vector I 3))
      _ (bam-neuron-output-> (value *X4*) (<-vector I 4))
      \\ Send the assigned Xi values, ie. the outputs, 
      \\ to the output neuron sets of the Xi neurons
      _ (send-outputs (value *X1*))
      _ (send-outputs (value *X2*))
      _ (send-outputs (value *X3*))
      _ (send-outputs (value *X4*))
      \\ Now the Yi neurons have received their inputs from the
      \\ Xi neurons.  Fire all Yi neurons
      O1 (transfer-function (value *Y1*))
      O2 (transfer-function (value *Y2*))
      O3 (transfer-function (value *Y3*))
      _ (output "~%~%Output of ANN: ~A~%" [O1 O2 O3])
      []))


\\ ------------------------------------------------------------
\\


(define list2vec
  { (list A) --> (vector A) }
  L ->
    (let
      Ln (length L)
      VEC (vector Ln)
      (list2vec-h L VEC Ln 1)))


(define list2vec-h
  { (list A) --> vector --> number --> number
     --> (vector A) }
  L VEC Ln Indx ->
    VEC where (> Indx Ln)
  L VEC Ln Indx ->
    (let
      _ (vector-> VEC Indx (hd L))
      (list2vec-h (tl L) VEC Ln (+ 1 Indx))))


\\ ------------------------------------------------------------
\\
\\ Testing the BAM network to the Y => X direction gives
\\ the 100% correct result, according to Luger, Page 461:
\\ 


(define test-bam-y
  { --> (list A) }
  ->
    (let
      _ (output "~%Give a sequence of 3 Yi neuron values: ")
      InL (lineread)
      _ (output "~%Numbers: ~A~%" InL)
      I (list2vec InL)
      _ (output "~%Vector: ~A~%" I)
      \\ These 3 values are the Yi values, ie. they are assigned 
      \\ into the Yi neuron outputs, and then the Yi neuron
      \\ outputs are distributed to all the Yi outputs, which are Xi
      \\ neurons, and thereafter the Xi neurons are
      \\ fired.
      _ (bam-neuron-output-> (value *Y1*) (<-vector I 1))
      _ (bam-neuron-output-> (value *Y2*) (<-vector I 2))
      _ (bam-neuron-output-> (value *Y3*) (<-vector I 3))
      \\ Send the assigned Xi values, ie. the outputs, 
      \\ to the output neuron sets of the Yi neurons
      _ (send-outputs (value *Y1*))
      _ (send-outputs (value *Y2*))
      _ (send-outputs (value *Y3*))
      \\ Now the Xi neurons have received their inputs from the
      \\ Yi neurons.  Fire all Xi neurons
      O1 (transfer-function (value *X1*))
      O2 (transfer-function (value *X2*))
      O3 (transfer-function (value *X3*))
      O4 (transfer-function (value *X4*))
      _ (output "~%~%Output of ANN: ~A~%" [O1 O2 O3 O4])
      []))


