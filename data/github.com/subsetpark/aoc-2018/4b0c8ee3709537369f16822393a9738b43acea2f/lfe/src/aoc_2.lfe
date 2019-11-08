(defmodule aoc_2
  (export (pt1 0) (pt2 0)))

(defun load () (aoc_util:loadm))

(defun pt1 () (checksum (load)))

(defun count-letters (word)
  (lists:foldl (fun count-letter 2) (dict:new) (binary_to_list word)))
(defun count-letter (letter dict) (dict:update_counter letter 1 dict))

(defun is (n)
  (match-lambda [((tuple _ m)) (when (== n m)) 'true]
                [(_) 'false]))

(defun get-increments (word)
  (let* ([dict (count-letters word)]
         [list (dict:to_list dict)])
    (tuple (cond
             ((lists:any (is 2) list) 1)
             ('true 0))
           (cond
             ((lists:any (is 3) list) 1)
             ('true 0)))))

(defun count-all (words)
  (lists:foldl
    (match-lambda
      [((tuple +two +three) (tuple twos threes)) (tuple (+ twos +two) (+ threes +three))])
    #(0 0)
    (lists:map (fun get-increments 1) words)))

(defun checksum (words)
  (let ([(tuple x y) (count-all words)])
    (* x y)))

(defun pt2 ()
  (let ([(list (tuple x (list y)) (tuple y (list x)))
               (minimal-pairs (lists:map (fun binary_to_list 1) (load)))])
    (common-word x y)))

(defun hamming (a b)
  (lists:foldl
    (match-lambda
      [((tuple x x) acc) acc]
      [(_ 0) 1]
      [(_ _) 'nomatch])
    0
    (lists:zip a b)))

(defun search (word words)
  (tuple word (lists:filter (lambda (x) (== 1 (hamming word x))) words)))

(defun search (words) (lists:map (lambda (w) (search w words)) words))

(defun minimal-pairs (words)
  (lists:filter (match-lambda [((tuple x ys)) (> (length ys) 0)])
                (search words)))

(defun common-word (a b)
  (lists:reverse
    (lists:foldl
      (match-lambda
        [((tuple x x) acc) (cons x acc)]
        [(_ acc) acc])
      ()
      (lists:zip a b))))
