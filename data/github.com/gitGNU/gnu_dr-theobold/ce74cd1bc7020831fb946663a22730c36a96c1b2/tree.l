## tree.l - Tree data type implementations
## Copyright (C) 2017  Christopher Howard

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## (record sym [sym ..]) -> NIL

# Defines getter and (non-destructive) setter functions which act upon
# a list of length equal to the number of symbols passed to
# record. Basically provides a quick and easy data structure.

# The functions assigned to each symbol can be used in two forms, the
# first being the getter and the second being the setter.

# (sym 'lst) -> any
# (sym 'lst 'any) -> lst

# Probably you want to prefix each symbol with a data type name.
(de record X
   (let (@N 1)
      (recur (@N X)
         (unless (not X)
            (def (car X)
               (macro '(@ (let (Lst (next))
                             (if (not (args))
                                (car (nth Lst @N))
                                (place @N Lst (next)) ) ) ) ) )
            (recurse (inc @N) (cdr X)) ) ) ) )

# AANode = ((Key Val) LeftT RightT Level)

(record aa-kv aa-left aa-right aa-level)

(de aa-search-val (Tree Key)
   (cdr (aa-search Tree Key)))

(de aa-search (Tree Key)
   (unless (not Tree)
      (if (= Key (car (aa-kv Tree)))
         (aa-kv Tree)
         (if (< Key (car (aa-kv Tree)))
            (aa-search (aa-left Tree) Key)
            (aa-search (aa-right Tree) Key) ) ) ) )

(de aa-skew (Tree)
   (unless (not Tree)
      (if (not (aa-left Tree))
         Tree
         (if (= (aa-level (aa-left Tree)) (aa-level Tree))
            (aa-right (aa-left Tree) (aa-left Tree (aa-right (aa-left Tree))))
            Tree ) ) ) )

(de aa-split (Tree)
   (unless (not Tree)
      (if (not (and (aa-right Tree) (aa-right (aa-right Tree))))
         Tree
         (if (= (aa-level Tree) (aa-level (aa-right (aa-right Tree))))
            (aa-left (aa-level (aa-right Tree) (inc (aa-level Tree))) (aa-right Tree (aa-left (aa-right Tree))))
            Tree ) ) ) )

# (aa-insert 'any 'any 'aa-tree) -> aa-tree

# Non-destructive insert operation. If `Key' matches a the key of a
# node, the node will be replaced by a node containing the new
# value. Can operate on an empty (NIL) tree.
(de aa-insert (Key Val Tree)
   (if (not Tree)
      (list (cons Key Val) NIL NIL 1)
      (aa-split
         (aa-skew
            (if (= Key (car (aa-kv Tree)))
               (aa-kv Tree (cons Key Val))
               (if (< Key (car (aa-kv Tree)))
                  (aa-left Tree
                     (aa-insert Key Val (aa-left Tree)) )
                  (aa-right Tree
                     (aa-insert Key Val (aa-right Tree)) ) ) ) ) ) ) )
# (aa-delete 'any 'aa-tree) -> aa-tree

# Non-destructive delete operation. Requests that match no node will
# be ignored.
(de aa-delete (Key Tree)
   (if (not Tree)
      Tree
      (let (PrunedTree
            (if (> Key (car (aa-kv Tree)))
               (aa-right Tree (aa-delete Key (aa-right Tree)))
               (if (< Key (car (aa-kv Tree)))
                  (aa-left Tree (aa-delete Key (aa-left Tree)))
                  (if (not (or (aa-left Tree) (aa-right Tree)))
                     NIL
                     (if (not (aa-left Tree))
                        (let (L (aa-successor Tree))
                           (aa-kv
                              (aa-right Tree
                                 (aa-delete
                                    (car (aa-kv L))
                                    (aa-right Tree) ) )
                              (aa-kv L) ) )
                        (let (L (aa-predecessor Tree))
                           (aa-kv
                              (aa-left Tree
                                 (aa-delete
                                    (car (aa-kv L))
                                    (aa-left Tree) ) )
                              (aa-kv L) ) ) ) ) ) )
            storeP '((Val) (setq PrunedTree Val))
            retrieveP '(() PrunedTree) )
         (storeP (aa-decrease-level (retrieveP)))
         (storeP (aa-skew (retrieveP)))
         (storeP (aa-right (retrieveP) (aa-skew (aa-right (retrieveP)))))
         (unless (not (aa-right (retrieveP)))
            (storeP
               (aa-right
                  (retrieveP)
                  (aa-right
                     (aa-right
                        (retrieveP) )
                        (aa-skew
                           (aa-right
                              (aa-right (retrieveP)) ) ) ) ) ) )
         (storeP (aa-split (retrieveP)))
         (aa-right (retrieveP) (aa-split (aa-right (retrieveP)))) ) ) )

(de aa-flatten (Tree)
   (let (L ()
         Fn '((Tr)
              (when Tr
                 (setq L (cons (aa-kv Tr) L))
                 (Fn (aa-left Tr))
                 (Fn (aa-right Tr)) ) ) )
      (Fn Tree)
      L ) )

(de aa-to-list (Tree)
   (by 'car sort (aa-flatten Tree)) )

(de aa-keys-to-list (Tree)
   (mapcar 'car (aa-to-list Tree)))

(de aa-decrease-level (Tree)
   (let (nilwrap '((X) (if (not X) 0 X))
         WT Tree
         ShouldBe (inc (min (nilwrap (aa-level (aa-left WT))) (nilwrap (aa-level (aa-right WT))))) )
      (if (< ShouldBe (aa-level WT))
         (prog (setq WT (aa-level WT ShouldBe))
            (if (< ShouldBe (aa-level (aa-right WT)))
               (setq WT
                  (aa-right WT
                     (aa-level
                        (aa-right WT) ShouldBe)))))
         NIL )
      WT ) )

(de aa-predecessor (Tree)
   (let (WT (aa-left Tree))
      (recur (WT)
         (if (not (aa-right WT))
            WT
            (recurse (aa-right WT)) ) ) ) )

(de aa-successor (Tree)
   (let (WT (aa-right Tree))
      (recur (WT)
         (if (not (aa-left WT))
            WT
            (recurse (aa-left WT)) ) ) ) )

(de bst-new (Key Val)
   (list Key Val NIL NIL) )

(de bst-insert (Tree Key Val)
   (if (= (car Tree) Key) (set (cdr Tree) Val)
      (if (< Key (car Tree))
         (if (not (caddr Tree))
            (set (cddr Tree) (bst-new Key Val))
            (bst-insert (caddr Tree) Key Val) )
         (if (not (cadddr Tree))
            (set (cdddr Tree) (bst-new Key Val))
            (bst-insert (cadddr Tree) Key Val) ) ) )
   Tree )

(de bst-search (Tree Key)
   (if (not Tree)
      NIL
      (if (= Key (car Tree))
         (list (car Tree) (cadr Tree))
         (if (< Key (car Tree))
            (bst-search (caddr Tree) Key)
            (bst-search (cadddr Tree) Key) ) ) ) )

