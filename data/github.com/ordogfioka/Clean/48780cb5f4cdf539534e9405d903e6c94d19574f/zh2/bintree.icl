module bintree

import StdEnv
import GenEq,StdMaybe

:: BSTree a = Empty | Node a (BSTree a)  (BSTree a)
:: KeyValue a b = KV a b
:: MapKV k v = M (BSTree (KeyValue k v))

instance < (KeyValue a b) | < a
 where
 < (KV a b) (KV c d) = a < c 

BSTree_emptyInt :: (BSTree Int)
BSTree_emptyInt = Empty

BSTree_emptyKV :: BSTree (KeyValue Int Char)
BSTree_emptyKV = Empty

derive gEq BSTree,KeyValue,Maybe,MapKV

BSTree_insert :: (BSTree a) a -> (BSTree a) | < a
BSTree_insert Empty number = Node number Empty Empty
BSTree_insert (Node number1 left right) number2
  | number2 < number1 = Node number1 (BSTree_insert left number2) right
  | number2 > number1 = Node number1 left (BSTree_insert right number2)  
                      = Node number1 left right
                                           
BSTree_lookup :: (BSTree a) a -> Maybe a | < a                
BSTree_lookup Empty _ = Nothing
BSTree_lookup (Node number left right) numb
  | numb < number = BSTree_lookup left numb
  | number < numb = BSTree_lookup right numb
                  = Just number               

BSTree_update :: (BSTree a) a -> (BSTree a) | < a                
BSTree_update Empty _ = Empty
BSTree_update (Node a left right) key 
 | key < a = Node a (BSTree_update left key) right
 | a < key = Node a left (BSTree_update right key)
           = Node key left right
                  
testIntBSTree =
  (Node 1 (Node 0 Empty Empty)
  (Node 21 (Node 4 (Node 2 Empty Empty)
  (Node 6 Empty (Node 8 Empty Empty)))
  (Node 63 Empty Empty)))
  
testKVBSTree =
  (Node (KV 6 'a')
  (Node (KV 4 'c')
  (Node (KV 3 'l') Empty Empty)
  (Node (KV 5 'y') Empty Empty))
  (Node (KV 9 'r')
  (Node (KV 7 's') Empty
  (Node (KV 8 'q') Empty Empty))
  (Node (KV 10 'p') Empty
  (Node (KV 50 'o') Empty Empty))))

BSTree_depth :: (BSTree a) -> Int
BSTree_depth tree = helpMe tree 0
 where
  helpMe Empty a = a
  helpMe (Node _ left right) a
   | (helpMe left a) > (helpMe right a) = (helpMe left a) + 1
                                        = (helpMe right a) + 1

BSTree_isBalanced :: (BSTree a) -> Bool
BSTree_isBalanced Empty = True
BSTree_isBalanced (Node _ left right) = (abs((BSTree_depth left)-(BSTree_depth right)) < 2) && (BSTree_isBalanced left) && (BSTree_isBalanced right)

instance toString (KeyValue a b) | toString a & toString b
  where
    toString (KV x y) = "key: " +++ toString x +++ ", value: " +++ toString y
   
class Traversable t  where
    inOrder :: (a b -> b) b (t a) -> b
    preOrder :: (a b -> b) b (t a) -> b
    postOrder :: (a b -> b) b (t a) -> b

instance Traversable BSTree where   
    inOrder f s Empty = s
    inOrder f s (Node a left right) = inOrder f (f a (inOrder f s left)) right
    preOrder f s Empty = s
    preOrder f s (Node a left right) = preOrder f (preOrder f (f a s) left) right
    postOrder f s Empty = s
    postOrder f s (Node a left right) = f a (postOrder f (postOrder f s left) right)

sortBSTree :: (BSTree a) -> [a]
sortBSTree tree = inOrder (\x s -> s ++  [x] ) [] tree

getValue :: (KeyValue k v) -> v
getValue (KV k v) = v

MapKV_update :: (MapKV k v) k (v -> Maybe v) -> MapKV k v | < k
MapKV_update (M Empty) key maybe = M Empty
MapKV_update (M tree) key updateFunction = M (find tree (BSTree_lookup tree (KV key undef)))
 where 
  find tree Nothing = tree
  find tree (Just asd) = tmp tree (updateFunction (getValue asd))
    where 
      tmp tree Nothing = tree
      tmp tree (Just vv) = BSTree_update tree (KV key vv)  
   
MapKV_lookup :: (MapKV k v) k -> Maybe (KeyValue k v) | < k
MapKV_lookup (M a) key = BSTree_lookup a (KV key undef)

MapKV_insert :: (MapKV k v) k v -> (MapKV k v) | < k 
MapKV_insert (M a) key value =M (BSTree_insert a (KV key value))

Start = (and (flatten alltests), alltests)
alltests =
  [ test_BSTRe_Empty
  , test_BSTree_insert
  , test_BSTree_lookup
  , test_BSTree_depth
  , test_BSTree_isBalanced
  , test_Traversable
  , test_sortBSTree
  , test_MapKV_update
  , test_MapKV_lookup
  , test_MapKV_insert
  ]
  
test_BSTRe_Empty =
  [ BSTree_emptyInt === Empty
  , BSTree_emptyKV === Empty
  ]
  
test_BSTree_insert =
  [ BSTree_insert BSTree_emptyInt 3 ===
    Node 3 Empty Empty
  , BSTree_insert (BSTree_insert BSTree_emptyInt 3) 5 ===
    Node 3 Empty (Node 5 Empty Empty)
  , BSTree_insert (BSTree_insert BSTree_emptyInt 3) 3 ===
    Node 3 Empty Empty
  , BSTree_insert (BSTree_insert BSTree_emptyInt 3) 1 ===
    Node 3 (Node 1 Empty Empty) Empty
  ]

test_BSTree_lookup =
  [ BSTree_lookup testIntBSTree 21 === Just 21
  , map (BSTree_lookup testIntBSTree) [3, 7, 50, 100] === repeatn 4 Nothing
  , BSTree_lookup Empty 'x' === Nothing
  , BSTree_lookup testKVBSTree (KV 3 undef) === Just (KV 3 'l')
  ]

test_BSTree_depth =
  [ BSTree_depth BSTree_emptyInt == 0
  , BSTree_depth BSTree_emptyKV == 0
  , BSTree_depth testIntBSTree == 5
  , BSTree_depth testKVBSTree == 4
  ]

test_BSTree_isBalanced =
  [ BSTree_isBalanced Empty == True
  , BSTree_isBalanced testIntBSTree == False
  , BSTree_isBalanced testKVBSTree == True
  ]
  
test_Traversable =
  [ inOrder (\x s -> s +++ toString x +++ ", ") "" testKVBSTree ==
    "key: 3, value: l, key: 4, value: c, key: 5, value: y, key: 6, "
    +++"value: a, key: 7, value: s, key: 8, value: q, key: 9, value: r, "
    +++"key: 10, value: p, key: 50, value: o, "
  , preOrder (\x s -> s +++ toString x) "" testIntBSTree == "1021426863"
  , postOrder (\x s -> s +++ toString x) "" testIntBSTree == "0286463211"
  , inOrder (\x s -> s +++ toString x) "" BSTree_emptyInt == ""
  , preOrder (\x s -> s +++ toString x) "" BSTree_emptyInt == ""
  , postOrder (\x s -> s +++ toString x) "" BSTree_emptyInt == ""
  ]

test_sortBSTree =
  [ sortBSTree BSTree_emptyInt == []
  , map (\(KV k v) -> v) (sortBSTree testKVBSTree) == ['l','c','y','a','s','q','r','p','o']
  , sortBSTree testIntBSTree == sort (sortBSTree testIntBSTree)
  , length (sortBSTree testIntBSTree) == 8
  ]

test_MapKV_update =
  [ MapKV_update (M BSTree_emptyKV) 4 (\_ -> Just 'a') === M BSTree_emptyKV
  , MapKV_update (M (Node (KV 5 'a') (Node (KV 3 's') Empty (Node (KV 4 'r') Empty Empty)) Empty)) 5 (\_ -> Just 't') ===
    M ((Node (KV 5 't') (Node (KV 3 's') Empty (Node (KV 4 'r') Empty Empty)) Empty))
  , MapKV_update (M (Node (KV 5 'a') (Node (KV 3 's') Empty (Node (KV 4 'r') Empty Empty)) Empty)) 5 (\_ -> Nothing) ===
    M ((Node (KV 5 'a') (Node (KV 3 's') Empty (Node (KV 4 'r') Empty Empty)) Empty))
  ]

test_MapKV_lookup =
  [ MapKV_lookup (M BSTree_emptyKV) 3 === Nothing
  , MapKV_lookup (M testKVBSTree) 7 === Just (KV 7 's')
  ]
    
test_MapKV_insert =
  [ foldl (\x (k,v) -> MapKV_insert x k v) (M BSTree_emptyKV)
    [(6,'a'),(4,'c'),(9,'r'),(5,'y'),(3,'l'),(7,'s'),(8,'q'),
    (10,'p'),(50,'o')] === M testKVBSTree
  , MapKV_insert (M BSTree_emptyKV) 4 'a' === M (Node (KV 4 'a') Empty Empty)
  , MapKV_insert (MapKV_insert (M BSTree_emptyKV) 4 'a') 4 'b' ===
    M (Node (KV 4 'a') Empty Empty)
  ] 