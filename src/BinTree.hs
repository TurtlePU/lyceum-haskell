{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module BinTree where

data BinTree a
    = Leaf
    | Node
        { left :: BinTree a
        , value :: a
        , right :: BinTree a
        , size :: Int
        }

instance Read a => Read (BinTree a) where
    readsPrec n ('(':s) =
        [ (node l v r, w)
        | (l, t) <- readsPrec n s
        , (v, u) <- readsPrec n t
        , (r, ')':w) <- readsPrec n u
        ]
        ++ [(Leaf, s)]
    readsPrec _ s = [(Leaf, s)]

instance Foldable BinTree where
    foldr :: (a -> b -> b) -> b -> BinTree a -> b
    foldr _ s Leaf = s
    foldr f s Node {..} = foldr f (f value (foldr f s right)) left
    -- foldr (+) 0 (((1)2())3(4)) = 1 + (2 + (3 + 4))

    length Leaf = 0
    length Node {..} = size

    minimum = fst . deleteMin
    maximum = snd . deleteMax

-- instance Functor BinTree where
-- instead:
-- Set.map; Set.mapMonotonic

mapMonotonic :: (a -> b) -> BinTree a -> BinTree b
mapMonotonic _ Leaf = Leaf
mapMonotonic f Node {..} = Node
    { left = mapMonotonic f left
    , value = f value
    , right = mapMonotonic f right
    , ..
    }

instance Show a => Show (BinTree a) where
    show Leaf = ""
    show Node {..} = "(" ++ show left ++ show value ++ show right ++ ")"

empty :: BinTree a
empty = Leaf

node :: BinTree a -> a -> BinTree a -> BinTree a
node left value right = Node { size = length left + 1 + length right, ..}

singleton :: a -> BinTree a
singleton v = node empty v empty

insert :: Ord a => a -> BinTree a -> BinTree a
insert v Leaf = singleton v
insert v Node {..}
    | v > value = node left value (insert v right)
    | v < value = node (insert v left) value right
    | otherwise = node left v right

cut :: Ord a => a -> BinTree a -> (BinTree a, a, BinTree a)
cut v Leaf = (empty, v, empty)
cut v Node {..}
    | v > value =
        let (lor, v', ror) = cut v right
         in (node left value lor, v', ror)
    | v < value =
        let (lol, v', rol) = cut v left
         in (lol, v', node rol value right)
    | otherwise = (left, value, right)

merge :: Ord a => BinTree a -> BinTree a -> BinTree a
merge Leaf t = t
merge t Leaf = t
merge n1@(Node l1 v1 r1 s1) n2@(Node l2 v2 r2 s2)
    | s1 <= s2 =
        let (l1', _, r1') = cut v2 n1
         in node (merge l1' l2) v2 (merge r1' r2)
    | otherwise =
        let (l2', v, r2') = cut v1 n2
         in node (merge l1 l2') v (merge r1 r2')

deleteMin :: Ord a => BinTree a -> (a, BinTree a)
deleteMin Leaf = error "tree is empty"
deleteMin Node {left=Leaf, ..} = (value, right)
deleteMin Node {..} =
    let (minValue, left') = deleteMin left
     in (minValue, node left' value right)

deleteMax :: Ord a => BinTree a -> (BinTree a, a)
deleteMax Leaf = error "tree is empty"
deleteMax Node {right=Leaf, ..} = (left, value)
deleteMax Node {..} =
    let (right', maxValue) = deleteMax right
     in (node left value right', maxValue)

delete :: Ord a => a -> BinTree a -> BinTree a
delete _ Leaf = Leaf
delete v Node {..}
    | v < value = node (delete v left) value right
    | v > value = node left value (delete v right)
    | otherwise = case (left, right) of
        (Leaf, _) -> right
        (_, Leaf) -> left
        _
            | length left < length right ->
                let (left', v') = deleteMax left
                 in node left' v' right
            | otherwise ->
                let (v', right') = deleteMin right
                 in node left v' right'

-- (++) :: [a] -> [a] -> [a]
-- (+) :: Int -> Int -> Int
-- xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
-- x + (y + z) = (x + y) + z

instance Ord a => Semigroup (BinTree a) where
    (<>) = merge

instance Ord a => Monoid (BinTree a) where
    mempty = empty

listToTree :: Ord a => [a] -> BinTree a
listToTree = foldMap singleton
