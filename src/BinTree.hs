{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module BinTree where
import Data.Void (Void)

data BinTree a
    = Leaf
    | Node
        { left :: BinTree a
        , value :: a
        , right :: BinTree a
        , size :: Int
        }
        deriving Read

instance Foldable BinTree where
    foldr :: (a -> b -> b) -> b -> BinTree a -> b
    foldr _ s Leaf = s
    foldr f s Node {..} = foldr f (f value (foldr f s right)) left
    -- foldr (+) 0 (((1)2())3(4)) = 1 + (2 + (3 + 4))

    length Leaf = 0
    length Node {..} = size

    minimum = fst . deleteMin
    maximum = snd . deleteMax

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
insert v n@Node {..}
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
    let (min, left') = deleteMin left
     in (min, node left' value right)

deleteMax :: Ord a => BinTree a -> (BinTree a, a)
deleteMax Leaf = error "tree is empty"
deleteMax Node {right=Leaf, ..} = (left, value)
deleteMax Node {..} =
    let (right', max) = deleteMax right
     in (node left value right', max)

delete :: Ord a => a -> BinTree a -> BinTree a
delete v Leaf = Leaf
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

data Entry k v = Entry { eKey :: k, eValue :: v } -- (k, v)
    deriving (Read, Show)

instance Eq k => Eq (Entry k v) where
    Entry k v == Entry k' v' = k == k'

instance Ord k => Ord (Entry k v) where
    compare (Entry k v) (Entry k' v') = compare k k'

newtype BinMap k v = BinMap { binMap :: BinTree (Entry k v) }
    deriving (Read, Show)

emptyMap :: BinMap k v
emptyMap = BinMap empty

singletonMap :: k -> v -> BinMap k v
singletonMap k v = BinMap (singleton (Entry k v))

insertMap :: Ord k => k -> v -> BinMap k v -> BinMap k v
insertMap k v (BinMap t) = BinMap (insert (Entry k v) t)

deleteMap :: Ord k => k -> BinMap k v -> BinMap k v
deleteMap k (BinMap t) = BinMap (delete (Entry k undefined) t)

instance Ord k => Semigroup (BinMap k v) where
    BinMap t <> BinMap t' = BinMap (t <> t')

instance Ord k => Monoid (BinMap k v) where
    mempty = emptyMap

run :: (Read k, Ord k, Show k, Read v, Show v) => BinMap k v -> IO ()
run map = do
    addend <- readLn
    let newMap = map <> addend
    print newMap
    run newMap

unit :: ()
unit = ()

pair :: (Int,String)
pair = (5, "a")

void :: Void
void = error "no such value"

q1 :: [BinTree ()]
q1 = [singleton (), empty]

q2 :: [BinTree Void]
q2 = [empty]

main :: IO ()
main = run (emptyMap :: BinMap Int Int)