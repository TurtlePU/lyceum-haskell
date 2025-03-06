module Playground where

import Prelude hiding (Functor)

example :: [Integer]
example = [1, 2, 43]

-- printAllInList :: Show a => [a] -> IO ()
-- printAllInList [] = return ()
-- printAllInList (x:xs) = do
--     putStr (show x)
--     printAllInList xs

allToString :: Show a => [a] -> String
-- allToString = concatMap show
allToString [] = ""
allToString [x] = show x
allToString (x:xs) = show x ++ " " ++ allToString xs

alt :: [String] -> String
alt = unwords

viaUnwords :: Show a => [a] -> String
-- viaUnwords xs = unwords [ show x | x <- xs ]
viaUnwords = unwords . map show

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map

instance Functor IO where
    fmap f comp = do
        x <- comp
        return (f x)

-- functor laws!
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

printAll :: IO ()
printAll = putStrLn ("(" ++ allToString example ++ ")")