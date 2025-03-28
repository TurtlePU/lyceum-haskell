module Main where

import BinMap (BinMap, emptyMap)

run :: (Read k, Ord k, Show k, Read v, Num v, Show v) => BinMap k v -> IO ()
run oldMap = do
    addend <- readLn
    let newMap = oldMap <> addend
    print newMap
    -- let r = foldMap show newMap
    print (sum newMap)
    run newMap

main :: IO ()
main = run (emptyMap :: BinMap Int Int)