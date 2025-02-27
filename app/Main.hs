module Main where

import BinMap (BinMap, emptyMap)

run :: (Read k, Ord k, Show k, Read v, Show v) => BinMap k v -> IO ()
run oldMap = do
    addend <- readLn
    let newMap = oldMap <> addend
    print newMap
    run newMap

main :: IO ()
main = run (emptyMap :: BinMap Int Int)