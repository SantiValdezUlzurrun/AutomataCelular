module Main where

import System.Random
import Regla110

iter :: Int
iter = 20
tam :: Int
tam = 100

genInicial :: Int -> IO [Int]
genInicial n = sequence $ replicate n $ randomRIO (0, 1)

main :: IO ()
main = do
	inicial <- genInicial tam 
	imprimirUniverso iter inicial
