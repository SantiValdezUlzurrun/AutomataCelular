module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import System.Random

import Regla110

tam :: Int
tam = 100

vEntAnAs :: Display
vEntAnAs = InWindow "Regla110" (640, 480) (100, 100)

colorDelFondo = makeColor 0 0 0 255

main :: IO ()
main = do
	listaRand <- sequence $ replicate tam $ randomRIO (0, 1)
	simulate vEntAnAs colorDelFondo 1 (genInicial tam listaRand) mostrarModelo avanzarGen
