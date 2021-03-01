module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Regla110

tam :: Int
tam = 100

vEntAnAs :: Display
vEntAnAs = InWindow "Regla110" (640, 480) (100, 100)

colorDelFondo = makeColor 0 0 0 255

main :: IO ()
main = simulate vEntAnAs colorDelFondo 1 (genInicial tam) mostrarModelo avanzarGen
