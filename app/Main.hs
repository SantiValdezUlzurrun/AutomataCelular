module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import System.Random

import Modelo
import Regla110
import Renderizado


vEntAnAs :: Display
vEntAnAs = InWindow "Regla 110" (anchoPantalla, altoPantalla) (100, 100)

colorDelFondo = makeColor 0 0 0 255

main :: IO ()
main = do
	listaRand <- sequence $ replicate n $ randomRIO (0, 1)
	simulate vEntAnAs colorDelFondo 1 (genInicial n listaRand) mostrarModelo avanzarGen
