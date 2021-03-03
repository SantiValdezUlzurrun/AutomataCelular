module Regla110 where

import Graphics.Gloss
import System.Random

import Modelo
import Renderizado

colorDelFondo = colorCelulaMuerta

mainRegla110 :: IO ()
mainRegla110 = do
	listaRand <- sequence $ replicate n $ randomRIO (0, 1)
	simulate ventana colorDelFondo 10 (genInicial listaRand) mostrarModelo avanzarGen
