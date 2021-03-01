module Regla110 where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort

import System.Random

data Celula = Viva | Muerta deriving (Eq, Show)
type Tablero = [[Celula]]

genInicial :: Int -> Tablero
genInicial n = [map f lista]
	where 
		f = (\x -> if x == 1 then Viva else Muerta)
		lista = [1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1]


mostrarModelo :: Tablero -> Picture
mostrarModelo tablero = pictures $ map pictures imagenesEnPos
	where
		cantCol = length tablero
		filasConPosRelY = zip tablero (take cantCol [1..cantCol])
		imagenesEnPos = map mostrarFila filasConPosRelY

mostrarFila :: ([Celula], Int) -> [Picture]
mostrarFila (celulas, posRelY) = map trasladar imagenesConPosRel
	where
		imagenes = map imagenCelula celulas
		cantCel = length celulas
		posRel = zip (take cantCel [1..cantCel]) (take cantCel (repeat posRelY))
		imagenesConPosRel = zip imagenes posRel


trasladar :: (Picture, (Int, Int)) -> Picture
trasladar (imagen, (posRelX, posRelY)) = translate x y imagen 
	where
		x = 10.0 * fromIntegral posRelX
		y = -10.0 * fromIntegral posRelY

imagenCelula :: Celula -> Picture
imagenCelula cel = if cel == Viva 
					then imagenCelulaViva
				   else imagenCelulaMuerta

colorCelulaViva :: Color
colorCelulaViva = makeColorI 255 50 50 255

imagenCelulaViva :: Picture
imagenCelulaViva = pictures [color colorCelulaViva $ rectangleSolid 10 10]

colorCelulaMuerta :: Color
colorCelulaMuerta = makeColorI 0 0 255 255

imagenCelulaMuerta :: Picture
imagenCelulaMuerta = pictures [color colorCelulaMuerta $ rectangleSolid 10 10]


-- Avanza de generacion segun la regla 110
avanzarGen :: ViewPort -> Float -> Tablero -> Tablero
avanzarGen _ _ tablero = tablero ++ [map' remplazarRegla gen']
	where
		gen = last tablero
		primero = head gen
		ultimo = last gen
		gen' = ultimo : gen ++ [primero]

map' :: (a -> a -> a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:y:z:xs) = f x y z : map' f (y:z:xs)
map' _ (x:y:xs) = []
map' _ (x:xs) = []

{--
111 	110 	101 	100 	011 	010 	001 	000
 0	   	 1 		 1 		 0 		 1 		 1 		 1 		 0 
--} 
remplazarRegla :: Celula -> Celula -> Celula -> Celula
remplazarRegla Muerta Muerta Muerta = Muerta
remplazarRegla Muerta Muerta Viva = Viva
remplazarRegla Muerta Viva Muerta = Viva
remplazarRegla Muerta Viva Viva = Viva
remplazarRegla Viva Muerta Muerta = Muerta
remplazarRegla Viva Muerta Viva = Viva
remplazarRegla Viva Viva Muerta = Viva
remplazarRegla Viva Viva Viva = Muerta


