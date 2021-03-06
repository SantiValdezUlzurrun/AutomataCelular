module Renderizado where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture

import Modelo

{-- Numero de celulas --}
n :: Int
n = 100

anchoPantalla :: Int
anchoPantalla = 640

altoPantalla :: Int
altoPantalla = 480

ventana :: Display
ventana = InWindow "Regla 110" (anchoPantalla, altoPantalla) (100, 100)

anchoCelula :: Float
anchoCelula = fromIntegral anchoPantalla / fromIntegral n

altoCelula :: Float
altoCelula = fromIntegral altoPantalla / fromIntegral n

genInicial :: [Int] -> Tablero
genInicial lista = [map f lista]
	where 
		f = (\x -> if x == 1 then Viva else Muerta)


mostrarModelo :: Tablero -> Picture
mostrarModelo tablero = translate (fromIntegral anchoPantalla * (-0.5))
                        		  (fromIntegral altoPantalla * 0.5) 
							   	  escena
							   
	where
		tablero' = drop ((length tablero) - n) tablero
		cantCol = length tablero'
		filasConPosRelY = zip tablero' (take cantCol [1..cantCol])
		imagenesEnPos = map mostrarFila filasConPosRelY
		escena = pictures $ map pictures imagenesEnPos

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
		x = anchoCelula * fromIntegral posRelX
		y = -altoCelula * fromIntegral posRelY

imagenCelula :: Celula -> Picture
imagenCelula cel = if cel == Viva 
					then imagenCelulaViva
				   else imagenCelulaMuerta

colorCelulaViva :: Color
colorCelulaViva = makeColorI 100 160 100 255

imagenCelulaViva :: Picture
imagenCelulaViva = pictures [color colorCelulaViva $ rectangleSolid anchoCelula altoCelula]

colorCelulaMuerta :: Color
colorCelulaMuerta = makeColorI 40 40 40 255

imagenCelulaMuerta :: Picture
imagenCelulaMuerta = pictures [color colorCelulaMuerta $ rectangleSolid anchoCelula altoCelula]


