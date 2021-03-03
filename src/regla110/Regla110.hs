module Regla110 where

import Graphics.Gloss.Data.ViewPort

import Modelo

-- Avanza de generacion actualizando el modelo
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
Regla de substitucion de 110
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


