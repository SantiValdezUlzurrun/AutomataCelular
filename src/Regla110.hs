module Regla110 where

imprimirUniverso :: Int -> [Int] -> IO ()
imprimirUniverso n generacion =
	if n == 0
		then return ()
	else do
		imprimirFila generacion
		imprimirUniverso (n-1) nuevaGeneracion
		where
			nuevaGeneracion = avanzarGen generacion

imprimirFila :: [Int] -> IO ()
imprimirFila fila = putStrLn cadena
	where
		cadena = "| " ++ (map (\x -> if x == 1 then '*' else ' ') fila) ++ " |"

-- Avanza de generacion segun la regla 110
avanzarGen :: [Int] -> [Int]
avanzarGen gen = map' remplazarRegla gen'
	where 
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
remplazarRegla :: Int -> Int -> Int -> Int
remplazarRegla 0 0 0 = 0
remplazarRegla 0 0 1 = 1
remplazarRegla 0 1 0 = 1
remplazarRegla 0 1 1 = 1
remplazarRegla 1 0 0 = 0
remplazarRegla 1 0 1 = 1
remplazarRegla 1 1 0 = 1
remplazarRegla 1 1 1 = 0

