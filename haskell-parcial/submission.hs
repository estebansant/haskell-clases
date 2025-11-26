{--
Yo: Esteban Santiago Pizzani
Certifico que el siguiente archivo fue elaborado únicamente por mí, sin ayuda de otras personas o herramientas.
--}

module ParcialSoluciones where

-- Ejercicio 1
esCuadradoDePrimo :: Integer -> Bool
esCuadradoDePrimo 1 = False
esCuadradoDePrimo n | esPrimo n == True = False
                    | calcularCuadrado (divisoresMayoresAUno n 2) == n = True
                    | otherwise = False

-- Funciones Auxiliares Ejercicio 1
esPrimo :: Integer -> Bool
esPrimo n | n == 1 = False
          | otherwise = divisoresMayoresAUno n 2 == n

divisoresMayoresAUno :: Integer -> Integer -> Integer
divisoresMayoresAUno n m | m == n = m
                         | mod n m == 0 = m
                         | otherwise = divisoresMayoresAUno n (m+1)
                        
calcularCuadrado :: Integer -> Integer
calcularCuadrado n = n*n

-- Ejercicio 2
-- posParesFormanEscalera :: [Integer] -> Bool
-- posParesFormanEscalera lista = False

-- Ejercicio 3
listadoDePeliculas :: [(Integer, [String])] -> [(String, Integer)]
listadoDePeliculas (x:xs) = armarListaDePelis (fst x, snd x) 

armarListaDePelis :: (Integer, [String])  -> [(String,Integer)]
armarListaDePelis (año, []) = []
armarListaDePelis (año, (titulo:resto)) = (titulo, año) : armarListaDePelis (año, resto)

-- Ejercicio 4
-- eliminarFilaQueMasSuma :: [[Integer]] -> [[Integer]]
-- eliminarFilaQueMasSuma [] = []
-- eliminarFilaQueMasSuma (xs:xss) = maximoDeLaLista (sumarElementosDeLaFila xs : eliminarFilaQueMasSuma xss)

-- sumarElementosDeLaFila :: [Integer] -> Integer
-- sumarElementosDeLaFila [] = 0
-- sumarElementosDeLaFila (x:xs) = x + sumarElementosDeLaFila xs

-- maximoDeLaLista :: [Integer] -> Integer
-- maximoDeLaLista [] = 0
-- maximoDeLaLista (x:xs) | x >= maximoDeLaLista xs = x
--                         | otherwise = maximoDeLaLista xs

-- quitarFila :: Integer -> [[Integer]] -> [Integer]
-- quitarFila _ [] = []
-- quitarFila n (x:xs) | n == maximoDeLaLista (head (x:xs))= xs
--                 | otherwise = quitarFila n xs

{--
Siendo la última modificación con la solución final:
01/17/2027 12:56
--}
