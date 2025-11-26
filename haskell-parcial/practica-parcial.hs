--Ejercicio1)

calcularCuadrado :: Integer -> Integer
calcularCuadrado n = n^2

numeroPrimo:: Integer  -> Bool
numeroPrimo n | n == 1 = False
              | otherwise = divisoresMayoresAUno n 2 == n

divisoresMayoresAUno :: Integer -> Integer -> Integer
divisoresMayoresAUno n m | n == m = m
                         | mod n m == 0 = m
                         | otherwise = divisoresMayoresAUno n (m+1)

esCuadradoDePrimo :: Integer -> Bool
esCuadradoDePrimo n | n == 1 = False
                    | numeroPrimo n == True = False
                    | calcularCuadrado (divisoresMayoresAUno n 2) == n = True
                    | otherwise = False

-- Ejercicio 2

posParesFormanEscalera :: [Integer] -> Bool
posParesFormanEscalera [] = True
posParesFormanEscalera [x] = True
posParesFormanEscalera [x,y] = True
posParesFormanEscalera (x:y:z:xs) | z /= 1+x = False
                                  | z == 1+x = posParesFormanEscalera (z:xs)
                                  | otherwise = False 

--Ejercicio 3

desempaquetar :: (Integer, String) -> (String,Integer)
desempaquetar (anio,peli) = (peli,anio)

listadoDePeliculas :: [(Integer,[String])] -> [(String,Integer)]
listadoDePeliculas [] = []
listadoDePeliculas ((anio,[]):xs) = listadoDePeliculas xs
listadoDePeliculas ((anio,(peli:otrasPelis)):xs)  = desempaquetar (anio,peli) : listadoDePeliculas ((anio,otrasPelis):xs)
                                                 

-- Ejercicio 4

-- sumarElementosFila :: [Integer] -> Integer
-- sumarElementosFila [] =0
-- sumarElementosFila (x:xs) = x + sumarElementosFila xs

-- sumarElementosDevolverLista :: [Integer] -> [Integer]
-- sumarElementosDevolverLista [] = []
-- sumarElementosDevolverLista (x:xs) = [x + sumarElementosFila xs]

-- maximaSuma :: [Integer] -> Integer -> Integer
-- maximaSuma [] e = e
-- maximaSuma (x:xs) e | x >= e = maximaSuma xs x
--                     | otherwise = maximaSuma xs e

-- eliminarFilaQueMasSuma :: [[Integer]] -> [[Integer]]
-- eliminarFilaQueMasSuma [] = []
-- eliminarFilaQueMasSuma [x] = [x]
-- eliminarFilaQueMasSuma ((x:xs):(y:ys):zs) | maximaSuma (sumarElementosFila (x:xs) : sumarElementosDevolverLista (y:ys)) (sumarElementosFila (x:xs)) == sumarElementosFila (x:xs) = eliminarFilaQueMasSuma ((y:ys):zs)
--                                    | otherwise = (x:xs) : eliminarFilaQueMasSuma ((y:ys):zs)