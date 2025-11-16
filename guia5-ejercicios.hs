import GHC.Base (BCO)
lista1 = [1,3..100]
lista2 = [100,99..1]
lista3 = [1,0..(-100)]
lista4 = [-19,-15..20]

longitud :: [Int] -> Int
longitud [] = 0
longitud (_:xs)=1+longitud xs

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- pertenece :: Int -> [Int] -> Bool
-- pertenece _ [] = False
-- pertenece a (x:xs) | a==x=True
--                    | otherwise = pertenece a xs 

contarOcurrencias :: Int -> [Int] -> Int
contarOcurrencias _ [] = 0
contarOcurrencias n (x:xs) | n == x = 1+ contarOcurrencias n xs
                           | otherwise = contarOcurrencias n xs 

todosNumerosIguales :: Int -> [Int] -> Bool
todosNumerosIguales _ [] = False
todosNumerosIguales n [x] = x == n  
todosNumerosIguales n (x:xs) = (x == n) && todosNumerosIguales n xs

-- Ejercicio 1. Definir las siguientes funciones sobre listas:
-- 1. longitud :: [t] -> Integer , que dada una lista devuelve su cantidad de elementos.
longitudBis :: [t] -> Integer
longitudBis [] = 0
longitudBis (_:xs) = 1+ longitudBis xs

-- 2. ultimo :: [t] -> t según la siguiente especificación:
ultimo :: [t] -> t
ultimo [t] = t
ultimo (_:xs) = ultimo xs

-- 3. principio :: [t] -> [t] según la siguiente especificación:
principio :: [t] -> [t]
principio [_] = []
principio (x:xs) = x : principio xs

-- 4. reverso :: [t] -> [t] según la siguiente especificación:
reverso :: [t] -> [t]
reverso lista = reversoAux lista []
  where
    reversoAux [] acumulador = acumulador      -- Caso base: devolver acumulador
    reversoAux (x:xs) acumulador = reversoAux xs (x:acumulador)

-- Ejercicio 2
-- 1. pertenece :: (Eq t) => t -> [t] -> Bool
perteneceBis :: (Eq t) => t -> [t] -> Bool
perteneceBis _ [] = False
perteneceBis t (x:xs) | t == x = True
                      | otherwise = perteneceBis t xs

-- 2. todosIguales :: (Eq t) => [t] -> Bool, que dada una lista devuelve verdadero sı́ y solamente sı́ todos sus ele- mentos son iguales.
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales (x:xs) | sonIguales x xs = True
                    | otherwise = False

sonIguales :: (Eq t) => t -> [t] -> Bool
sonIguales x [y] = x == y
sonIguales x y = (x == head y ) && sonIguales x (tail y)

-- 3. todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos (x:xs) | perteneceALista x xs = False
                      | otherwise = todosDistintos xs

perteneceALista :: (Eq t) => t -> [t] -> Bool
perteneceALista _ [] = False
perteneceALista y (z:zs) | y == z = True
                         | otherwise = perteneceALista y zs
                        
-- 4. hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | perteneceALista x xs = True
                    | otherwise =hayRepetidos xs
                
-- 5. quitar :: (Eq t) => t -> [t] -> [t], que dados un entero x y una lista xs, elimina la primera aparición de x en la lista xs (de haberla).
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar n (x:xs) | n == x = xs
                | otherwise = x : quitar n xs

-- 6. quitarTodos :: (Eq t ) => t -> [t] -> [t], que dados un entero x y una lista xs, elimina todas las apariciones de x en la lista xs (de haberlas)
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos n (x:xs) | n == x = quitarTodos n xs
                     | otherwise = x : quitarTodos n xs
            
-- 7. eliminarRepetidos :: (Eq t) => [t] -> [t] que deja en la lista una única aparición de cada elemento, eliminando las repeticiones adicionales.
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos( quitarTodos x (x:xs) )

-- Ejercicio 3
-- 2. Definir productoria
productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- 3. maximo :: [Integer] -> Integer
maximo :: [Integer] -> Integer
maximo [] = 0
maximo [x] = x
maximo (x:xs) | x >= maximo xs = x
              | otherwise = maximo xs

-- 4. sumarN :: Integer -> [Integer] -> [Integer]
sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [] = []
sumarN n (x:xs) = (x+n) : sumarN n xs

-- 5. sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumarN x (x:xs)


-- Ejercicio 8
-- 1. sumaTotal :: [[Integer]] -> Integer
sumaTotal :: [[Integer]] -> Integer
sumaTotal [] = 0
sumaTotal (xs:xss) = sumaElementos xs + sumaTotal xss
    where sumaElementos [] = 0
          sumaElementos (x:xs) = x +sumaElementos xs  

-- 2. cantidadDeApariciones :: Integer -> [[Integer]] -> Integer
cantidadDeApariciones :: Integer -> [[Integer]] -> Integer
cantidadDeApariciones _ [] = 0
cantidadDeApariciones n (xs:xss) = contarEnLista n xs + cantidadDeApariciones n xss
    where contarEnLista n [] = 0
          contarEnLista n (x:xs) | n==x=1+contarEnLista n xs
                             | otherwise = contarEnLista n xs 


-- 3. contarPalabras :: String ->[[String]] ->Int

contarPalabras :: String ->[[String]] ->Int
contarPalabras _ [] = 0
contarPalabras palabra (xs:xss) = palabraEsIgual palabra xs + contarPalabras palabra xss
    where palabraEsIgual _ [] = 0
          palabraEsIgual elemento (x:xs) | elemento == x = 1 + palabraEsIgual elemento xs
                                         | otherwise = palabraEsIgual elemento xs







-- Ejercicio Parcial 1
contarElemento :: String -> [String] -> Int
contarElemento _ [] = 0
contarElemento a (x:xs) | a == x = 1 + contarElemento a xs
                        | otherwise = contarElemento a xs

eliminarElemento :: String -> [String] -> [String]
eliminarElemento _ [] = []
eliminarElemento n (x:xs) | n == x = eliminarElemento n xs
                          | otherwise = x : eliminarElemento n xs

generarStock :: [String] ->[(String, Int)]
generarStock [] = []
generarStock (x:xs) = (x,contarElemento x (x:xs)) : generarStock (eliminarElemento x xs)

-- Ejercicio Parcial 2

estaEnStock :: String -> [(String, Int)] -> Bool
estaEnStock producto [] = False
estaEnStock producto (x:xs) | producto == fst x = True
                            | otherwise = estaEnStock producto xs

devolverCantidadDeStock :: String -> [(String, Int)] -> Int
devolverCantidadDeStock producto [] = 0
devolverCantidadDeStock producto (x:xs) | producto == fst x = snd x
                                        | otherwise = devolverCantidadDeStock producto xs

stockDeProducto :: [(String, Int)] -> String ->Int
stockDeProducto (x:xs) producto | estaEnStock producto (x:xs) == False = 0
                                | otherwise = devolverCantidadDeStock producto (x:xs)

-- Ejercicio Parcial 3
stockConPrecio :: (String, Int) ->[(String, Float)] -> Float
stockConPrecio _ [] = 0
stockConPrecio x (y:ys) | fst x == fst y = snd y
                        | otherwise = stockConPrecio x ys

operacionPorProducto :: (String, Int) ->[(String, Float)] -> Float
operacionPorProducto _ [] = 0
operacionPorProducto x (y:ys) = fromIntegral (snd x) * stockConPrecio x (y:ys)

dineroEnStock :: [(String, Int)] ->[(String, Float)] -> Float
dineroEnStock [] [] = 0
dineroEnStock [] (y:ys) = 0
dineroEnStock (x:xs) (y:ys) = operacionPorProducto x (y:ys) + dineroEnStock xs (y:ys)

-- Ejercicio Parcial 4

esMayorADiez :: [(String, Int)] -> String -> Bool
esMayorADiez stock prod = stockDeProducto stock prod > 10

aplicarDescuento :: [(String, Int)] -> (String, Float) -> (String, Float)
aplicarDescuento stock (prod, prec)
  | esMayorADiez stock prod = (prod, prec * 0.8)
  | otherwise = (prod, prec)

aplicarOferta :: [(String, Int)] -> [(String, Float)] -> [(String, Float)]
aplicarOferta stock [] = []
aplicarOferta stock (y:ys) = aplicarDescuento stock y : aplicarOferta stock ys

-- aplicarOferta :: [(String, Int)] ->[(String, Float)] ->[(String,Float)]
-- aplicarOferta _ [] = []
-- aplicarOferta [] (y:ys) =[]
-- aplicarOferta (x:xs) (y:ys) | 
                            