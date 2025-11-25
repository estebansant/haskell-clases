-- Ejercicio 1
--a) longitud

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--b) Ultimo

ultimo :: [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo xs

--c) principio:

principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x:principio xs

--d) reverso

reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- Ejercicio 2:
--a)

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece t [] = False
pertenece t (x:xs) | t == x = True
                   | otherwise = pertenece t xs

--b) 
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:xs) | x /= y = False
                      | otherwise = todosIguales (y:xs)

--c)
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:y:xs) | x == y = False
                        | otherwise = todosDistintos (y:xs)

--d)
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x:xs) | comparacion x xs == False = hayRepetidos xs
                    | otherwise = True
    where
        comparacion x [] = False
        comparacion x (y:ys) | x == y = True
                             | otherwise = comparacion x ys

--e)
quitar:: (Eq t) => t -> [t] -> [t]
quitar e [] = []
quitar e (x:xs) | e ==x = xs
                | otherwise = x : quitar e xs

--f)
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos e [] = []
quitarTodos e (x:xs) | e == x = quitarTodos e xs
                     | otherwise = x : quitarTodos e xs

--g)

comparacion :: (Eq t) => t -> [t] -> [t]
comparacion e [] = []
comparacion e (x:xs) | e == x = comparacion e xs
                     | otherwise = x : comparacion e xs


eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (comparacion x (x:xs))

--h)

existeElemento :: (Eq t) => t -> [t] -> Bool
existeElemento e [] = False
existeElemento e (x:xs) | e == x = True
                        | otherwise = existeElemento e xs

subconjunto :: (Eq t) => [t] -> [t] -> Bool
subconjunto [] _ = True
subconjunto (x:xs) ys = (existeElemento x ys) && (subconjunto xs ys)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos l1 l2 = (subconjunto l1 l2) && (subconjunto l2 l1)

--i)

listaInvertida :: (Eq t) => [t] -> [t]
listaInvertida [] = []
listaInvertida (x:xs) = listaInvertida xs ++ [x]

igualReverso :: (Eq t) => [t] -> [t] -> Bool
igualReverso [] [] = True
igualReverso (x:xs) (y:ys) | x == y = igualReverso xs ys
                           | otherwise = False

capicua :: (Eq t) => [t] -> Bool
capicua [] = True
capicua [x] = True
capicua (x:xs) = igualReverso (x:xs) (listaInvertida (x:xs))

-- Ejercicio 3

--a) Sumatoria
sumatoria2 :: [Integer] -> Integer
sumatoria2 [] = 0
sumatoria2 (x:xs) = x + sumatoria xs

--b) Productoria
productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

--c) Maximo
compararMayor :: Integer -> [Integer] -> Integer
compararMayor e [] = e
compararMayor e (x:xs) | e >= x = compararMayor e xs
                       | otherwise = compararMayor x xs

maximo :: [Integer] -> Integer
maximo (x:xs) = compararMayor x xs

--d) SumarN

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [] = []
sumarN n (x:xs) = (x+n) : sumarN n xs

--e) SumarElPrimero

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs)

--f) SumarElUltimo
ultimoReverso :: [Integer] -> [Integer]
ultimoReverso [] = []
ultimoReverso (x:xs) = ultimoReverso xs ++ [x]

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo (x:xs) = ultimoReverso (sumarElPrimero (ultimoReverso (x:xs)))

--g) Pares

esElPar :: Integer -> Bool
esElPar n = mod n 2 == 0

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | esElPar x == True = x : pares xs
             | otherwise = pares xs

--h) MultiplosDeN

loDivideN :: Integer -> [Integer] -> Bool
loDivideN n (x:xs) = mod x n == 0 

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | loDivideN n (x:xs) = x : multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

--i) Ordenar

invertirOrden :: [Integer] -> [Integer]
invertirOrden [] = []
invertirOrden (x:xs) = invertirOrden xs ++ [x]

eliminarMaximo :: Integer -> [Integer] -> [Integer]
eliminarMaximo n [] = []
eliminarMaximo n (x:xs) = quitar n (x:xs)

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar (x:xs) = ordenar (eliminarMaximo (maximo (x:xs)) (x:xs)) ++ [maximo(x:xs)]