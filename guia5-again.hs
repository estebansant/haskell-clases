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
eliminarRepetidos (x:xs) | comparacion x (x:xs) == (x:xs) = x : eliminarRepetidos xs
                         | otherwise = x : eliminarRepetidos (comparacion x (x:xs))