-- Ejercicio 1

f :: Integer -> Integer
f x | x==1 = 8
    | x==4 = 131
    | x==16=16
    | otherwise = 0

g :: Integer -> Integer
g y | y==8=16
    |y==16=4
    |y==131=1
    |otherwise = 0

h :: Integer -> Integer
h n = f(g n)

k :: Integer -> Integer
k n = g(f n)

-- Ejercicio 2
-- a) absoluto: calcula el valor absoluto de un número entero.
absoluto :: Integer -> Integer
absoluto x | x < 0 = -x
           | x > 0 = x
           | otherwise = 0
        
-- b) maximoAbsoluto: devuelve el máximo entre el valor absoluto de dos números enteros.
maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x y | absoluto x > absoluto y = absoluto x
                   | otherwise = absoluto y

-- c) maximo3: devuelve el máximo entre tres números enteros.
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x > y && x > z = x
              | y > x && y > z = y
              | z > x && z > y = z
              | x == y && y == z = x
            
-- d) algunoEsCero: dados dos números racionales, decide si alguno es igual a 0 (resolverlo con y sin pattern matching).
algunoEsCero :: Float -> Float -> Bool
algunoEsCero x y | x == 0 || y == 0 = True
                 | otherwise = False
                
algunoEsCeroPatt :: Float -> Float -> Bool
algunoEsCeroPatt 0 y = True
algunoEsCeroPatt x 0 = True
algunoEsCeroPatt x y = False

-- e) ambosSonCero: dados dos números racionales, decide si ambos son iguales a 0 (resolverlo con y sin pattern matching).
ambosSonCero :: Float -> Float -> Bool
ambosSonCero x y | x==0 && y== 0 = True
                 | otherwise = False
ambosSonCeroPatt :: Float -> Float -> Bool
ambosSonCeroPatt 0 0 = True
ambosSonCeroPatt x y = False

-- f) enMismoIntervalo: dados dos números reales, indica si están relacionados por la relación de equivalencia en R cuyas clases de equivalencia son: (−∞, 3], (3, 7] y (7, ∞), o dicho de otra manera, si pertenecen al mismo intervalo.

enMismoIntervalo :: Int -> Int -> Bool
enMismoIntervalo x y | x <= 3 && y <= 3 = True
                     | x > 3 && x <= 7 && y >3 && y <= 7 = True
                     | x >7 && y > 7 = True
                     | otherwise = False
        
-- g) sumaDistintos: que dados tres números enteros calcule la suma sin sumar repetidos (si los hubiera).
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | x /= y && x /= z && y/= z = x+y+z
                    | x /= y && x /=z && y == z = x
                    | y /= x && y /=z && x == z = y
                    | z /= y && z /=x && x == y = z
                    | otherwise = 0

-- h) esMultiploDe: dados dos números naturales, decide si el primero es múltiplo del segundo.
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod y x == 0 = True
                 | otherwise = False

-- i) digitoUnidades: dado un número entero, extrae su dı́gito de las unidades.
digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10 

-- j) digitoDecenas: dado un número entero mayor a 9, extrae su dı́gito de las decenas.
eliminarUltimoDigito :: Int -> Int
eliminarUltimoDigito x = div x 10

digitoDecenas :: Int -> Int
digitoDecenas x = eliminarUltimoDigito (mod x 100)