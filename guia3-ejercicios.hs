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

-- Ejercicio 3. Implementar una función estanRelacionados :: Integer -> Integer -> Bool

estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b | mod a b == 0 = True
                      | otherwise = False

-- Ejercicio 4
-- a) productoInterno: calcula el producto interno entre dos tuplas de R × R.
productoInterno :: (Int, Int) -> (Int, Int) -> Int
productoInterno (a,b) (c,d) = a*c + b*d

-- b) esParMenor: dadas dos tuplas de R × R, decide si cada coordenada de la primera tupla es menor a la coordenada correspondiente de la segunda tupla.
esParMenor :: (Int, Int) -> (Int, Int) -> Bool
esParMenor (a,b) (c,d) | a < c && b < d = True
                       | otherwise = False
                    
-- c) distancia: calcula la distancia euclı́dea entre dos puntos de R2 .
distancia :: (Float,Float) -> Float
distancia (a, b) = sqrt(a^2 + b^2)

-- d) sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (a,b,c) = a+b+c

-- e) sumarSoloMultiplos: dada una terna de números enteros y un natural, calcula la suma de los elementos de la terna que son múltiplos del número natural.
sumarSoloMultiplos :: (Int, Int, Int)-> Int -> Int
sumarSoloMultiplos (a, b, c) d | estanRelacionados a d  && estanRelacionados b d  && estanRelacionados c d  = a+b+c
                               | estanRelacionados a d  && estanRelacionados b d = a+b
                               | estanRelacionados a d  && estanRelacionados c d = a+c
                               | estanRelacionados b d  && estanRelacionados c d = b+c
                               | estanRelacionados a d = a
                               | estanRelacionados b d = b
                               | estanRelacionados c d = c
                               | otherwise = 0

-- posPrimerPar: dada una terna de enteros, devuelve la posición del primer número par si es que hay alguno, o devuelve 4 si son todos impares.
esPar :: Integer -> Bool
esPar n | mod n 2 == 0 = True
        | otherwise = False


posPrimerPar :: (Integer, Integer, Integer)-> Integer
posPrimerPar (a,b,c) | esPar a && esPar b && esPar c = a
                     | esPar a && esPar b = a
                     | esPar a && esPar c = a
                     | esPar b && esPar c = b
                     | esPar c = c
                     | otherwise = 4

-- Ejercicio 5. Implementar la función todosMenores :: (Integer, Integer, Integer) -> Bool

f2 :: Integer -> Integer
f2 x | x <= 7 =x^2
     | x > 7 = 2*x-1

g2 :: Integer -> Integer
g2 y | esPar y = div y 2
     | otherwise = 3*y+1

todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (a,b,c) | f2 a > g2 a && f2 b > g2 b && f2 c > g2 c = True
                     | otherwise = False

-- Ejercicio 6
type Anio = Integer
type EsBisiesto = Bool
bisiesto :: Anio -> EsBisiesto
bisiesto x | mod x 4 /= 0 = False
           | mod x 100 == 0 && mod x 400 /= 0 = False
           | mod x 4 == 0 = True
        
bisiesto2 :: Anio -> EsBisiesto
bisiesto2 x | mod x 400 == 0 = True
            | mod x 100 == 0 = False
            | mod x 4 == 0 = True
            | otherwise = False

-- Ejercicio 7
absolutoFloat :: Float -> Float
absolutoFloat x | x < 0 = -x
                | x > 0 = x
                | otherwise = 0

distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (a,b,c) (d,e,f) = absolutoFloat(a-d) + absolutoFloat(b-e) + absolutoFloat(c-f)

-- Ejercicio 8

sumaUltimosDosDigitos :: Integer -> Integer
sumaUltimosDosDigitos x = mod x 10 + mod valor 10
    where valor = div (absoluto x) 10

comparar :: Integer -> Integer -> Integer
comparar a b | sumaUltimosDosDigitos a  < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a  > sumaUltimosDosDigitos b = -1
             | sumaUltimosDosDigitos a  == sumaUltimosDosDigitos b = 0