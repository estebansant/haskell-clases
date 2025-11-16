-- Ejercicio 1.
-- a) Implementar la función parcial f :: Integer -> Integer definida por extensión de la siguiente manera:
-- f (1) = 8
-- f (4) = 131
-- f (16) = 16
-- y cuya especificación es:
-- problema f (n : Z) : Z {
-- requiere: {n = 1 ∨ n = 4 ∨ n = 16}
-- asegura: {(n = 1 → res = 8) ∧ (n = 4 → res = 131) ∧ (n = 16 → res = 16)}
-- }

f :: Integer -> Integer
f x | x==1 = 8
    | x == 4 = 131
    | x == 16 = 16

-- b) Análogamente, especificar e implementar la función parcial g :: Integer -> Integer
-- g(8) = 16
-- g(16) = 4
-- g(131) = 1

g :: Integer -> Integer
g y | y == 8 = 16
    | y == 16 = 4
    | y == 131 = 1

-- c) A partir de las funciones definidas en los ı́tems a) y b), implementar las funciones parciales h = f ◦ g y k = g ◦ f

h :: Integer -> Integer
h x = f (g x)

k :: Integer -> Integer
k x = g (f x)
-- Ejercicio 2. ⋆ Especificar e implementar las siguientes funciones, incluyendo su signatura.
-- a) absoluto: calcula el valor absoluto de un número entero.

absoluto :: Integer -> Integer
absoluto x | x < 0 = -x
      | x > 0 = x
      | x == 0 = 0  

-- b) maximoAbsoluto: devuelve el máximo entre el valor absoluto de dos números enteros.
maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x y | absoluto x > absoluto y = absoluto x
                   | absoluto y > absoluto x = absoluto y
                   | absoluto y == absoluto x = absoluto x


-- c) maximo3: devuelve el máximo entre tres números enteros.

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | x > y && x > z = x
              | y > x && y > z = y
              | z > x && z > y = z
              | x == y && x > z = x
              | x == z && x > y = x
              | y == z && y > x = y
              | x == z && x == y = x

-- d) algunoEsCero: dados dos números racionales, decide si alguno es igual a 0 (resolverlo con y sin pattern matching).

algunoEsCero :: Integer -> Integer -> Bool
algunoEsCero x y | (x == 0 )|| (y == 0) = True
                 | otherwise = False

algunoEsCeroBis :: Integer -> Integer -> Bool
algunoEsCeroBis 0 y = True
algunoEsCeroBis x 0 = True
algunoEsCeroBis x y = False


-- e) ambosSonCero: dados dos números racionales, decide si ambos son iguales a 0 (resolverlo con y sin pattern matching).

ambosSonCero :: Integer -> Integer -> Bool
ambosSonCero x y | (x == 0) && (y == 0) = True
                 | otherwise = False
                
ambosSonCeroBis :: Integer -> Integer -> Bool
ambosSonCeroBis 0 0 = True
ambosSonCeroBis x y = False


-- f) enMismoIntervalo: dados dos números reales, indica si están relacionados por la relación de equivalencia en R cuyas
-- clases de equivalencia son: (−∞, 3], (3, 7] y (7, ∞), o dicho de otra manera, si pertenecen al mismo intervalo.

enMismoIntervalo :: Integer -> Integer -> Bool
enMismoIntervalo x y | (x <= 3) && (y <= 3) = True
                     | (x > 3 && x <= 7) && (y > 3 && y <= 7) = True
                     | (x > 7) && (y > 7) = True
                     | otherwise = False

-- g) sumaDistintos: que dados tres números enteros calcule la suma sin sumar repetidos (si los hubiera).

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | (x == y) && (x == z) = 0
                    | (x /= y) && (x /= z) && (y /= z) = x + y + z
                    | x == y = x + z
                    | x == z = x + y
                    | otherwise = x + y

-- h) esMultiploDe: dados dos números naturales, decide si el primero es múltiplo del segundo.

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | otherwise = False

-- i) digitoUnidades: dado un número entero, extrae su dı́gito de las unidades.

digitoUnidades :: Integer -> Integer
digitoUnidades x = mod x 10


-- j) digitoDecenas: dado un número entero mayor a 9, extrae su dı́gito de las decenas.

digitoDecenas :: Integer -> Integer
digitoDecenas x = div (mod x 100) 10



-- Ejercicio 3. Implementar una función estanRelacionados :: Integer -> Integer -> Bool
-- problema estanRelacionados (a : Z, b : Z) : Bool {
-- requiere: {a ̸= 0 ∧ b ̸= 0}
-- asegura: {(res = true) ↔ (a ∗ a + a ∗ b ∗ k = 0 para algún k ∈ Z con k ̸= 0)}
-- }
-- Por ejemplo:
-- estanRelacionados 8 2 ⇝ True
-- estanRelacionados 7 3 ⇝ False
-- porque existe k = −4 tal que 82 + 8 × 2 × (−4) = 0
-- porque no existe un k entero tal que 72 + 7 × 3 × k = 0

-- Ejercicio 4. ⋆ Especificar e implementar las siguientes funciones utilizando tuplas para representar pares y ternas de
-- números.
-- a) productoInterno: calcula el producto interno entre dos tuplas de R × R.


-- b) esParMenor: dadas dos tuplas de R × R, decide si cada coordenada de la primera tupla es menor a la coordenada correspondiente de la segunda tupla.


-- c) distancia: calcula la distancia euclı́dea entre dos puntos de R2 .


-- d) sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.


-- e) sumarSoloMultiplos: dada una terna de números enteros y un natural, calcula la suma de los elementos de la terna que son múltiplos del número natural.
-- Por ejemplo:
-- sumarSoloMultiplos (10,-8,-5) 2 ⇝ 2
-- sumarSoloMultiplos (66,21,4) 5 ⇝ 0
-- sumarSoloMultiplos (-30,2,12) 3 ⇝ -18


-- f) posPrimerPar: dada una terna de enteros, devuelve la posición del primer número par si es que hay alguno, o devuelve 4 si son todos impares.


-- g) crearPar :: a -> b -> (a, b): a partir de dos componentes, crea un par con esos valores. Debe funcionar para elementos de cualquier tipo.


-- h) invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como parámetro. Debe funcionar para elementos de cualquier tipo.


-- i) Reescribir los ejercicios productoInterno, esParMenor y distancia usando el siguiente renombre de tipos:
-- type Punto2D = (Float, Float)


-- Ejercicio 5. Implementar la función todosMenores :: (Integer, Integer, Integer) -> Bool
-- problema todosMenores (t : Z × Z × Z) : Bool {
-- requiere: {T rue}
-- asegura: {(res = true) ↔ ((f (t0 ) > g(t0 )) ∧ (f (t1 ) > g(t1 )) ∧ (f (t2 ) > g(t2 )))}
-- }


-- problema f (n : Z) : Z {
-- requiere: {T rue}
-- asegura: {(n ≤ 7 → res = n2 ) ∧ (n > 7 → res = 2n − 1)}
-- }


-- problema g (n : Z) : Z {
-- requiere: {T rue}
-- asegura: {Si n es un número par entonces res = n/2, en caso contrario, res = 3n + 1}
-- }


-- Ejercicio 6. Usando los siguientes tipos:
-- type Anio = Integer
-- type EsBisiesto = Bool
-- Programar la función bisiesto :: Anio -> EsBisiesto según la siguiente especificación:
-- problema bisiesto (año : Z) : Bool {
-- requiere: {T rue}
-- asegura: {(res = f alse) ↔ (año no es múltiplo de 4, o bien, año es múltiplo de 100 pero no de 400)}
-- }
-- Por ejemplo:
-- bisiesto 1901 ⇝ False
-- bisiesto 1900 ⇝ False
-- bisiesto 1904 ⇝ True
-- bisiesto 2000 ⇝ True

-- Ejercicio 7.
-- a) Implementar la función:
-- distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) -> Float
-- problema distanciaManhattan (p : R × R × R, q : R × R × R) : R {
-- requiere: {T rue}
-- P2
-- asegura: {res = i=0 |pi − qi |}
-- }
-- Por ejemplo:
-- distanciaManhattan (2, 3, 4) (7, 3, 8) ⇝ 9
-- distanciaManhattan ((-1), 0, (-8.5)) (3.3, 4, (-4)) ⇝ 12.8


-- b) Reimplementar la función teniendo en cuenta el siguiente tipo: type Punto3D = (Float, Float, Float)


-- Ejercicio 8. Implementar la función comparar :: Integer -> Integer -> Integer
-- problema comparar (a : Z, b : Z) : Z {
-- requiere: {T rue}
-- asegura: {(res = 1) ↔ (sumaUltimosDosDigitos(a) < sumaUltimosDosDigitos(b))}
-- asegura: {(res = −1) ↔ (sumaUltimosDosDigitos(a) > sumaUltimosDosDigitos(b))}
-- asegura: {(res = 0) ↔ (sumaUltimosDosDigitos(a) = sumaUltimosDosDigitos(b))}
-- }
-- problema sumaUltimosDosDigitos (x : Z) : Z {
-- requiere: {T rue}
-- j k
-- asegura: {res = (|x| mód 10) + ( |x|
-- 10
-- mód 10)}
-- }
-- Por ejemplo:
-- comparar 45 312 ⇝ -1 porque 45 ≺ 312 y 4 + 5 > 1 + 2.
-- comparar 2312 7 ⇝ 1 porque 2312 ≺ 7 y 1 + 2 < 0 + 7.
-- comparar 45 172 ⇝ 0 porque no vale 45 ≺ 172 ni tampoco 172 ≺ 45.


-- Ejercicio 9. A partir de las siguientes implementaciones en Haskell, describir en lenguaje natural qué hacen y especificarlas.
-- a) f1 :: Float -> Float
-- f1 n | n == 0 = 1
-- | otherwise = 0d) f4 :: Float -> Float -> Float
-- f4 x y = ( x + y )/2
-- b) f2 :: Float -> Float
-- f2 n | n == 1 = 15
-- | n == -1 = -15e) f5 :: ( Float , Float ) -> Float
-- f5 (x , y ) = ( x + y )/2
-- c) f3 :: Float -> Float
-- f3 n | n <= 9 = 7
-- | n >= 3 = 5f) f6 :: Float -> Int -> Bool
-- f6 a b = truncate a == b