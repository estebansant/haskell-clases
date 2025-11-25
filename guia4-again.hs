-- Recursion sobre enteros

-- Ejercicio 1. Implementar la función fibonacci:: Integer ->Integer que devuelve el i-ésimo número de Fibonacci.
-- problema fibonacci (n: Z) : Z {
-- requiere: { n ≥ 0 }
-- asegura: { resultado = f ib(n) }
-- }

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci (n-1) + fibonacci (n-2)
        
-- Factorial

factorial :: Integer -> Integer
factorial n | n == 0 =1
            | n == 1 = 1
            | otherwise = n * factorial (n-1)

-- Ejercicio 2. Implementar una función parteEntera :: Float ->Integer según la siguiente especificación:
-- problema parteEntera (x: R) : Z {
-- requiere: { x ≥ 0 }
-- asegura: { resultado ≤ x < resultado + 1 }
-- }

parteEntera :: Float -> Integer
parteEntera 0 = 0
parteEntera x | x<0 = -1
              | otherwise = parteEntera(x-1) + 1



-- Ejercicio 3. Especificar e implementar la función esDivisible :: Integer ->Integer ->Bool que dados dos números
-- naturales determinar si el primero es divisible por el segundo. No está permitido utilizar las funciones mod ni div.

esDivisible :: Integer -> Integer -> Bool
esDivisible 0 b = True
esDivisible a b | a < 0 = False
                | otherwise = esDivisible (a-b) b


-- Ejercicio 4. Especificar e implementar la función sumaImpares :: Integer ->Integer que dado n ∈ N sume los primeros
-- n números impares. Por ejemplo: sumaImpares 3 ; 1+3+5 ⇝ 9 .

sumaImpares :: Integer -> Integer
sumaImpares n | n == 0 = 0
              | otherwise = 2*n -1 + sumaImpares (n-1)


-- Ejercicio 5. Implementar la función medioFact :: Integer ->Integer que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · ·
-- Por ejemplo:
-- medioFact 10 ; 10 ∗ 8 ∗ 6 ∗ 4 ∗ 2 ; 3840.
-- medioFact 9 ; 9 ∗ 7 ∗ 5 ∗ 3 ∗ 1 ; 945.
-- medioFact 0 ; 1.

medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n*medioFact(n-2)



-- Ejercicio 6. Implementar la función todosDigitosIguales :: Integer ->Bool que determina si todos los dı́gitos de un
-- número natural son iguales, es decir:
-- problema todosDigitosIguales (n: Z) : B {
-- requiere: { n > 0 }
-- asegura: { resultado = true ↔ todos los dı́gitos de n son iguales }
-- }

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n < 10 = True
                      | mod n 10 /= mod (div n 10) 10 = False
                      | otherwise = todosDigitosIguales (div n 10)


-- Ejercicio 7. Implementar la función iesimoDigito :: Integer ->Integer ->Integer que dado un n ∈ Z mayor o igual
-- a 0 y un i ∈ Z mayor o igual a 1 y menor o igual a la cantidad de dı́gitos de n, devuelve el i-ésimo dı́gito de n.
-- problema iesimoDigito (n: Z, i: Z) : Z {
-- requiere: { n ≥ 0 ∧ 1 ≤ i ≤ cantDigitos(n) }
-- asegura: { resultado = (n div 10cantDigitos(n)−i ) mod 10 }
-- }
-- problema cantDigitos (n: Z) : N {
-- requiere: { n ≥ 0 }
-- asegura: { n = 0 → resultado = 1}
-- asegura: { n ̸= 0 → (n div 10resultado−1 > 0 ∧ n div 10resultado = 0) }
-- }

cantDigitos :: Integer -> Integer
cantDigitos n | n <10 = 1
              | otherwise = 1 + cantDigitos (div n 10)

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n (10 ^ (cantDigitos n - i))) 10

iesimoDigitoBis :: Integer -> Integer -> Integer
iesimoDigitoBis n i | cantDigitos n == i = mod n 10
                    | otherwise = iesimoDigitoBis (div n 10) i

-- Ejercicio 8. Especificar e implementar la función sumaDigitos :: Integer ->Integer que calcula la suma de dı́gitos de
-- un número natural. Para esta función pueden utilizar div y mod.

leerUltimoDigito :: Integer -> Integer
leerUltimoDigito x = mod x 10

eliminarUltimoDigito :: Integer -> Integer
eliminarUltimoDigito x = div x 10

sumaDigitos :: Integer -> Integer
sumaDigitos x | x < 10 = x
              | otherwise = leerUltimoDigito x + sumaDigitos (eliminarUltimoDigito x)



-- Ejercicio 9. Especificar e implementar una función esCapicua :: Integer ->Bool que dado n ∈ N≥0 determina si n es
-- un número capicúa.

recuperarPrimerDigito :: Integer -> Integer
recuperarPrimerDigito x | x < 10 = x
                        | otherwise = recuperarPrimerDigito (div x 10)

eliminarPrimerDigito :: Integer -> Integer
eliminarPrimerDigito x = iesimoDigito x 1

esCapicua :: Integer -> Bool
esCapicua n | n < 10 = True
            | recuperarPrimerDigito n /= leerUltimoDigito n = False
            | otherwise = esCapicua (eliminarPrimerDigito (eliminarUltimoDigito n))

-- Ejercicios que se definen con sumatorias y sucesiones:

-- Ejercicio 10 a:

f1 :: Integer -> Integer
f1 n | n == 0 = 1
     | otherwise = 2^n + f1 (n-1)

-- Ejercicio 10 b:

f2 :: Integer -> Integer -> Integer
f2 n q | n == 1 = q
       | otherwise = q^n + f2 (n-1) q

-- Ejercicio 10 c :

f3 :: Integer -> Integer -> Integer
f3 n q | n == 1 = q + q ^(n+1)
       | otherwise = q^(n*2) + q^(n*2-1) + f3 (n - 1) q

    
-- Tambien se puede pensar usando una funcion auxiliar que me haga una sumatoria normal, pero en vez de pasarle n le paso 2n. lo aplico a continuacion

f4Aux :: Integer -> Integer -> Integer -> Integer
f4Aux m q n | m == n = q^n
            | otherwise = q ^m + f4Aux (m-1) q n


f4 :: Integer -> Integer -> Integer
f4 n q = f4Aux (2*n) q n


-- Ejercicio 11

factorialAux :: Integer -> Integer
factorialAux n | n == 0 =1
               | otherwise = n * factorialAux (n-1)

eAprox :: Integer -> Float
eAprox n | n == 0 = 1
         | otherwise = 1/(fromIntegral (factorialAux n)) + eAprox (n-1)


-- Ejercicio 12
-- Completar
sucesionAux :: Integer -> Float
sucesionAux m | m <= 1 = 2 + 1/2
              | otherwise = 1/(sucesionAux (m-1))

raizDe2Aprox :: Integer -> Float
raizDe2Aprox n | n==1 = 2
               | otherwise = 1/(sucesionAux (n-1)) + raizDe2Aprox (n-1)

-- Ejercicio 13

sumaSobreM :: Integer -> Integer -> Integer
sumaSobreM n m | m == 1 = n
               | otherwise = n^m + sumaSobreM n (m-1)

sumaDoble :: Integer -> Integer -> Integer
sumaDoble n m | n == 1 = sumaSobreM n m 
              | otherwise = sumaSobreM n m + sumaDoble (n-1) m

-- Ejercicio 14. Especificar e implementar una función sumaPotencias :: Integer ->Integer ->Integer ->Integer que dados tres naturales q, n, m sume todas las potencias de la forma q a+b con 1 ≤ a ≤ n y 1 ≤ b ≤ m.

sumaPotM :: Integer -> Integer -> Integer -> Integer
sumaPotM q n m | m == 1 = q ^ (n+1)
               | otherwise = q ^ (n+m) + sumaPotM q n (m-1)


sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m | n == 1 = sumaPotM q 1 m
                    | otherwise = sumaPotM q n m + sumaPotencias q (n-1) m

-- Ejercicio 15. Implementar una función sumaRacionales :: Integer ->Integer ->Float que dados dos naturales n, m
-- sume todos los números racionales de la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m, es decir:

sumaRacM :: Integer -> Integer -> Float
sumaRacM n m | m ==1 = fromIntegral n
             | otherwise = (fromIntegral n)/(fromIntegral m) + sumaRacM n (m-1)

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales n m | n == 1 = sumaRacM 1 m
                   | otherwise = sumaRacM n m + sumaRacionales (n-1) m 

-- Ejercicio 16. Recordemos que un entero p > 1 es primo si y sólo si no existe un entero k tal que 1 < k < p y k divida a p.
-- a) Implementar menorDivisor :: Integer ->Integer que calcule el menor divisor (mayor que 1) de un natural n pasado
-- como parámetro.
-- b) Implementar la función esPrimo :: Integer ->Bool que indica si un número natural pasado como parámetro es primo.
-- c) Implementar la función sonCoprimos :: Integer ->Integer ->Bool que dados dos números naturales indica si no
-- tienen algún divisor en común mayor estricto que 1.
-- d) Implementar la función nEsimoPrimo :: Integer ->Integer que devuelve el n-ésimo primo (n ≥ 1). Recordar que el
-- primer primo es el 2, el segundo es el 3, el tercero es el 5, etc.

divideA :: Integer -> Integer -> Bool
divideA n m | n == 0 = True
            | n < 0 = False
            | otherwise = divideA(n-m) m

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n m | divideA n m == True = m
                      | otherwise = menorDivisorDesde n (m+1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

--------

esPrimo :: Integer -> Bool
esPrimo n | n == 1 = False
          | n == menorDivisor n = True
          | otherwise = False

-------

mcd :: Integer -> Integer -> Integer
mcd a b | b == 0 = a
        | otherwise = mcd b (mod a b)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = mcd n m == 1

-------

buscarPrimo :: Integer -> Integer -> Integer
buscarPrimo n m | esPrimo m && n == 1 = m
                | esPrimo m == True = buscarPrimo (n-1) (m+1)
                | otherwise = buscarPrimo n (m+1)

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n = buscarPrimo n 2 

-- Ejercicio 17. Implementar la función esFibonacci :: Integer ->Bool según la siguiente especificación:
-- problema esFibonacci (n: Z) : B {
-- requiere: { n ≥ 0 }
-- asegura: { resultado = true ↔ n es algún valor de la secuencia de Fibonacci definida en el ejercicio 1}
-- }

-- fibonacci :: Integer -> Integer
-- fibonacci n | n == 0 = 0
--             | n == 1 = 1
--             | otherwise = fibonacci (n-1) + fibonacci (n-2)

verificarFibo :: Integer -> Integer -> Integer -> Bool
verificarFibo n a b | a == n = True
                     | a > n = False
                     | otherwise = verificarFibo n b (a + b)

esFibonacci :: Integer -> Bool
esFibonacci n = verificarFibo n 0 1


-- Ejercicio 18. Implementar una función mayorDigitoPar :: Integer ->Integer según la siguiente especificación:
-- problema mayorDigitoPar (n: N) : N {
-- requiere: { T rue }
-- asegura: { resultado es el mayor de los dı́gitos pares de n. Si n no tiene ningún dı́gito par, entonces resultado es -1.
-- }

obtenerDigito :: Integer -> Integer
obtenerDigito n = mod n 10

obternerRestoNumero :: Integer -> Integer
obternerRestoNumero n = div n 10

esPar:: Integer -> Bool
esPar n | mod n 2 == 0 = True
        | otherwise = False

buscarMayor :: Integer -> Integer -> Integer
buscarMayor n max | n == 0 = max
                  | esPar (obtenerDigito n) && obtenerDigito n > max = buscarMayor (obternerRestoNumero n) (obtenerDigito n)
                  | otherwise = buscarMayor (obternerRestoNumero n) max

mayorDigitoPar :: Integer -> Integer
mayorDigitoPar n = buscarMayor n (-1)


-- }
-- Ejercicio 19. Implementar la función esSumaInicialDePrimos :: Integer ->Bool según la siguiente especificación:
-- problema esSumaInicialDePrimos (n: Z) : B {
-- requiere: { n ≥ 0 }
-- asegura: { resultado = true ↔ n es igual a la suma de los m primeros números primos, para algún m.}
-- }

irSumando :: Integer -> Integer -> Bool
irSumando m n = verificarSuma n (nEsimoPrimo m + nEsimoPrimo (m+1))

verificarSuma :: Integer -> Integer -> Bool
verificarSuma n primo | n == 0 = False
                      | n == 1 = False
                      | n == primo = True
                      | primo > n = False
                      | otherwise = irSumando primo n

esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = verificarSuma n 1


-- Ejercicio 20. Especificar e implementar la función tomaValorMax :: Integer ->Integer ->Integer que dado un número
-- entero n1 ≥ 1 y un n2 ≥ n1 devuelve algún m entre n1 y n2 tal que sumaDivisores(m) = máx{sumaDivisores(i) | n1 ≤
-- i ≤ n2 }



-- Ejercicio 21. Especificar e implementar una función pitagoras :: Integer ->Integer ->Integer ->Integer que dados
-- m, n , r ∈ N≥0 , cuente cuántos pares (p, q) con 0 ≤ p ≤ m y 0 ≤ q ≤ n satisfacen que p2 + q 2 ≤ r2 . Por ejemplo:
-- pitagoras 3 4 5 ⇝ 20
-- pitagoras 3 4 2 ⇝ 6
