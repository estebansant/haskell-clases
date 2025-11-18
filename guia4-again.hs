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




-- Ejercicio 6. Implementar la función todosDigitosIguales :: Integer ->Bool que determina si todos los dı́gitos de un
-- número natural son iguales, es decir:
-- problema todosDigitosIguales (n: Z) : B {
-- requiere: { n > 0 }
-- asegura: { resultado = true ↔ todos los dı́gitos de n son iguales }
-- }




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

-- Ejercicio 8. Especificar e implementar la función sumaDigitos :: Integer ->Integer que calcula la suma de dı́gitos de
-- un número natural. Para esta función pueden utilizar div y mod.



-- Ejercicio 9. Especificar e implementar una función esCapicua :: Integer ->Bool que dado n ∈ N≥0 determina si n es
-- un número capicúa.

-- Ejercicios que se definen con sumatorias y sucesiones:










-- Ejercicio 15. Implementar una función sumaRacionales :: Integer ->Integer ->Float que dados dos naturales n, m
-- sume todos los números racionales de la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m, es decir:



-- Ejercicio 16. Recordemos que un entero p > 1 es primo si y sólo si no existe un entero k tal que 1 < k < p y k divida a p.
-- a) Implementar menorDivisor :: Integer ->Integer que calcule el menor divisor (mayor que 1) de un natural n pasado
-- como parámetro.
-- b) Implementar la función esPrimo :: Integer ->Bool que indica si un número natural pasado como parámetro es primo.
-- c) Implementar la función sonCoprimos :: Integer ->Integer ->Bool que dados dos números naturales indica si no
-- tienen algún divisor en común mayor estricto que 1.
-- d) Implementar la función nEsimoPrimo :: Integer ->Integer que devuelve el n-ésimo primo (n ≥ 1). Recordar que el
-- primer primo es el 2, el segundo es el 3, el tercero es el 5, etc.



-- Ejercicio 17. Implementar la función esFibonacci :: Integer ->Bool según la siguiente especificación:
-- problema esFibonacci (n: Z) : B {
-- requiere: { n ≥ 0 }
-- asegura: { resultado = true ↔ n es algún valor de la secuencia de Fibonacci definida en el ejercicio 1}
-- }



-- Ejercicio 18. Implementar una función mayorDigitoPar :: Integer ->Integer según la siguiente especificación:
-- problema mayorDigitoPar (n: N) : N {
-- requiere: { T rue }
-- asegura: { resultado es el mayor de los dı́gitos pares de n. Si n no tiene ningún dı́gito par, entonces resultado es -1.
-- }



-- }
-- Ejercicio 19. Implementar la función esSumaInicialDePrimos :: Integer ->Bool según la siguiente especificación:
-- problema esSumaInicialDePrimos (n: Z) : B {
-- requiere: { n ≥ 0 }
-- asegura: { resultado = true ↔ n es igual a la suma de los m primeros números primos, para algún m.}
-- }



-- Ejercicio 20. Especificar e implementar la función tomaValorMax :: Integer ->Integer ->Integer que dado un número
-- entero n1 ≥ 1 y un n2 ≥ n1 devuelve algún m entre n1 y n2 tal que sumaDivisores(m) = máx{sumaDivisores(i) | n1 ≤
-- i ≤ n2 }



-- Ejercicio 21. Especificar e implementar una función pitagoras :: Integer ->Integer ->Integer ->Integer que dados
-- m, n , r ∈ N≥0 , cuente cuántos pares (p, q) con 0 ≤ p ≤ m y 0 ≤ q ≤ n satisfacen que p2 + q 2 ≤ r2 . Por ejemplo:
-- pitagoras 3 4 5 ⇝ 20
-- pitagoras 3 4 2 ⇝ 6

