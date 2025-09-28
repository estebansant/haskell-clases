returnLastDigit :: Int -> Int
returnLastDigit x = mod x 10


removeLastDigit :: Int -> Int
removeLastDigit x = div x 10

evenOdd :: Int -> Int
evenOdd x = mod x 2

sumaColumnasMFijo :: Int -> Int -> Int -> Int
sumaColumnasMFijo q a b | a == 1 = q^(1+b)
          | otherwise = q^(a+b) + sumaColumnasMFijo q (a-1) b

--sumaColumnasNFijo :: Int -> Int -> Int -> Int
--sumaColumnasNFijo q a b | b == 1 = q^(1+a)
--        | otherwise = q^(a+b) + sumaColumnasMFijo q a (b-1)

sumaFila ::Int -> Int -> Int -> Int
sumaFila q n 1 = q^(n+1)
sumaFila q n m = q^(n+m) + sumaFila q n (m-1)

sumaPotencias :: Int -> Int -> Int -> Int
sumaPotencias q 1 m = sumaFila q 1 m
sumaPotencias q n m =sumaFila q n m + sumaPotencias q (n-1) m



------
divideN:: Int -> Int -> Int
divideN n m | mod n m == 0 = m
            | otherwise = divideN n (m+1)

menorDivisor :: Int -> Int
menorDivisor n = divideN n 2