discriminante :: Int -> Int-> Int -> Int
discriminante x y z | (y^2 - 4*x*z) > 0 = 2
                    | (y^2 - 4*x*z) == 0 = 1
                    | otherwise = 0

cantidadSolucionesCuadraticas :: Int-> Int-> Int-> Int
cantidadSolucionesCuadraticas a b c | discriminante a b c == 2 = 2
                                    | discriminante a b c == 1 = 1
                                    | discriminante a b c == 0 = 0

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt (cuadradoA + cuadradoB)
  where
    cuadradoA = a ^ 2
    cuadradoB = b ^ 2

areaCirculo :: Float -> Float
areaCirculo r = piVal*r^2
    where piVal = 3.14159
