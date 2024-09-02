-- A
{- problema absoluto (n: Z) : N {
 requiere: {true}
 asegura: {res = |n|}
-}

absoluto :: Integer -> Integer
absoluto n | n > 0 = n
           | n < 0 = -n

-- B
{- problema maximoabsoluto (n: Z, m: Z) : Z
 requiere: {true}
 asegura : {res es el mayor absoluto de n y m}
 -}

maximoabsoluto :: Integer -> Integer -> Integer
maximoabsoluto n m | absoluto n > absoluto m = absoluto n 
                   | absoluto m > absoluto n = absoluto m 
                   | otherwise = absoluto n

-- C
{- problema maximo3 (n: Z, m: Z, x: Z) : Z
 requiere: {true}
 asegura : {res es el mayor número de n, m o x}
-}

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 n m x | n >= m && n >= x = n
              | m >= n && m >= x = m
              | x >= n && x >= m = x
              | otherwise = n

-- D
{- problema algunoEs0: (n: Q, m: Q) : Q 
 requiere: {true}
 asegura : {res es True si n = 0 o m = 0}
-}

algunoEs0 :: Float -> Float -> Bool
algunoEs0 n m | n == 0 || m == 0 = True
              | otherwise = False

-- E
{- problema ambosSon0: (n: Q, m: Q) : Q 
 requiere: {true}
 asegura : {res es True si n = 0 y m = 0}
-}

ambosSon0 :: Float -> Float -> Bool
ambosSon0 n m | n == 0 && m == 0 = True
              | otherwise = False

-- F
{- problema mismoIntervalo: (n: R, m: R) : Bool 
 requiere: {true}
 asegura : {res es True si n y m pertenecen a (-inf,3] o si pertenecen a (3,7] o si pertenecen a (7,inf)}
-}

mismoIntervalo :: Int -> Int -> Bool
mismoIntervalo n m | n <= 3 && m <= 3 = True
                   | 3 < n && n <= 7 && 3 < m && m <= 7 = True
                   | n > 7 && m > 7 = True
                   | otherwise = False

-- G
{- problema sumaDistintos: (n: Z, m: Z, x: Z) : Z 
 requiere: {true}
 asegura : {si n != m != x, entonces res = n+m+x}
 asegura : {si hay algún repetido, no sumarlo}
-}

sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos n m x | n /= m && n /= x && m /= x = n+m+x
                    | n /= m && n == x = m
                    | n /= m && m == x = n 
                    | n == m && m /= x = x
                    | otherwise = 0

-- H
{- problema esMultiploDe: (n: N, m: N) : Bool 
 requiere: {true}
 asegura : {res es True si n es multiplo de m}
-}

esMultiploDe :: Int -> Int -> Bool
esMultiploDe n m | mod n m == 0 = True
                 | otherwise = False

-- I
{- problema digitoUnidades: (n: Z) : Z 
 requiere: {true}
 asegura : {res es el digito de unidades de n}
-}

digitoUnidades :: Int -> Int 
digitoUnidades n = mod n 10

-- J
{- problema digitoDecenas: (n: Z) : Z 
 requiere: {n > 9}
 asegura : {res es el digito de decenas de n}
-}

digitoDecenas :: Int -> Int
digitoDecenas n = mod (div n 10) 10