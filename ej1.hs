-- A
{- problema f (n: Z) : Z {
 requiere: {n = 1 ∨ n = 4 ∨ n = 16}
 asegura: {(n = 1 → result = 8) ∧ (n = 4 → result = 131) ∧ (n = 16 → result = 16)}
-}

f :: Integer -> Integer
f n | n == 1 = 8
    | n == 4 = 131
    | n == 16 = 16
    | otherwise = error "Dato de entrada inválido"

-- B

g :: Integer -> Integer
g n | n == 8 = 16
    | n == 16 = 4
    | n == 131 = 1
    | otherwise = error "Dato de entrada inválido"