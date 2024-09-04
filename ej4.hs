-- A
{-problema prodInt (a:RxR, b:RxR) : R {
requiere: {true}
asegura: {res = a1xb1 + a2xb2}
}
-}

type TuplaR = (Float, Float)

prodInt :: TuplaR -> TuplaR -> Float
prodInt (a1, a2) (b1,b2) = (a1*b1) + (a2*b2) 

-- B
{-problema todoMenor (a:RxR, b:RxR) : Bool {
requiere: {true}
asegura: {(res = true) ↔ a1 < b1 y a2 < b2)}
}
-}

type TernaZ = (Int, Int, Int)

todoMenor :: TuplaR -> TuplaR -> Bool
todoMenor a b = (fst a < fst b) && (snd a < snd b)

-- C
{-problema distanciaPuntos (a:RxR, b:RxR) : R {
requiere: {true}
asegura: {res = √(a1-b1)^2 + (a2-b2)^2}
}
-}

distanciaPuntos :: TuplaR -> TuplaR -> Float
distanciaPuntos (a1, a2) (b1,b2) = sqrt ((a1-b1)^2 + (a2-b2)^2)

-- D
{-problema sumaTerna (a:ZxZxZ) : Z {
requiere: {true}
asegura: {res = a1+a2+a3}
}
-}

sumaTerna :: TernaZ -> Int
sumaTerna (a1,a2,a3) = a1 + a2 + a3

-- E
{-problema sumarSoloMultiplos (a:ZxZxZ, b: N) : Z {
requiere: {true}
asegura: {res = a1+a2+a3 con a1,a2 y a3 multiplos de b}
asegura: {res = 0 si ninguno es multiplo de b}
}
-}

sumarSoloMultiplos :: TernaZ -> Int -> Int
sumarSoloMultiplos (a1,a2,a3) b | mod a1 b == 0 && mod a2 b /= 0 && mod a3 b /= 0 = a1
                                | mod a1 b /= 0 && mod a2 b == 0 && mod a3 b /= 0 = a2
                                | mod a1 b /= 0 && mod a2 b /= 0 && mod a3 b == 0 = a3
                                | mod a1 b == 0 && mod a2 b == 0 && mod a3 b /= 0 = a1+a2
                                | mod a1 b /= 0 && mod a2 b == 0 && mod a3 b == 0 = a2+a3
                                | mod a1 b == 0 && mod a2 b /= 0 && mod a3 b == 0 = a1+a3
                                | mod a1 b == 0 && mod a2 b == 0 && mod a3 b == 0 = a1+a2+a3
                                | otherwise = 0


-- F
{-problema posPrimerPar (a:ZxZxZ) : Z {
requiere: {true}
asegura: {res = 1 si mod a1 2 == 0, 2 si mod a2 2 == 0, 3 si mod a3 2 == 0, 4 si ninguna de las anteriores es verdadera}
}
-}

posPrimerPar :: TernaZ -> Int
posPrimerPar (a1, a2, a3) | mod a1 2 == 0 = 1
                          | mod a2 2 == 0 = 2
                          | mod a3 2 == 0 = 3
                          | otherwise = 4

-- G
{-problema crearPar (a:T, b:T) : TxT {
requiere: {true}
asegura: {res = axb}
}
-}

crearPar :: t -> f -> (t, f)
crearPar a b = (a,b)

