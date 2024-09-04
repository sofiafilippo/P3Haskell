{-problema estanRelacionados (a:Z, b:Z) : Bool {
requiere: {a̸= 0 ∧ b̸= 0}
asegura: {(res = true) ↔ a ∗ a + a ∗ b ∗ k = 0 para algún k ∈ Z con k̸= 0)}
}
-}

-- a * a = -(a * b * k) -> a = b * k

estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b = mod a b == 0  