-- Universidad Complutense de Madrid
-- Facultad de Informatica
-- Programacion Declarativa: Practica final
-- Erik Karlgren Domercq

--- Relación
data Rel a = R [(a,a)] deriving (Show, Read)



--- Apartado 1
-- Determina si (r:rs) es una relación, es decir, no tiene elementos repetidos

rmDups :: Eq a => [a] -> [a]
rmDups = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

esRelacion :: Eq a => Rel a -> Bool
esRelacion (R rs) = length rs == length (rmDups rs)




 --- Apartado 2
instance Eq a => Eq (Rel a) where
     R ax == R bx = length_ax == length_bx && length_ax == length [a | a <- ax, a `elem` bx]
                    where length_ax = length ax
                          length_bx = length bx




--- Apartado 3
-- Dominio
dominio :: Eq a => Rel a -> [a]
dominio (R rs) = rmDups [x | (x,_) <- rs] -- Coje los elementos del dominio y quita duplicados


-- Soporte
soporte :: Eq a => Rel a -> [a]
soporte (R rs) = rmDups [a | (x,y) <- rs, a <- [x,y]]


-- Relacion de equivalencia
relReflexiva :: Eq a => Rel a -> Bool
relReflexiva (R rs) = all (\x -> (x,x) `elem` rs) (soporte (R rs))

relSimetrica :: Eq a => Rel a -> Bool
relSimetrica (R rs) = all (\(x, y) -> (y, x) `elem` rs) rs

relTransitiva :: Eq a => Rel a -> Bool
relTransitiva (R rs) = all (\(x, y) -> all (\(a,z) -> a/=y || (x,z) `elem` rs) rs) rs

relEquivalencia :: Eq a => Rel a -> Bool
relEquivalencia r = relReflexiva r && relSimetrica r && relTransitiva r


-- Conjunto cociente
claseDeEquivalencia :: Eq a => Rel a -> a -> [a]
claseDeEquivalencia (R rs) elem = if relEquivalencia (R rs) 
    then rmDups [x | (a,b) <- filter (\(a,b) -> a==elem || b==elem) rs, x <-[a,b]]
    else error "No es relacion de equivalencia"

conjCociente :: Eq a => Rel a -> [[a]]
conjCociente rel = foldr (anyadirSiClaseEquivExiste . claseDeEquivalencia rel) [] (soporte rel)
                      where anyadirSiClaseEquivExiste xs acc = if any (\ys -> head xs `elem` ys) acc then acc else xs:acc

-- Genera conjunto tal que, dados n y m, {(x,y) | n <= x, y <= m, x divisor de y}
generaDiv :: Int -> Int -> Rel Int
generaDiv n m = R [(x,y) | x <- [n..m], y <- [n..m], mod y x == 0]

-- Relacion mayor o igual que (a R b <-> a >= b)
generaGE :: Ord a => [a] -> Rel a
generaGE xs = R [(x,y) | x <- xs, y <- xs, x >= y]

-- Composicion: r3 = r1 o r2, es decir, para cada aRb de r1 y bRc de r2, r3 tendrá aRc
composicion :: Eq a => Rel a -> Rel a -> Rel a
composicion (R ax) (R bx) = R $ rmDups [(x,z) | (x,y) <- ax, (_,z) <- filter (\(a,b) -> a==y) bx]




--- Apartado 4: entrada/salida
-- Introducir una relacion por consola (de momento tiene que ser de enteros)
unirRelaciones :: Eq a => Rel a -> Rel a -> Rel a
unirRelaciones (R ax) (R bx) = R $ rmDups $ ax ++ bx

introRel :: IO(Rel Int)
introRel = do
    putStrLn "Introduzca los valores de cada relacion (deben ser numeros). Para abortar, deja 2 lineas en blanco."
    introRelAux (R [])

introRelAux :: Rel Int -> IO(Rel Int)
introRelAux r = do
    putStr "Para (a,b), a <- "
    aLine <- getLine
    putStr "Para (a,b), b <- "
    bLine <- getLine

    if aLine /= "" && bLine /= ""
        then do
            let a = read aLine::Int
            let b = read bLine::Int
            introRelAux $ unirRelaciones (R [(a,b)]) r
        else return r


-- Leer y mostrar una relacion
muestraRel :: IO()
muestraRel = do
    r <- introRel
    let sop = soporte r
    putStr " "
    putStrLn $ foldr (\x acc -> acc ++ " " ++ show x) "" sop
    muestraRelLoop r sop sop

muestraRelLoop :: (Eq a, Show a) => Rel a -> [a] -> [a] -> IO()
muestraRelLoop _ _ [] = putStrLn ""
muestraRelLoop (R rs) ys (x:xs) = do
    putStrLn $ foldr (\y acc -> if (x,y) `elem` rs then acc ++ " X" else acc ++ "  ") (show x) ys
    muestraRelLoop (R rs) ys xs




--- Extra: incluye varias relaciones definidas sobre varios conjuntos soporte
r1 = R [("a","a"),("b","b"),("a","b")] -- Falla la simetría
r2 = R [(1,2), (2,1), (1,1)] -- Falla la reflexividad
r3 = R [("a","a"),("b","b"),("c","c"),("a","b"),("b","c")] -- Falla la transitividad y la simetría
r4 = R [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1),(2,3),(3,2)] -- Relación de equivalencia
r5 = R [(1,1),(2,2),(3,3)] -- Relacion de equivalencia con varias clases de equivalencia
r6 = R [(1,1),(3,3),(2,2)] -- Relacion de equivalencia. Igual a r5.
