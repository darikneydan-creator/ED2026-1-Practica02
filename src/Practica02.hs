module Practica02 where

--BINARIOS
data Bit = O | I 
        deriving (Show, Eq)

type Binario = [Bit]

--BINARIOS

toDecimal :: Binario -> Int
toDecimal [] = 0
toDecimal (b : bs) = if b == I then (2 ^ largo bs) + toDecimal bs else 0 + toDecimal bs

toBin :: Int -> Binario
toBin 0 = []
toBin n = if mod n 2 == 1 then pegar (toBin (div n 2)) [I] else pegar (toBin (div n 2)) [O]

suma :: Binario -> Binario -> Binario
suma b1 b2 = toBin (toDecimal b1 + toDecimal b2)

--LISTAS 

--Funciones auxiliares
concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = (x:concatena xs ys)

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = concatena (reversa xs) [x] --O bien reversa xs ++ [x]

contiene :: (Eq a) => [a] -> a -> Bool
contiene [] _ = False
contiene (x:xs) y = if x == y then True else contiene xs y

diferencia :: (Eq a) => [a] -> [a] -> [a]
diferencia [] _ = []
diferencia (x:xs) ys = if contiene (ys) x then diferencia xs ys else concatena ([x]) (diferencia xs ys)


--FIN DE LAS FUNCIONES AUXILIARES 

palindromo :: (Eq a)=>[a] -> Bool 
palindromo []= True
palindromo (xs) = if (xs) == reversa(xs) then True else False



--Funcion principal que calcula la diferencia simetrica (Aquellos elementos que esten en la union pero no en la interseccion)
diferenciaSimetrica :: (Eq a) => [a] -> [a] -> [a]
diferenciaSimetrica [] []= []
diferenciaSimetrica (xs) [] = xs
diferenciaSimetrica [] (ys) = ys
diferenciaSimetrica (xs) (ys) = concatena (diferencia xs ys) (diferencia ys xs)

--Conjunto potencia
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia []= [[]]
conjuntoPotencia (x:xs) = concatena (conjuntoPotencia xs) [x:ys | ys <- conjuntoPotencia xs]



--LISTAS DE LONGITUD PAR
--Esta va de regalo
type ListaPar a b = [(a,b)]

--Longitud
longitud :: ListaPar a b -> Int
longitud = undefined

--Map
myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d 
myMap = undefined

--Sumar pares
sumaPares :: ListaPar a b -> (a,b)
sumaPares = undefined

--Filter pares
myFilter :: ((a,b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter = undefined


--FUNCIONES AUXILIARES

--Funcion que te da un int que representa el largo de una lista
largo :: [l] -> Int
largo [] = 0
largo (_:l) = 1 + largo l

--Funcion que pega dos listas
pegar :: [a] -> [a] -> [a]
pegar [] l = l
pegar (a:l) l2 = a:pegar l l2
