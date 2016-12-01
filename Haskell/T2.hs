module Trabalho2(getLast, squareOdd) where


{- retorna o ultimo elemento de uma lista de inteiros -}
listLast :: [Int] -> Int
getLast [x] = x 
getLast (_:xs) = listLast xs
getLast [] = error "Lista vazia!"

 n = [x*((2*x - 1)/2) | x <- [1..n]]
