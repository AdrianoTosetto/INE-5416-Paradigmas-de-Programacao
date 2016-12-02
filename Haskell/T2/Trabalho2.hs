module Trabalho2(getLast,intOddSummationSerie,oddNumbersSummation,
				intEvenSummationSerie,evenNumbersSummation,intEvenSquareSerie,
				intEvenSquareSummation,intOddSquareSerie,intOddSquareSummation,twoAsSerie,eulerConstantAsSerie) where


{- retorna o ultimo elemento de uma lista de inteiros -}
getLast :: [a] -> a
getLast [x] = x 
getLast (_:xs) = getLast xs
getLast [] = error "Lista vazia!"

intOddSummationSerie n = [x*((2*x-1) + 1)/2 | x <- [1..n]]
oddNumbersSummation  n = getLast(intOddSummationSerie n)

intEvenSummationSerie n = [x*((2*x) + 2)/2 | x <- [1..n]]
evenNumbersSummation  n = getLast(intEvenSummationSerie n)

intEvenSquareSerie n = [x*(x+1)*(2*x+1)/6 | x <- [1..n]]
intEvenSquareSummation n = getLast(intEvenSquareSerie n)

intOddSquareSerie n = [x*(4*x^2-1)/3 | x <- [1..n]]
intOddSquareSummation n = getLast(intOddSquareSerie n)

twoAsSerie n = sum([2/(x+x^2) | x <- take n [1..]])
eulerConstantAsSerie n = 1 + sum([1 / product[1..x] | x <- take n [1..]])
