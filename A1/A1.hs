Atividade = "1"

-- 1
-- Recebe uma string e 
-- Retorna-a sem as vogais.   
noVog :: String -> String
noVog s = filter (`notElem` "aeiouAEIOU") s

-- 2
-- Retorna quantas vezes x é divisível por n
num'divs :: Int -> Int -> Int
num'divs x n = length $ takeWhile (\y -> x `mod` (n^y) == 0) [1..]

-- 3
-- Dado um inteiro n. determinar se
-- Ele é ou não um número prime
is'prime :: Int -> Bool
is'prime n
    | n < 2 = False
    | otherwise = null [ x | x <- [2..isqrt n], n `mod` x == 0]
    where
        isqrt = floor . sqrt . fromIntegral

-- 4
-- Inverte um inteiro, por exemplo
-- O inverso de 251 é 152.
int'inv :: Int -> Int
int'inv x = read (reverse (show x)) :: Int