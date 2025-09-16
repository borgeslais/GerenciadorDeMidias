module Validation where
import Types
import Data.Time (Year)
import Data.Char (ord)

{- Provisório. Trocá-lo-ei por uma função que verifica com o ano atual.
Isso, porém, é uma operação de entrada, então a farei depois, separada das funções puras. -}
validarAno :: Year -> Validade
validarAno y
 | y >= -500 && y <= 2025 = Valido
 | otherwise = Invalido

-- Não sei será necessário quando da implementação da interface.
validarMidia :: String -> Validade
validarMidia m
 | any (== m) ["Livro", "Filme", "Jogo"] = Valido
 | otherwise = Invalido

-- Verifica se o título ou o autor não tem só espaços em branco
validarTitAut :: String -> Validade
validarTitAut s
  | (any (/= ' ') s) == False = Invalido
  | otherwise = Valido

-- Testa se a string é composta apenas por números.
testeNum :: String -> Validade
testeNum [] = Valido
testeNum (x:xs) = if any (== (ord x)) [48..57]
                    then testeNum xs
                    else Invalido

-- Verifica se o código tem mais de 4 caracteres
-- e é formado só por números.
validarCodigo :: String -> Validade
validarCodigo xs = if (length xs) < 5
                        then Invalido
                        else testeNum xs


