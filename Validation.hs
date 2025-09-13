module Validation where
import Types
import Data.Time

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
