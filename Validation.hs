module Validation where
import Types
import Data.Time (Year)
import Data.Char (ord)
import Data.Char (isAlphaNum, isDigit, isLetter)
import Data.List (elemIndex)

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

-- Funções auxiliares para validar EMAIL -- Avaliação seguindo padrões RFC básicos

--Presença obrigatória do @: Verifica se há exatamente um símbolo @
--Validação do usuário (parte antes do @):
  --Comprimento entre 1-64 caracteres
  --Caracteres válidos: letras, números, ponto, hífen, underscore
  --Não pode começar ou terminar com ponto
  --Não pode ter pontos consecutivos

--Validação do domínio (parte depois do @):
  --Comprimento entre 3-255 caracteres
  --Deve conter pelo menos um ponto
  --Cada parte separada por ponto deve ter 1-63 caracteres
  --Não pode começar ou terminar com hífen ou ponto
  --TLD (última parte) deve ter pelo menos 2 caracteres e apenas letras

valArrobas :: String -> Int -- Verifica se há exatamente um @
valArrobas = length.filter (== '@')

valCaracteresValidosUsuario :: String -> Bool -- Caracteres válidos para usuário
valCaracteresValidosUsuario = all caracterValidoUsuario
  where
    caracterValidoUsuario c = isAlphaNum c || c `elem` "._-"

valCaracterValidoDominio :: Char -> Bool -- Caracteres válidos para o domínio
valCaracterValidoDominio c = isAlphaNum c || c == '-'

valPontosConsecutivos :: String -> Bool -- Verifica se há pontos consecutivos
valPontosConsecutivos [] = False
valPontosConsecutivos [_] = False
valPontosConsecutivos (x:y:xs) = (x == '.' && y == '.') || valPontosConsecutivos (y:xs)

valUsuario :: String -> Bool -- Validar parte do usuário (antes do @)
valUsuario [] = False
valUsuario user =
  length user >= 1 &&
  length user <= 64 &&
  valCaracteresValidosUsuario user &&
  not (head user == '.') &&
  not (last user == '.') &&
  not (valPontosConsecutivos user)

divPorPonto :: String -> [String] -- Dividir string por pontos
divPorPonto [] = [""]
divPorPonto (x:xs)
  | x == '.' = "" : divPorPonto xs
  | otherwise = case divPorPonto xs of
                  (y:ys) -> (x:y):ys
                  [] -> [[x]]


valParteIndividual :: String -> Bool -- Validar uma parte individual do domínio
valParteIndividual [] = False
valParteIndividual parte =
  length parte >= 1 &&
  length parte <= 63 &&
  all valCaracterValidoDominio parte &&
  not (head parte == '-') &&
  not (last parte == '-')

valPartesDominio :: [String] -> Bool -- Validar cada parte do domínio
valPartesDominio [] = False
valPartesDominio partes =
  all valParteIndividual partes &&
  length (last partes) >= 2 && -- TLD deve ter pelo menos 2 caracteres
  all isLetter (last partes)   -- TLD deve conter apenas letras


valDominio :: String -> Bool -- Validar dominio (depois do @)
valDominio [] = False
valDominio dominio =
  case elemIndex '.' dominio of
    Nothing -> False -- Deve ter pelo menos um ponto
    Just _ ->
      length dominio >= 3 &&
      length dominio <= 255 &&
      valPartesDominio (divPorPonto dominio) &&
      not (head dominio == '.') &&
      not (last dominio == '.') &&
      not (head dominio == '-') &&
      not (last dominio == '.') &&
      not (valPontosConsecutivos dominio)

-- Função principal para validar EMAIL
validarEmail :: String -> Bool
validarEmail eMail =
  case elemIndex '@' eMail of
    Nothing -> False -- Não tem @
    Just pos ->
      let (user, resto) = splitAt pos eMail
          dominio = drop 1 resto -- Remove o @
      in valUsuario user &&
         valDominio dominio &&
         valArrobas eMail == 1
