module Validation where
import Types
import Data.Time (Year)
import Data.Char (ord)
import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import Data.List (elemIndex)
import Algorithms (buscarItCod, buscarItRepetido, buscarUsRepetido, buscarUsMat)
import Reading (separarEm)

-- Bool pra Validade
boolValidade :: Bool -> Validade
boolValidade True = Valido
boolValidade False = Invalido

validarAnoString :: String -> Validade
validarAnoString [] = Invalido
validarAnoString (x:xs)
 | x == '-' = testeNum xs
 | otherwise = testeNum (x:xs)

validarAnoYear :: Year -> TipoMidia -> Validade
validarAnoYear y m =
  case m of
   Livro -> if (y >= 1800) && (y <= 2025) then Valido else Invalido
   Filme -> if (y >= 1930) && (y <= 2025) then Valido else Invalido 
   Jogo -> if (y >= 2000) && (y <= 2025) then Valido else Invalido  -- A partir da 6a ger. (ecluído o DreamCast)

validarMidia :: String -> Validade
validarMidia m
 | any (== m) ["Livro", "Filme", "Jogo"] = Valido
 | otherwise = Invalido

-- Funções auxiliares para validar título e autor

--Limite de tamanho:
  --Mínimo: 2 caracteres válidos (sem contar espaços)
  --Máximo: 100 caracteres total

--Validação de caracteres:
  --Permite apenas letras, números e caracteres especiais apropriados
  --Inclui acentos e caracteres em português
  --Remove espaços do início e fim antes de validar

--Funções especializadas:
  --validarAutor: Mais restritiva, não permite muitos números
  --validarTitulo: Mais permissiva, permite pontuação adicional

-- Configurações de limite
limiteMinimo :: Int
limiteMinimo = 2  -- Pelo menos 2 caracteres válidos
limiteMaximo :: Int  
limiteMaximo = 100  -- Máximo 100 caracteres total

-- Remove espaços em branco do início e fim
removerEspacos :: String -> String
removerEspacos = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Verifica se todos os caracteres são válidos para nomes/títulos
todosCaracteresValidos :: String -> Bool
todosCaracteresValidos = all caracterValido

-- Caracteres válidos para títulos (mais permissivos)
todosCaracteresValidosTitulo :: String -> Bool
todosCaracteresValidosTitulo = all caracterValidoTitulo

-- Caracteres válidos especificamente para nomes de autores
caracterValidoAutor :: Char -> Bool
caracterValidoAutor c = isLetter c || c `elem` " .'-áéíóúàèìòùâêîôûãõçÁÉÍÓÚÀÈÌÒÙÂÊÎÔÛÃÕÇ"

todosCaracteresValidosAutor :: String -> Bool
todosCaracteresValidosAutor = all caracterValidoAutor

-- Verifica se há excesso de números (para autores)
temNumeroExcessivo :: String -> Bool
temNumeroExcessivo s = length (filter isDigit s) > 2

caracterValidoTitulo :: Char -> Bool
caracterValidoTitulo c = isLetter c || isDigit c || c `elem` caracteresPermitidosTitulo

caracteresPermitidosTitulo :: String
caracteresPermitidosTitulo = " .,'-():!?;\"&+áéíóúàèìòùâêîôûãõçÁÉÍÓÚÀÈÌÒÙÂÊÎÔÛÃÕÇ"

-- Define quais caracteres são válidos
caracterValido :: Char -> Bool
caracterValido c = isLetter c || isDigit c || c `elem` caracteresPermitidos

-- Caracteres especiais permitidos além de letras e números
caracteresPermitidos :: String
caracteresPermitidos = " .,'-():!?áéíóúàèìòùâêîôûãõçÁÉÍÓÚÀÈÌÒÙÂÊÎÔÛÃÕÇ"

-- Versão mais específica para autores (sem alguns símbolos)
validarAutor :: String -> Validade
validarAutor s
    | null (removerEspacos s) = Invalido
    | length s > 80 = Invalido  -- Limite menor para nomes
    | length (removerEspacos s) < 2 = Invalido
    | not (todosCaracteresValidosAutor s) = Invalido
    | temNumeroExcessivo s = Invalido  -- Autor não deve ter muitos números
    | otherwise = Valido

-- Versão específica para títulos (permite mais símbolos)
validarTitulo :: String -> Validade
validarTitulo s
    | null (removerEspacos s) = Invalido
    | length s > 200 = Invalido  -- Títulos podem ser mais longos
    | length (removerEspacos s) < 1 = Invalido
    | not (todosCaracteresValidosTitulo s) = Invalido
    | otherwise = Valido

-- Testa se a string é composta apenas por números.
testeNum :: String -> Validade
testeNum xs = if all isDigit xs
              then Valido
              else Invalido

-- Funções auxiliares para verificar se código já existe
codigoJaExiste :: String -> [Item] -> Bool
codigoJaExiste cod listaItens
 | buscarItCod cod listaItens == [] = False
 | otherwise = True

matriculaJaExiste :: String -> [Usuario] -> Bool
matriculaJaExiste mat listaUsuarios
 | buscarUsMat mat listaUsuarios == [] = False
 | otherwise = True

validCodigo :: String -> Validade
validCodigo xs
 | length xs /= 4 = Invalido
 | testeNum xs == Invalido = Invalido
 | otherwise = Valido

-- Função principal para validar o código
validarCodigo :: String -> [Item] -> Validade
validarCodigo xs listaIt
 | (validCodigo xs == Invalido) || (codigoJaExiste xs listaIt == True) = Invalido
 | otherwise = Valido


-- Fazer função de validação para o item inteiro, comparar todos os campos do item depois de ver se todos são válidos.

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


divPorPonto = separarEm '.' -- Dividir string por pontos

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

-- Função secundária para validar EMAIL
validEmail :: String -> Bool
validEmail eMail =
  case elemIndex '@' eMail of
    Nothing -> False -- Não tem @
    Just pos ->
      let (user, resto) = splitAt pos eMail
          dominio = drop 1 resto -- Remove o @
      in valUsuario user &&
         valDominio dominio &&
         valArrobas eMail == 1
         
-- Função final para validar EMAIL
validarEmail :: String -> Validade
validarEmail xs = boolValidade (validEmail xs)

----- VALIDAR ITEM inteiro
validarItem :: Item -> Validade
validarItem it = if validarTitulo (titulo it) == Valido &&
                    validarAutor (autor it) == Valido &&
                    (validarAnoYear (ano it) (midia it)) == Valido &&
                    validCodigo (codigo it) == Valido &&
                    validarMidia (show (midia it)) == Valido
                   then Valido
                   else Invalido

filtrarItensValidos :: [Item] -> [Item]
filtrarItensValidos xs = filter ((== Valido).(validarItem)) xs

itemJaExiste :: Item -> [Item] -> Bool
itemJaExiste it listaIt
 | buscarItRepetido it listaIt == [] = False
 | otherwise = True


-- VALIDAR USUARIO inteiro
validarUsuario :: Usuario -> Validade
validarUsuario us = if validarAutor (nome us) == Valido &&
                       validarEmail (email us) == Valido &&
                       validCodigo (matricula us) == Valido
                     then Valido
                     else Invalido

filtrarUsuariosValidos :: [Usuario] -> [Usuario]
filtrarUsuariosValidos xs = filter ((== Valido).(validarUsuario)) xs

usuarioJaExiste :: Usuario -> [Usuario] -> Bool
usuarioJaExiste us listaUs
 | buscarUsRepetido us listaUs == [] = False
 | otherwise = True
