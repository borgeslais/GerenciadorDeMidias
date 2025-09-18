module Reading where

import Types
import Algorithms
import Data.Time (Year)

-- Pega uma lista até certo elemento
pararEm :: Eq a => a -> [a] -> [a]
pararEm _ [] = []
pararEm n (x:xs) = if x == n then [] else x : pararEm n xs

-- Pega uma lista depois de certo elemento
depoisDe :: Eq a => a -> [a] -> [a]
depoisDe _ [] = []
depoisDe n (x:xs) = if x == n then xs else depoisDe n xs

-- Separa a lista a cada vez que aparece certo elemento
-- e devolve uma lista com essas listas.
separarEm :: Eq a => a -> [a] -> [[a]]
separarEm _ [] = []
separarEm n (x:xs) = [pararEm n (x:xs)] ++ separarEm n (depoisDe n (x:xs))

-- Pega uma linha, divide-a nas vírgulas, pega cada parte e
-- devolve um item que as têm por campo.
lerItem :: String -> Item
lerItem xs = Item {
                  titulo = (ys !! 0),
                  autor = (ys !! 1),
                  ano = read (ys !! 2) :: Year, 
                  codigo = (ys !! 3),
                  midia = read (ys !! 4) :: TipoMidia}
   where ys = separarEm ',' xs

lerVariosItens :: [String] -> [Item]
lerVariosItens [] = []
lerVariosItens (x:xs) = lerItem x : lerVariosItens xs

lerUsuario :: String -> Usuario
lerUsuario xs = Usuario {
                        nome = (ys !! 0),
                        email = (ys !! 1),
                        matricula = (ys !! 2)}
   where ys = separarEm ',' xs

lerVariosUsuarios :: [String] -> [Usuario]
lerVariosUsuarios [] = []
lerVariosUsuarios (x:xs) = lerUsuario x : lerVariosUsuarios xs


formatarItem :: Item -> String
formatarItem it = "\n" ++ (titulo it) ++ "," ++
                  (autor it) ++ "," ++
                  (show (ano it)) ++ "," ++
                  (codigo it) ++ "," ++
                  (show (midia it))

formatarUsuario :: Usuario -> String
formatarUsuario us = (nome us) ++ "," ++
                  (email us) ++ "," ++
                  (matricula us)


mostrarPorCod :: Item -> String
mostrarPorCod it = (codigo it) ++ "_ " ++
                   "Titulo: " ++ (titulo it) ++
                   ", Autor: " ++ (autor it) ++
                   ", Ano: " ++ (show (ano it)) ++
                   ", Midia: " ++ (show (midia it))

variosPorCod :: [Item] -> String
variosPorCod listaIt = unlines (map mostrarPorCod (ordenar "CODIGO" listaIt))

mostrarPorMat :: Usuario -> String
mostrarPorMat us = (matricula us) ++ "_ " ++
                   "Nome: " ++ (nome us) ++
                   ", Email: " ++ (email us)

variosPorMat :: [Usuario] -> String
variosPorMat listaUs = unlines (map mostrarPorMat (ordenarUs "MATRICULA" listaUs))


colocarNaLista :: a -> [a] -> [a]
colocarNaLista n xs = xs ++ [n]

tirarDaLista :: Eq a => a -> [a] -> [a]
tirarDaLista _ [] = []
tirarDaLista n (x:xs) = if n == x
                         then xs
                         else [x] ++ tirarDaLista n xs 
