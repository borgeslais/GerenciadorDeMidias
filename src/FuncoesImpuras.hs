module FuncoesImpuras where

import Types
import Algorithms
import Leitura
import Order
import Validation
import Data.Time (Year)

-- Arq Item
inserirItem :: FilePath -> Item -> [Item] -> IO ()
inserirItem fp it listaIt = appendFile fp (formatarItem it)

listaItParaArq :: FilePath -> [Item] -> IO ()
listaItParaArq fp listaIt = writeFile fp (unlines (map formatarItem listaIt))

-- Arq Usuario
inserirUsuario :: FilePath -> Usuario -> [Usuario] -> IO ()
inserirUsuario fp us listaUs = appendFile fp (formatarUsuario us)

listaUsParaArq :: FilePath -> [Usuario] -> IO ()
listaUsParaArq fp listaUs = writeFile fp (unlines (map formatarUsuario listaUs))

-- Para Ler um item
lendoItem :: IO Item
lendoItem = do 
               mid' <- getMidia
               tit' <- getTitulo 
               aut' <- getAutor mid' 
               an0' <- getAno mid'
               cod' <- getCodigo
               let it = Item {titulo = tit', autor = aut', ano = an0', codigo = cod', midia = mid'}
               return (it)
             

getMidia :: IO TipoMidia
getMidia = do putStr "\nDigite o tipo de mídia: "
              mid <- getLine
              if validarMidia mid == Invalido
                then do putStrLn "Campo Inválido! Tente novamente!"
                        getMidia
                else do let mid1 = read mid :: TipoMidia
                        return (mid1)

getTitulo :: IO String
getTitulo = do putStr "Digite o titulo: "
               tit <- getLine
               if validarTitulo tit == Invalido
                then do putStrLn "Campo Inválido! Tente novamente!"
                        getTitulo
                else return (tit)

getAutor :: TipoMidia -> IO String
getAutor m = do
  case m of
   Livro -> putStr "Digite o autor: "
   Filme -> putStr "Digite o diretor: "
   Jogo -> putStr "Digite o criador: "
  aut <- getLine
  if validarAutor aut == Invalido
    then do putStrLn "Campo Inválido! Tente novamente!"
            getAutor m
    else return (aut)

getAno :: TipoMidia -> IO Year
getAno m = do putStr "Digite o ano: "
              an0 <- getLine
              if validarAnoString an0 == Invalido
                then do putStrLn "Campo Inválido! Tente novamente!"
                        getAno m
                else do let an0year = read an0 :: Year
                        if (validarAnoYear an0year m) == Invalido
                         then do putStrLn "Campo Inválido! Tente novamente!"
                                 getAno m
                         else return (an0year)

getCodigo :: IO String
getCodigo = do putStr "Digite o codigo: "
               cod <- getLine
               if validCodigo cod == Invalido
                 then do putStrLn "Campo Inválido! Tente novamente!"
                         getCodigo
                 else return (cod)

-- Para ler um usuário
lendoUsuario :: IO Usuario
lendoUsuario = do 
               nom' <- getNome
               em' <- getEmail 
               mat' <- getMatricula
               let us = Usuario {nome = nom', email = em', matricula = mat'}
               return (us)

getNome :: IO String
getNome = do putStr "Digite o nome: "
             nom <- getLine
             if validarAutor nom == Invalido
              then do putStrLn "Campo Inválido! Tente novamente!"
                      getNome
              else return (nom)

getEmail :: IO String
getEmail = do putStr "Digite o email: "
              em <- getEmail
              if validarEmail em == Invalido
                then do putStrLn "Campo Inválido! Tente novamente!"
                        getEmail
                else return (em)

getMatricula :: IO String
getMatricula = do putStr "Digite a matricula: "
                  mat <- getLine
                  if validCodigo mat == Invalido
                   then do putStrLn "Campo Inválido! Tente novamente!"
                           getMatricula
                   else return (mat)
