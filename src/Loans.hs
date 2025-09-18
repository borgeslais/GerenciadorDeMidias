module Loans where

import Types
import Algorithms
import Validation
import Data.Time
import Reading (separarEm)

-- Presume que o empréstimo já foi validado
relacaoDeUmEmprestimo :: Emprestimo -> [Usuario] -> [Item] -> String
relacaoDeUmEmprestimo emp lU lI =  "Codigo: " ++ (codigoIt emp) ++
                                   " | Titulo: " ++ (titulo it) ++
                                   " | Usuario: " ++ (nome us) ++
                                   " | Data: " ++ (show (dia emp))
  where it = head (buscarItCod (codigoIt emp) lI)
        us = head (buscarUsMat (matriculaUs emp) lU)

relacaoDeVariosEmprestimos :: [Emprestimo] -> [Usuario] -> [Item] -> String
relacaoDeVariosEmprestimos lE lU lI = unlines (map (relEmp) lE)
  where relEmp emp = relacaoDeUmEmprestimo emp lU lI

formatarEmprestimo :: Emprestimo -> String
formatarEmprestimo emp = "\n" ++ (codigoIt emp) ++ "," ++
                         (matriculaUs emp) ++ "," ++
                         (show (dia emp))

lerEmprestimo :: String -> Emprestimo
lerEmprestimo xs = Emprestimo {
                    codigoIt = (ys !! 0),
                    matriculaUs = (ys !! 1),
                    dia = read (ys !! 2) :: UTCTime} 
   where ys = separarEm ',' xs

lerVariosEmprestimos :: [String] -> [Emprestimo]
lerVariosEmprestimos [] = []
lerVariosEmprestimos (x:xs) = lerEmprestimo x : lerVariosEmprestimos xs
