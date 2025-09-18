module Algorithms where
import Types
import Order
import Data.Char (toUpper)
import Data.Time.Calendar
import Data.Time

-- QuickSort compatível com as relações de ordenação inusuais permitidas por Order.hs
quickSort :: (a -> a -> Ordering) -> [a] -> [a]
quickSort f [] = []
quickSort f (x:xs) = quickSort f (filter (`mIg` x) xs) ++ [x] ++ quickSort f (filter (`mr` x) xs)
  where mIg = menorIgR f
        mr = maiorR f

-- Ordena uma lista de Itens de acordo com a categoria escolhida
ordenar :: String -> [Item] -> [Item]
ordenar str xs = 
 let comp = compareItem str
 in quickSort comp xs     

ordenarUs :: String -> [Usuario] -> [Usuario]
ordenarUs str xs = 
 let comp = compareUsuario str
 in quickSort comp xs 

{- Fiz essa função para uma ideia que depois abandonei
   Deixei-a aí só em caso de vir a calhar em algum momento -}
encontrar :: Eq a => a -> [a] -> Maybe a
encontrar _ [] = Nothing
encontrar n (x:xs) = if n == x then Just n else encontrar n xs

-- Verifica se uma string contém outra. Desconsidera maiúsculas e minúsculas. 
strContemstr :: String -> String -> Bool
strContemstr _ [] = False
strContemstr contida continente = if map toUpper contida == map toUpper parte
                                  then True else strContemstr contida (drop 1 continente)
  where parte = take (length contida) continente


{- Compara um item ao outro apenas em alguns campos.
   Quando não se quer comparar um campo, coloca-se 
   nele o "valor genérico" correspondente. Assim, podemos
   realizar buscas exatas e também buscas com apenas
   alguns campos em uma mesma função busca.               -}
igualFiltroIt :: Item -> Item -> Bool
igualFiltroIt i1 i2 = all (==True) [tit, aut, an0, cod, mid] 
  where tit = if (titulo i1 == "") 
               then True else strContemstr (titulo i1) (titulo i2)
        aut = if (autor i1 == "")
               then True else strContemstr (autor i1) (autor i2)
        an0 = if (ano i1 == -50000) -- Chamei de an0 pra nao confundir
               then True else ano i1 == ano i2
        cod = if (codigo i1 == "") 
               then True else codigo i1 == codigo i2
        mid = if (midia i1 == Nenhum)
               then True else midia i1 == midia i2

buscarIt :: Item -> [Item] -> [Item]
buscarIt it xs = filter (igualFiltroIt it) xs

buscarItCod :: String -> [Item] -> [Item]
buscarItCod cod = buscarIt it
  where it = generico {codigo = cod}
        generico = Item {titulo = "", autor = "", ano = -50000, codigo = "", midia = Nenhum}

buscarItRepetido :: Item -> [Item] -> [Item]
buscarItRepetido it = buscarIt it'
  where it' = it {codigo = ""}

-- Para o Usuário
igualFiltroUs :: Usuario -> Usuario -> Bool
igualFiltroUs u1 u2 = all (==True) [nom, ema, mat] 
  where nom = if (nome u1 == "") 
               then True else (nome u1) == (nome u2)
        ema = if (email u1 == "")
               then True else (email u1) == (email u2)
        mat = if (matricula u1 == "") 
               then True else matricula u1 == matricula u2

buscarUs :: Usuario -> [Usuario] -> [Usuario]
buscarUs us xs = filter (igualFiltroUs us) xs

buscarUsMat :: String -> [Usuario] -> [Usuario]
buscarUsMat mat = buscarUs us
  where us = generico {matricula = mat}
        generico = Usuario {nome = "", email = "", matricula = ""}

buscarUsRepetido :: Usuario -> [Usuario] -> [Usuario]
buscarUsRepetido us = buscarUs us'
  where us' = us {matricula = ""}


-- Para Emprestimos
-- data para UTC
dataGenerica :: UTCTime
dataGenerica = UTCTime (fromGregorian 2000 01 01) (secondsToDiffTime 0)

igualFiltroEmp :: Emprestimo -> Emprestimo -> Bool
igualFiltroEmp emp1 emp2 = all (==True) [cod, mat, dd] 
  where cod = if (codigoIt emp1 == "") 
               then True else (codigoIt emp1) == (codigoIt emp2)
        mat = if (matriculaUs emp1 == "") 
               then True else matriculaUs emp1 == matriculaUs emp2
        dd = if (dia emp1 == dataGenerica) 
               then True else dia emp1 == dia emp2

buscarEmp :: Emprestimo -> [Emprestimo] -> [Emprestimo]
buscarEmp emp xs = filter (igualFiltroEmp emp) xs

buscarEmpCod:: String -> [Emprestimo] -> [Emprestimo]
buscarEmpCod cod = buscarEmp emp
  where emp = generico {codigoIt = cod}
        generico = Emprestimo {codigoIt = "", matriculaUs = "", dia = dataGenerica}
