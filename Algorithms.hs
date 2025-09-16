module Algorithms where
import Types
import Order
import Data.Char (toUpper)

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
igualFiltro :: Item -> Item -> Bool
igualFiltro i1 i2 = all (==True) [tit, aut, an0, cod, mid, sta] 
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
        sta = if (status i1 == Nulo)
               then True else status i1 == status i2

buscar :: Item -> [Item] -> [Item]
buscar it xs = filter (igualFiltro it) xs

