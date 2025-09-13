module Order where
import Types


{-  Funções que nos permitem trabalhar com relações de ordem
distintas das pré-estabelecidas pela classe Ord.
    Serão de grande valia quando implementarmos a listagem
por categorias.                                             -}


-- menor relativo
menorR :: (a -> a-> Ordering) -> a -> a -> Bool
menorR f x y
 | (f x y) == LT = True
 | otherwise = False

-- maior relativo
maiorR :: (a -> a-> Ordering) -> a -> a -> Bool
maiorR f x y
 | (f x y) == GT = True
 | otherwise = False

-- menor igual relativo
menorIgR :: (a -> a-> Ordering) -> a -> a -> Bool
menorIgR f x y
 | (f x y) == LT = True
 | (f x y) == EQ = True
 | otherwise = False

-- maior igual relativo
maiorIgR :: (a -> a-> Ordering) -> a -> a -> Bool
maiorIgR f x y
 | (f x y) == GT = True
 | (f x y) == EQ = True
 | otherwise = False

-- Diferentes formas de ordenar itens
compareItems :: String -> Item -> Item -> Ordering
compareItems "AUTOR" i1 i2 = compare (autor i1) (autor i2)
compareItems "TITULO" i1 i2 = compare (titulo i1) (titulo i2)
compareItems "ANO" i1 i2 = compare (ano i1) (ano i2)
compareItems "CODIGO" i1 i2 = compare (codigo i1) (codigo i2)
