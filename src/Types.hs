module Types where
import Data.Time

-- DECLARAÇÕES --

-- O Vazio serve para auxiliar na busca 
data TipoMidia = Livro | Filme | Jogo | Nenhum
  deriving (Show, Eq, Read)

data Status = Emprestado | Disponivel | Nulo
  deriving (Show, Eq)

data Validade = Valido | Invalido
  deriving (Show, Eq)

data Item = Item
  { titulo :: String
  , autor :: String
  , ano :: Year      -- Deve-se sempre colocar o ano da edição, não o de publicação da obra
  , codigo :: String
  , midia :: TipoMidia
  } deriving (Eq)
 
 -- Não sei se devo incluir um [Item] em Usuario
data Usuario = Usuario
  { nome :: String
  , matricula :: String
  , email :: String
  } deriving (Eq)

-- INSTÂNCIAS --

instance Show Item where
  show (Item t au an c Livro) = "Titulo: " ++ t ++
                                ", Autor: " ++ au ++
                                ", Ano: " ++ show an ++
                                ", Codigo: " ++ c ++
                                ", Midia: " ++ show Livro ++
                                " |-| "

  show (Item t au an c Filme) = "Titulo: " ++ t ++
                                ", Diretor: " ++ au ++
                                ", Ano: " ++ show an ++
                                ", Codigo: " ++ c ++
                                ", Midia: " ++ show Filme ++
                                " |-| "

  show (Item t au an c Jogo) = "Titulo: " ++ t ++
                                ", Criador: " ++ au ++
                                ", Ano: " ++ show an ++
                                ", Codigo: " ++ c ++
                                ", Midia: " ++ show Jogo ++
                                " |-| "

instance Show Usuario where
  show (Usuario n m e) = "Nome: " ++ n ++
                         ", Matricula: " ++ m ++
                         ", e-mail: " ++ e
                                
{- PARA TESTAR NO GHCI:

generico = Item {titulo = "", autor = "", ano = -50000, codigo = "", midia = Nenhum}
x1 = Item {titulo = "Os Lusiadas", autor = "Camoes", ano = 1572, codigo = "1001", midia = Livro}
x2 = Item {titulo = "Rimas", autor = "Camoes", ano = 1595, codigo = "1002", midia = Livro}
x3 = Item {titulo = "Rimas", autor = "José Albano", ano = 1966, codigo = "103", midia = Livro}
y1 = Item {titulo = "Festim Diabólico", autor = "Hitchcock", ano = 1948, codigo = "2001", midia = Filme}
y2 = Item {titulo = "Sangue de Heróis", autor = "John Ford", ano = 1948, codigo = "2002", midia = Filme}
y3 = Item {titulo = "Rastros de Ódio", autor = "John Ford", ano = 1956, codigo = "203", midia = Filme}
z1 = Item {titulo = "Super Mario Bros.", autor = "Myamoto", ano = 1985, codigo = "3001", midia = Jogo}
z2 = Item {titulo = "Mario Kart 8", autor = "Nintendo", ano = 2014, codigo = "3020", midia = Jogo}

lista = [x1,x2,x3,y1,y2,y3,z1,z2]

   
w1 = Usuario {nome = "Joao", matricula = "12300", email = "test1@mail.com"}
t1 = Usuario {nome = "Jose", matricula = "17743", email = "test2@mail.com"}
-}
