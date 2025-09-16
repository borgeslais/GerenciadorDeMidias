module Types where
import Data.Time

-- DECLARAÇÕES --

-- O Vazio serve para auxiliar na busca 
data TipoMidia = Livro | Filme | Jogo | Nenhum
  deriving (Show, Eq)

data Status = Emprestado | Disponivel | Nulo
  deriving (Show, Eq)

data Validade = Valido | Invalido
  deriving (Show, Eq)

data Item = Item
  { titulo :: String
  , autor :: String
  , ano :: Year
  , codigo :: String
  , midia :: TipoMidia
  , status :: Status
  } deriving (Eq)
 
 -- Não sei se devo incluir um [Item] em Usuario
data Usuario = Usuario
  { nome :: String
  , matricula :: String
  , email :: String
  } deriving (Eq)

-- INSTÂNCIAS --

instance Show Item where
  show (Item t au an c Livro s) = "Titulo: " ++ t ++
                                ", Autor: " ++ au ++
                                ", Ano: " ++ show an ++
                                ", Codigo: " ++ c ++
                                ", Midia: " ++ show Livro ++
                                ", Status: " ++ show s ++
                                " |-| "

  show (Item t au an c Filme s) = "Titulo: " ++ t ++
                                ", Diretor: " ++ au ++
                                ", Ano: " ++ show an ++
                                ", Codigo: " ++ c ++
                                ", Midia: " ++ show Filme ++
                                ", Status: " ++ show s ++
                                " |-| "

  show (Item t au an c Jogo s) = "Titulo: " ++ t ++
                                ", Criador: " ++ au ++
                                ", Ano: " ++ show an ++
                                ", Codigo: " ++ c ++
                                ", Midia: " ++ show Jogo ++
                                ", Status: " ++ show s ++
                                " |-| "

instance Show Usuario where
  show (Usuario n m e) = "Nome: " ++ n ++
                         ", Matricula: " ++ m ++
                         ", e-mail: " ++ e
                                
{- PARA TESTAR NO GHCI:

generico = Item {titulo = "", autor = "", ano = -50000, codigo = "", midia = Nenhum, status = Nulo}
x1 = Item {titulo = "Os Lusiadas", autor = "Camoes", ano = 1572, codigo = "101", midia = Livro, status = Emprestado}
x2 = Item {titulo = "Rimas", autor = "Camoes", ano = 1595, codigo = "102", midia = Livro, status = Disponivel}
x3 = Item {titulo = "Rimas", autor = "José Albano", ano = 1966, codigo = "103", midia = Livro, status = Emprestado}
y1 = Item {titulo = "Festim Diabólico", autor = "Hitchcock", ano = 1948, codigo = "201", midia = Filme, status = Emprestado}
y2 = Item {titulo = "Sangue de Heróis", autor = "John Ford", ano = 1948, codigo = "202", midia = Filme, status = Disponivel}
y3 = Item {titulo = "Rastros de Ódio", autor = "John Ford", ano = 1956, codigo = "203", midia = Filme, status = Disponivel}
z1 = Item {titulo = "Super Mario Bros.", autor = "Myamoto", ano = 1985, codigo = "301", midia = Jogo, status = Emprestado}
z2 = Item {titulo = "Mario Kart 8", autor = "Nintendo", ano = 2014, codigo = "302", midia = Jogo, status = Disponivel}

let lista = [x1,x2,x3,y1,y2,y3,z1,z2]

   
w1 = Usuario {nome = "Joao", matricula = "12300", email = "test1@mail.com"}
t1 = Usuario {nome = "Jose", matricula = "17743", email = "test2@mail.com"}
-}
