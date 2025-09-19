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
 
data Usuario = Usuario
  { nome :: String
  , matricula :: String
  , email :: String
  } deriving (Eq)

data Emprestimo = Emprestimo
  { codigoIt :: String
  , matriculaUs :: String
  , dia :: UTCTime
  } deriving (Show, Eq)

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
