module Interface where

import Types
import Algorithms
import Order
import Validation
import Leitura
import Data.Time (Year)
import System.IO (stdout, hSetBuffering, BufferMode (NoBuffering))

linha :: IO ()
linha = do 
   putStrLn "========================================"

prompt :: Read a => String -> IO a
prompt msg = do putStr msg
                readLn


interface :: IO ()
interface = do hSetBuffering stdout NoBuffering
               conteudo <- readFile "itens.csv"
               let x = lines conteudo
               length x `seq` return ()
               let y = filtrarItensValidos (lerVariosItens x)
               menuPrincipal y


-- MENU PRINCIPAL --
menuPrincipal :: [Item] -> IO ()
menuPrincipal listaIt = do
    linha
    putStrLn "  Sistema de Mı́dias - Menu Principal"
    linha
    putStrLn "1 - Cadastro de Itens"
    putStrLn "2 - Cadastro de Usuários"
    putStrLn "3 - Empréstimos e Devoluções"
    putStrLn "4 - Busca e Listagem Avançada"
    putStrLn "5 - Relatórios e Estatı́sticas"
    putStrLn "6 - Edição de Dados"
    putStrLn "7 - Exportação/Importação de Dados"
    putStrLn "8 - Auditoria e Histórico"
    putStrLn "0 - Salvar e Sair"
    putStr "Digite uma opção: "
    
    opcao <- getLine
    case opcao of
        "1" -> menuCadastroItens listaIt
        "2" -> menuCadastroUsuarios listaIt
        "3" -> menuEmpDev listaIt
        "4" -> menuBuscaListagem listaIt
        "5" -> menuRelatEstat listaIt
        "6" -> menuEdicao listaIt
        "7" -> menuExpImp listaIt
        "8" -> menuAuditHist listaIt
    -- Nao implementei o 0 ainda
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuPrincipal listaIt 

-- SUBMENU: Cadastro de Itens --
menuCadastroItens :: [Item] -> IO ()
menuCadastroItens listaIt = do
    linha
    putStrLn "\tCadastro de Itens"
    linha
    putStrLn "1 - Adicionar novo item"
    putStrLn "2 - Remover item"
    putStrLn "3 - Listar itens cadastrados"
    putStrLn "0 - Voltar ao menu principal"
    putStr "Digite uma opção: "

    opcao <- getLine
    case opcao of
        "1" -> do it <- lendoItem
                  inserirItem "itens.csv" it listaIt
                  putStr (formatarItem it)
     --   "2" -> 
     --   "3" -> 
        "0" -> menuPrincipal listaIt
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuCadastroItens listaIt           
            

-- SUBMENU: Cadastro de Usuários --
menuCadastroUsuarios :: [Item] -> IO ()
menuCadastroUsuarios listaIt = do
    linha
    putStrLn "\tCadastro de Usuarios"
    linha
    putStrLn "1 - Adicionar novo usuário"
    putStrLn "2 - Remover usuário"
    putStrLn "3 - Listar usuários cadastrados"
    putStrLn "0 - Voltar ao menu principal"
    putStr "Digite uma opção: "
{-
    opcao <- getLine
    case opcao of
        "1" -> 
        "2" -> 
        "3" -> 
        "0" -> menuPrincipal listaIt
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuCadastroUsuarios listaIt           -}



-- SUBMENU: Empréstimos e Devoluções  --
menuEmpDev:: [Item] -> IO ()
menuEmpDev listaIt = do
    linha
    putStrLn "\tEmpréstimos e Devoluções"
    linha
    putStrLn "1 - Registrar empréstimo"
    putStrLn "2 - Registrar devolução"
    putStrLn "3 - Visualizar empréstimos ativos"
    putStrLn "4 - Renovar empréstimo"
    putStrLn "5 - Empréstimo/devolução em lote"
    putStrLn "0 - Voltar ao menu principal"
    putStr "Digite uma opção: "
{-
    opcao <- getLine
    case opcao of
        "1" -> 
        "2" -> 
        "3" -> 
        "4" ->
        "5" ->
        "0" -> menuPrincipal listaIt
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuEmpDev listaIt           -}
            


-- SUBMENU: Busca e Listagem Avançada --
menuBuscaListagem :: [Item] -> IO ()
menuBuscaListagem listaIt = do
    linha
    putStrLn "\tBusca e Listagem Avançada"
    linha
    putStrLn "1 - Buscar por tı́tulo"
    putStrLn "2 - Buscar por autor/diretor"
    putStrLn "3 - Busca combinada (múltiplos campos)"
    putStrLn "4 - Filtrar por categoria"
    putStrLn "5 - Ordenar resultados (tı́tulo, ano, autor/diretor)"
    putStrLn "0 - Voltar ao menu principal"
    putStr "Digite uma opção: "
{-
    opcao <- getLine
    case opcao of
        "1" -> 
        "2" -> 
        "3" -> 
        "4" ->
        "5" ->
        "0" -> menuPrincipal listaIt
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuBuscaListagem listaIt           -}
            
            

-- SUBMENU: Relatórios e Estatı́sticas --
menuRelatEstat :: [Item] -> IO ()
menuRelatEstat listaIt = do
    linha
    putStrLn "\tRelatórios e Estatı́sticas"
    linha
    putStrLn "1 - Empréstimos ativos (por categoria)"
    putStrLn "2 - Usuários mais ativos"
    putStrLn "3 - Itens mais emprestados"
    putStrLn "4 - Frequência de empréstimos por perı́odo"
    putStrLn "5 - Itens com lista de espera"
    putStrLn "6 - Relatório de operações (por usuário/tipo de item)"
    putStrLn "0 - Voltar ao menu principal"
    putStr "Digite uma opção: "
{-
    opcao <- getLine
    case opcao of
        "1" -> 
        "2" -> 
        "3" -> 
        "4" ->
        "5" ->
        "6" ->
        "0" -> menuPrincipal listaIt
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuRelatEstat listaIt           -}



-- SUBMENU: Edição de Dados --
menuEdicao :: [Item] -> IO ()
menuEdicao listaIt = do
    linha
    putStrLn "\tEdição de Dados"
    linha
    putStrLn "1 - Editar item"
    putStrLn "2 - Editar usuário"
    putStrLn "0 - Voltar ao menu principal"
    putStr "Digite uma opção: "
{-
    opcao <- getLine
    case opcao of
        "1" -> 
        "2" -> 
        "0" -> menuPrincipal listaIt
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuEdicao listaIt           -}



-- SUBMENU: Exportação/Importação --
menuExpImp :: [Item] -> IO ()
menuExpImp listaIt = do
    linha
    putStrLn "\tExportação/Importação"
    linha
    putStrLn "1 - Exportar dados para CSV"
    putStrLn "2 - Importar dados de CSV"
    putStrLn "0 - Voltar ao menu principal"
    putStr "Digite uma opção: "
{-
    opcao <- getLine
    case opcao of
        "1" -> 
        "2" -> 
        "0" -> menuPrincipal listaIt
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuExpImp listaIt           -}


-- SUBMENU: Auditoria e Histórico --
menuAuditHist :: [Item] -> IO ()
menuAuditHist listaIt = do
    linha
    putStrLn "\tAuditoria e Histórico"
    linha
    putStrLn "1 - Exibir log de operações"
    putStrLn "2 - Exibir histórico de alterações"
    putStrLn "0 - Voltar ao menu principal"
    putStr "Digite uma opção: "
{-
    opcao <- getLine
    case opcao of
        "1" -> 
        "2" -> 
        "0" -> menuPrincipal listaIt
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuAuditHist listaIt           -}

------------------------------------------------

-- FUNÇÕES QUE LIDAM COM ARQUIVOS --

inserirItem :: FilePath -> Item -> [Item] -> IO ()
inserirItem fp it listaIt = if (validarItem it) == Valido &&
                               not (codigoJaExiste cod listaIt)
                             then appendFile fp (formatarItem it)
                             else appendFile fp ""
    where cod = codigo it

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
