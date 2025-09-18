module Interface where

import Types
import Algorithms
import Order
import Validation
import Leitura
import Loans
import FuncoesImpuras
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
               conteudoIt <- readFile "itens.csv"
               let x = lines conteudoIt
               length x `seq` return ()
               let y = filtrarItensValidos (lerVariosItens x)
               conteudoUs <- readFile "usuarios.csv"
               let w = lines conteudoUs
               length w `seq` return ()
               let z = filtrarUsuariosValidos (lerVariosUsuarios w)
               conteudoEmp <- readFile "emprestimos.csv"
               let g = lines conteudoEmp
               length g `seq` return ()
               let h = lerVariosEmprestimos g
               menuPrincipal y z h


-- MENU PRINCIPAL --
menuPrincipal :: [Item] -> [Usuario] -> [Emprestimo] -> IO ()
menuPrincipal listaIt listaUs listaEmp = do
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
        "1" -> menuCadastroItens listaIt listaUs listaEmp
        "2" -> menuCadastroUsuarios listaIt listaUs listaEmp
        "3" -> menuEmpDev listaIt listaUs listaEmp
        "4" -> menuBuscaListagem listaIt
        "5" -> menuRelatEstat listaIt
        "6" -> menuEdicao listaIt
        "7" -> menuExpImp listaIt
        "8" -> menuAuditHist listaIt
        "0" -> putStr "\nSaindo...\n"
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuPrincipal listaIt listaUs listaEmp

-- SUBMENU: Cadastro de Itens --
menuCadastroItens :: [Item] -> [Usuario] -> [Emprestimo] -> IO ()
menuCadastroItens listaIt listaUs listaEmp = do
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
                  if (validarItem it) == Valido &&
                     not (codigoJaExiste (codigo it) listaIt) &&
                     not (itemJaExiste it listaIt)
                   then do inserirItem "itens.csv" it
                           let novaListaIt = colocarNaLista it listaIt
                           putStrLn ("\n" ++ (formatarItem it))
                           menuPrincipal novaListaIt listaUs listaEmp
                   else do putStrLn "Ocorreu algo de errado com a inserção."
                           menuCadastroItens listaIt listaUs listaEmp

        "2" -> do putStr (variosPorCod listaIt)
                  putStr "Digite o codigo do item: "
                  cod <- getLine
                  if (codigoJaExiste cod listaIt)
                   then do let tirado = head (buscarItCod cod listaIt)
                           let novaListaIt = tirarDaLista tirado listaIt
                           listaItParaArq "itens.csv" novaListaIt
                           putStrLn ("\n" ++ (show novaListaIt))
                           menuPrincipal novaListaIt listaUs listaEmp
                   else do putStrLn "Ocorreu algo de errado com a remoção."
                           menuCadastroItens listaIt listaUs listaEmp
                   
        "3" -> do putStrLn (variosPorCod listaIt)
                  putStr "Pressione Enter para continuar. "
                  tecla <- getLine
                  case tecla of
                    _ -> menuPrincipal listaIt listaUs listaEmp
        "0" -> menuPrincipal listaIt listaUs listaEmp
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuCadastroItens listaIt listaUs listaEmp         
            

-- SUBMENU: Cadastro de Usuários --
menuCadastroUsuarios :: [Item] -> [Usuario] -> [Emprestimo] -> IO ()
menuCadastroUsuarios listaIt listaUs listaEmp = do
    linha
    putStrLn "\tCadastro de Usuarios"
    linha
    putStrLn "1 - Adicionar novo usuário"
    putStrLn "2 - Remover usuário"
    putStrLn "3 - Listar usuários cadastrados"
    putStrLn "0 - Voltar ao menu principal"
    putStr "Digite uma opção: "

    opcao <- getLine
    case opcao of
        "1" -> do us <- lendoUsuario
                  if (validarUsuario us) == Valido &&
                     not (matriculaJaExiste (matricula us) listaUs) &&
                     not (usuarioJaExiste us listaUs)
                   then do inserirUsuario "usuarios.csv" us
                           let novaListaUs = colocarNaLista us listaUs
                           putStrLn ("\n" ++ (formatarUsuario us))
                           menuPrincipal listaIt novaListaUs listaEmp
                   else do putStrLn "Ocorreu algo de errado com a inserção."
                           menuCadastroItens listaIt listaUs listaEmp

        "2" -> do putStrLn (variosPorMat listaUs)
                  putStr "Digite a matricula do usuario: "
                  mat <- getLine
                  if (matriculaJaExiste mat listaUs)
                   then do let tirado = head (buscarUsMat mat listaUs)
                           let novaListaUs = tirarDaLista tirado listaUs
                           listaUsParaArq "usuarios.csv" novaListaUs
                           putStrLn ("\n" ++ (show novaListaUs))
                           menuPrincipal listaIt novaListaUs listaEmp
                   else do putStrLn "Ocorreu algo de errado com a remoção."
                           menuCadastroItens listaIt listaUs listaEmp
        "3" -> do putStrLn (variosPorMat listaUs)
                  putStr "Pressione Enter para continuar. "
                  tecla <- getLine
                  case tecla of
                    _ -> menuPrincipal listaIt listaUs listaEmp
        "0" -> menuPrincipal listaIt listaUs listaEmp
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuCadastroUsuarios listaIt listaUs listaEmp


-- SUBMENU: Empréstimos e Devoluções  --
menuEmpDev:: [Item] -> [Usuario] -> [Emprestimo] -> IO ()
menuEmpDev listaIt listaUs listaEmp = do
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

    opcao <- getLine
    case opcao of
        "1" -> do emp <- lendoEmprestimo
                  inserirEmprestimo "emprestimos.csv" emp
                  let novaListaEmp = colocarNaLista emp listaEmp
                  putStrLn ("\n" ++ (formatarEmprestimo emp))
                  menuPrincipal listaIt listaUs novaListaEmp
                  
        "2" -> do putStrLn (relacaoDeVariosEmprestimos listaEmp listaUs listaIt)
                  putStr "Digite o codigo do item: "
                  codIt <- getLine
                  let tirado = head (buscarEmpCod codIt listaEmp)
                  let novaListaEmp = tirarDaLista tirado listaEmp
                  listaEmpParaArq "emprestimos.csv" novaListaEmp
                  putStrLn ("\n" ++ (show novaListaEmp))
                  menuPrincipal listaIt listaUs novaListaEmp
                  
        "3" -> do putStrLn (relacaoDeVariosEmprestimos listaEmp listaUs listaIt)
                  putStr "Pressione Enter para continuar. "
                  tecla <- getLine
                  case tecla of
                    _ -> menuPrincipal listaIt listaUs listaEmp
       {- "4" ->
        "5" -> -}
        "0" -> menuPrincipal listaIt listaUs listaEmp
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuEmpDev listaIt listaUs listaEmp
            


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
        "0" -> menuPrincipal listaIt listaUs
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
        "0" -> menuPrincipal listaIt listaUs
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
        "0" -> menuPrincipal listaIt listaUs
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
        "0" -> menuPrincipal listaIt listaUs
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
        "0" -> menuPrincipal listaIt listaUs
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuAuditHist listaIt           -}
