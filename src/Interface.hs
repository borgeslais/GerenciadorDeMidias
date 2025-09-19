module Interface where

import Types
import Algorithms
import Order
import Validation
import Reading
import Loans
import ImpureFunctions
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
    putStrLn "5 - Edição de Dados"
    putStrLn "0 - Salvar e Sair"
    putStr "Digite uma opção: "
    
    opcao <- getLine
    case opcao of
        "1" -> menuCadastroItens listaIt listaUs listaEmp
        "2" -> menuCadastroUsuarios listaIt listaUs listaEmp
        "3" -> menuEmpDev listaIt listaUs listaEmp
        "4" -> menuBuscaListagem listaIt listaUs listaEmp
        "5" -> menuEdicao listaIt listaUs listaEmp
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
                   then do let novaListaIt = colocarNaLista it listaIt
                           listaItParaArq "itens.csv" novaListaIt
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
                   then do let novaListaUs = colocarNaLista us listaUs
                           listaUsParaArq "usuarios.csv" novaListaUs
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
    putStrLn "0 - Voltar ao menu principal"
    putStr "Digite uma opção: "

    opcao <- getLine
    case opcao of
        "1" -> do emp <- lendoEmprestimo
                  if (codigoJaExiste (codigoIt emp) listaIt) &&
                     (matriculaJaExiste (matriculaUs emp) listaUs) &&
                     (buscarEmpCod (codigoIt emp) listaEmp) == []
                   then do let novaListaEmp = colocarNaLista emp listaEmp
                           listaEmpParaArq "emprestimos.csv" novaListaEmp
                           putStrLn ("\n" ++ (formatarEmprestimo emp))
                           menuPrincipal listaIt listaUs novaListaEmp
                   else do putStrLn "Codigo ou matricula problematico(s)."
                           menuPrincipal listaIt listaUs listaEmp
                  
        "2" -> do putStrLn (relacaoDeVariosEmprestimos listaEmp listaUs listaIt)
                  codIt <- getCodigo
                  if (buscarEmpCod codIt listaEmp) /= []
                    then do let tirado = head (buscarEmpCod codIt listaEmp)
                            let novaListaEmp = tirarDaLista tirado listaEmp
                            listaEmpParaArq "emprestimos.csv" novaListaEmp
                            putStrLn ("\n" ++ (show novaListaEmp))
                            menuPrincipal listaIt listaUs novaListaEmp
                    else do putStrLn "Codigo inexistente."
                            menuPrincipal listaIt listaUs listaEmp
                  
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
menuBuscaListagem :: [Item] -> [Usuario] -> [Emprestimo] -> IO ()
menuBuscaListagem listaIt listaUs listaEmp = do
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

    opcao <- getLine
    case opcao of
        "1" -> do let generico = Item {titulo = "", autor = "", ano = -50000, codigo = "", midia = Nenhum}
                  tit <- getTitulo
                  let it = generico {titulo = tit}
                  let listaBusca = buscarIt it listaIt
                  putStrLn (concat (map exibirItem listaBusca))
                  menuPrincipal listaIt listaUs listaEmp
                  
        "2" -> do let generico = Item {titulo = "", autor = "", ano = -50000, codigo = "", midia = Nenhum}
                  aut <- getAutor Livro
                  let it = generico {autor = aut}
                  let listaBusca = buscarIt it listaIt
                  putStrLn (concat (map exibirItem listaBusca))
                  menuPrincipal listaIt listaUs listaEmp
                  
        "3" -> do tit <- talvezTit
                  aut <- talvezAut
                  an0 <- talvezAno
                  mid <- talvezMidia
                  let it = Item {titulo = tit, autor = aut, ano = an0, codigo = "", midia = mid}
                  let listaBusca = buscarIt it listaIt
                  putStrLn (concat (map exibirItem listaBusca))
                  menuPrincipal listaIt listaUs listaEmp
                  
        "4" -> do putStrLn "Filtrar por: "
                  putStrLn "1 - Livro"
                  putStrLn "2 - Filme"
                  putStrLn "3 - Jogo"
                  putStrLn "0 - Voltar para o menu de Busca"
                  putStr "Digite uma opção: "
                  campo <- getLine
                  let generico = Item {titulo = "", autor = "", ano = -50000, codigo = "", midia = Nenhum}
                  case campo of
                    "1" -> do let it = generico {midia = Livro}
                              let listaBusca = buscarIt it listaIt
                              putStrLn (concat (map exibirItem listaBusca))
                              menuPrincipal listaIt listaUs listaEmp
                    "2" -> do let it = generico {midia = Filme}
                              let listaBusca = buscarIt it listaIt
                              putStrLn (concat (map exibirItem listaBusca))
                              menuPrincipal listaIt listaUs listaEmp
                    "3" -> do let it = generico {midia = Jogo}
                              let listaBusca = buscarIt it listaIt
                              putStrLn (concat (map exibirItem listaBusca))
                              menuPrincipal listaIt listaUs listaEmp
                    "0" -> menuBuscaListagem listaIt listaUs listaEmp
                    _ -> do putStrLn "Campo Inválido! Tente novamente!"
                            menuBuscaListagem listaIt listaUs listaEmp

        "5" -> do putStrLn "Ordenar por: "
                  putStrLn "1 - Titulo"
                  putStrLn "2 - Ano"
                  putStrLn "3 - Autor"
                  putStrLn "0 - Voltar para o menu de Busca"
                  putStr "Digite uma opção: "
                  op <- getLine
                  case op of
                    "1" -> do let listaOrdenada = ordenar "TITULO" listaIt
                              putStrLn (concat (map exibirItem listaOrdenada))
                              menuPrincipal listaIt listaUs listaEmp
                    "2" -> do let listaOrdenada = ordenar "ANO" listaIt
                              putStrLn (concat (map exibirItem listaOrdenada))
                              menuPrincipal listaIt listaUs listaEmp
                    "3" -> do let listaOrdenada = ordenar "AUTOR" listaIt
                              putStrLn (concat (map exibirItem listaOrdenada))
                              menuPrincipal listaIt listaUs listaEmp
                    "0" -> menuBuscaListagem listaIt listaUs listaEmp
                    _ -> do putStrLn "Campo Inválido! Tente novamente!"
                            menuBuscaListagem listaIt listaUs listaEmp

        "0" -> menuPrincipal listaIt listaUs listaEmp
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuBuscaListagem listaIt listaUs listaEmp 
            

-- SUBMENU: Edição de Dados --
menuEdicao :: [Item] -> [Usuario] -> [Emprestimo] -> IO ()
menuEdicao listaIt listaUs listaEmp = do
    linha
    putStrLn "\tEdição de Dados"
    linha
    putStrLn "1 - Editar item"
    putStrLn "2 - Editar usuário"
    putStrLn "0 - Voltar ao menu principal"
    putStr "Digite uma opção: "

    opcao <- getLine
    case opcao of
        "1" -> do putStrLn "Do item que deseja editar: "
                  cod <- getCodigo
                  if codigoJaExiste cod listaIt
                   then do let it = head (buscarItCod cod listaIt)
                           putStrLn (show it)
                           putStrLn "Qual campo deseja substituir?"
                           putStrLn "1 - Titulo"
                           putStrLn "2 - Autor"
                           putStrLn "3 - Ano"
                           putStr "Opcao: "
                           op <- getLine
                           case op of
                              "1" -> do tit <- getTitulo
                                        let novoIt = it {titulo = tit}
                                        let novaListaIt = tirarDaLista it listaIt
                                        let novaListaIt' = colocarNaLista novoIt novaListaIt
                                        listaItParaArq "itens.csv" novaListaIt'
                                        menuPrincipal novaListaIt' listaUs listaEmp
                              "2" -> do aut <- getAutor (midia it)
                                        let novoIt = it {autor = aut}
                                        let novaListaIt = tirarDaLista it listaIt
                                        let novaListaIt' = colocarNaLista novoIt novaListaIt
                                        listaItParaArq "itens.csv" novaListaIt'
                                        menuPrincipal novaListaIt' listaUs listaEmp
                              "3" -> do an0 <- getAno (midia it)
                                        let novoIt = it {ano = an0}
                                        let novaListaIt = tirarDaLista it listaIt
                                        let novaListaIt' = colocarNaLista novoIt novaListaIt
                                        listaItParaArq "itens.csv" novaListaIt'
                                        menuPrincipal novaListaIt' listaUs listaEmp
                              _ -> do putStrLn "Codigo invalido! Tente novamente"
                                      menuEdicao listaIt listaUs listaEmp
                   else do putStrLn "Resposta invalida! Tente novamente"
                           menuEdicao listaIt listaUs listaEmp

        "2" -> do putStrLn "Do Usuario que deseja editar: "
                  mat <- getMatricula
                  if matriculaJaExiste mat listaUs
                   then do let us = head (buscarUsMat mat listaUs)
                           putStrLn (show us)
                           putStrLn "Qual campo deseja substituir?"
                           putStrLn "1 - Nome"
                           putStrLn "2 - Email"
                           putStr "Opcao: "
                           op <- getLine
                           case op of
                              "1" -> do nom <- getNome
                                        let novoUs = us {nome = nom}
                                        let novaListaUs = tirarDaLista us listaUs
                                        let novaListaUs' = colocarNaLista novoUs novaListaUs
                                        listaUsParaArq "usuarios.csv" novaListaUs'
                                        menuPrincipal listaIt novaListaUs' listaEmp
                              "2" -> do ema <- getEmail
                                        let novoUs = us {email = ema}
                                        let novaListaUs = tirarDaLista us listaUs
                                        let novaListaUs' = colocarNaLista novoUs novaListaUs
                                        listaUsParaArq "usuarios.csv" novaListaUs'
                                        menuPrincipal listaIt novaListaUs' listaEmp
                              _ -> do putStrLn "Resposta invalida! Tente novamente"
                                      menuEdicao listaIt listaUs listaEmp
                   else do putStrLn "Codigo invalido! Tente novamente"
                           menuEdicao listaIt listaUs listaEmp

        "0" -> menuPrincipal listaIt listaUs listaEmp
        _   -> do
            putStrLn "\nOpção inválida!\n"
            menuEdicao listaIt listaUs listaEmp
