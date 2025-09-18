# GerenciadorDeMidias

Projeto em Haskell para gerenciamento de mídias.

## Pré-requisitos

Antes de começar, instale:

- [GHC](https://www.haskell.org/ghc/) (compilador Haskell)
- [Cabal](https://www.haskell.org/cabal/) (ferramenta de build)
- [Stack](https://haskellstack.org) (opcional, mas recomendado para builds reproduzíveis)

Verifique as versões instaladas:

```bash
ghc --version
cabal --version
stack --version

````

## Estrutura do Projeto

GerenciadorDeMidias/
├── app/
│   └── Main.hs          # ponto de entrada da aplicação
├── src/                 # módulos internos do projeto
├── test/               
├── GerenciadorDeMidias.cabal
├── stack.yaml
└── README.md


## Para compilar e executar
usando stack (recomendado)
stack setup     # prepara o compilador (apenas na primeira vez)
stack build     # compila
stack run       # executa

ou usando cabal
cabal v2-build          # compila
cabal v2-run GerenciadorDeMidias # executa
