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

```

## Estrutura do Projeto

```bash
GerenciadorDeMidias/
├── app/
│   └── Main.hs          # ponto de entrada da aplicação
├── src/                 # módulos internos do projeto
├── test/               
├── GerenciadorDeMidias.cabal
├── stack.yaml
└── README.md
```


## Para compilar e executar

no terminal, vá até a pasta "src" e digite os comandos:

para compilar:

no Linux: ghc Main

no Windows: ghc Main.hs

para executar:

no Linux: ./Main

no Windows .\Main.exe
