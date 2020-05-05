# MatematicaDiscreta-TabelaVerdade-Haskell

## Programa para visualizaço de tabela verdade em Haskell.

### A fórmula é colocada em TabelaVerdade.hs na linha 182, como o exemplo a seguir:
  "let form = (Ou (E (Var "A") (Var "B")) (Implic (Var "B") (Var "D")))"
### As letras devem ser colocadas como Verdadeiro ou Falso para a função de avaliar a fórmula (opção 1), editado em TabelaVerdade.hs na linha 183, como o exemplo a seguir:
    let context =  [("A",True),("B",False),("C",True),("D",False)]
    
O programa contém as seguintes funções:

  1. Avaliar fórmula
  2. Criar tabela verdade
  3. Criar arquivo HTML com a tabela verdade
  4. Criar arquivo HTML(Bootstrap) com a tabela verdade
  5. Traduzir fórmula para modo legível
