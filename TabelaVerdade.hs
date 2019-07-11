-- Gabriel Augusto Requena dos Reis - 16.2.8105
-- Bruno César Cota Conceição - 13.2.8514
import System.IO
import Data.List

data Formula = Lit Bool 
        | Var String 
        | E Formula Formula 
        | Ou Formula Formula 
        | Nao Formula 
        | Implic Formula Formula 
        | Bicond Formula Formula
        deriving( Show , Eq )
type Contexto = [(String, Bool)]
type TabelaVerdade = [(Contexto, Bool)]

-- Princpipais funções
-- avalia (Avaia uma formula dado um contexto)
-- truthTable (Retorna uma tabela verdade a partir de uma fórmula) -- Não Formatada
-- imprimiTabela (Imprime tabela formatada no console)
-- tautologia (retorna se formula é tautologia)
-- contradicao (retorna de sórmula é contradição)
-- traduzForm (Exibe fórmula de modo legível)
-- html (Gera arquivo .html na pasta onde o código fonte foi executado, caso seja executado 
--      diretamente pelo ghci, estará na pasta padrão do mesmo. )
-- main (seleciona menu de opções) (entrada da fórmula via console não disponível)

-- -- -- -- -- -- -- -- -- -- -- -- -- --Avalia a Fórmula-- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Defina a funcao avaliar que dado uma formula e um contexto
-- diz qual o resultado da formula nesse contexto
-- exemplo: avalia [("A",True),("B",False)] (E (Var "A") (Var "B"))
avalia :: Contexto -> Formula -> Bool
avalia contex (Var str) = recContexto contex str
avalia contex (Lit f) = f
avalia contex (Nao f) = not(avalia contex f)
avalia contex (E f1 f2) = (avalia contex f1) && (avalia contex f2)
avalia contex (Ou f1 f2) = (avalia contex f1) || (avalia contex f2)
avalia contex (Implic f1 f2) = (avalia contex (Nao f1)) || (avalia contex f2)
avalia contex (Bicond f1 f2) = (avalia contex (Implic f1 f2)) && (avalia contex (Implic f2 f1))

--Procura a váriavel no contexto e retorna seu valor
recContexto :: Contexto -> String -> Bool
recContexto ((x,bl):xs) str 
    |x == str = bl
    |otherwise = recContexto xs str

-- -- -- -- -- -- -- -- -- -- -- -- --Cria a tabela verdade-- -- -- -- -- -- -- -- -- -- -- -- --
--Para cada linha retorna o contexto e o resultado da avalia
-- Contex são todas as possibilidades sem as duplicatas das variáveis usadas na formula
truthTable :: Formula -> TabelaVerdade
truthTable form = [(context,(avalia context form)) | context<-possibilidades(nub(listaVars form))]

-- Retorna todos os possíveis contextos
possibilidades :: [String] -> [Contexto]
possibilidades []     = [[]]
possibilidades (x:xs) = [ (x,bl):v | v <- possibilidades xs, bl <- [True,False]]

-- Retorna a lista de variáveis utilizadas (COM REPETIÇÃO)
listaVars :: Formula -> [String]
listaVars (Lit _) = []
listaVars (Var f) = [f]
listaVars (Nao f) = listaVars f
listaVars (E f0 f1) = listaVars f0 ++ listaVars f1
listaVars (Ou f0 f1) = listaVars f0 ++ listaVars f1
listaVars (Implic f0 f1) = listaVars f0 ++ listaVars f1
listaVars (Bicond f0 f1) = listaVars f0 ++ listaVars f1

-- -- -- -- -- -- -- -- -- -- -Retorna se formula e uma tautologia- -- -- -- -- -- -- -- -- -- --
tautologia :: Formula -> Bool
tautologia form = foldr1 (&&) (respostaTabelaVerdade(truthTable form))

-- -- -- -- -- -- -- -- -- -- -Retorna se formua e uma contradicao- -- -- -- -- -- -- -- -- -- --
contradicao :: Formula -> Bool
contradicao form = not (foldr1 (||) (respostaTabelaVerdade(truthTable form)))

-- Retorna somente os resultados da tabela verdade 
respostaTabelaVerdade :: TabelaVerdade -> [Bool]
respostaTabelaVerdade [] = []
respostaTabelaVerdade ((_,b):xs) = [b] ++ respostaTabelaVerdade xs

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -EXTRAS DO TRABALHO- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--Traduz formula para modo legivel
traduzForm :: Formula -> String
traduzForm (Lit True)            = "True"
traduzForm (Lit False)           = "False"
traduzForm (Var f)               = f
traduzForm (Nao f)               = "~" ++ traduzForm f
traduzForm (E f0 f1)             = "(" ++ traduzForm f0 ++ " & "   ++ traduzForm f1 ++ ")" 
traduzForm (Ou  f0 f1)           = "(" ++ traduzForm f0 ++ " | "   ++ traduzForm f1 ++ ")" 
traduzForm (Implic f0 f1)    = "(" ++ traduzForm f0 ++ " -> "  ++ traduzForm f1 ++ ")" 
traduzForm (Bicond f0 f1) = "(" ++ traduzForm f0 ++ " <-> " ++ traduzForm f1 ++ ")" 

-- -- -- -- -- -- -- -- -- -- -- -- -- --LISTA NO CONSOLE-- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Gatilho para imprimir tabela no console
imprimiTabela :: Formula -> IO() 
imprimiTabela fr = do
    putStrLn (cabecalho x)
    imprimiTabelaLinhas (x:xs)
    where table = truthTable fr
          x = head table
          xs = tail table

--Imprime somente cabecalho da tabela
cabecalho :: (Contexto, Bool) -> String
cabecalho ([],bol) = "  R"
cabecalho (((str,bl):xs),bol) = "  "++str++"     " ++ cabecalho(xs,bol)

--Gatilho para imprimir cada linha da tabela
imprimiTabelaLinhas :: TabelaVerdade -> IO() 
imprimiTabelaLinhas [] = return()
imprimiTabelaLinhas (x:xs) = do
    putStrLn (linhaTabela x)
    imprimiTabelaLinhas xs

--Imprime cada linha da tabela
linhaTabela :: (Contexto, Bool) -> String
linhaTabela ([],bl)
    |bl = "True"
    |otherwise = "False"
linhaTabela (((_,bl):xs),bol)
    |bl = "True    " ++ linhaTabela(xs,bol)
    |otherwise = "False   " ++ linhaTabela(xs,bol)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- --LISTA EM HTML-- -- -- -- -- -- -- -- -- -- -- -- -- --

--Gatilho para imprimir (arquivo) HTML da Tabela
html :: Formula -> IO()
html fr = do
    arq <- openFile "tabelaVerdade.html" WriteMode
    hPutStr arq (tabelaHTML fr )
    putStrLn "Arquivo html gerado com sucesso na pasta fonte desse programa Haskell"
    putStrLn "**O conteúdo do .html pode demorar para aparecer no arquivo"

--Gatilho para imprimir HTML com dados base da tabela HTML
tabelaHTML :: Formula -> String
tabelaHTML fr = 
    "<!DOCTYPE html>\n<html>\n"++
    "<head>\n\t<title>Tabela verdade - Haskell</title> \n</head>"++
    "\n<body>"++
    "\n\t<table border=1 cellspacing=0 cellpadding=5>\n"++
    "\t\t<caption>" ++ traduzForm fr ++ "</caption>"++
        "\n\t\t<thead> \n\t\t\t"++
           "<tr>\n" ++ 
            (cabecalhoHTML x)++
            "\t\t\t</tr>"++
        "\n\t\t</thead>\n"++
        "\t\t<tbody>\n" ++
            (linhasHTML table)++
        "\t\t</tbody>\n"++
    "</table>\n</body>\n</html>"
    where table = truthTable fr
          x = head table
          xs = tail table

--Define cabeçalho da Tabela HTML
cabecalhoHTML :: (Contexto, Bool) -> String
cabecalhoHTML ([],bol) = "\t\t\t\t<th>"++"R"++"</th>\n"
cabecalhoHTML (((str,bl):xs),bol) = "\t\t\t\t<th>"++str++"</th>\n" ++ cabecalhoHTML(xs,bol)

-- Gatilho para imprimir cada linha HTML
linhasHTML :: TabelaVerdade -> String
linhasHTML [] = ""
linhasHTML (x:xs) = "\t\t\t<tr>\n" ++ linhaInternaHTML x ++ "\t\t\t</tr>\n" ++ linhasHTML xs

--Define cada linha da tabela em HTML
linhaInternaHTML :: (Contexto, Bool) -> String
linhaInternaHTML ([],bl)
    |bl = "\t\t\t\t<td>"++"True"++"</td>\n"
    |otherwise = "\t\t\t\t<td>"++"False"++"</td>\n"
linhaInternaHTML (((_,bl):xs),bol)
    |bl = "\t\t\t\t<td>"++"True"++"</td>\n" ++ linhaInternaHTML(xs,bol)
    |otherwise = "\t\t\t\t<td>"++"False"++"</td>\n" ++ linhaInternaHTML(xs,bol)


main :: IO()
main = do
    putStrLn "Digite a fórmula"
    let form = (Ou (E (Var "A") (Var "B")) (Implic (Var "B") (Var "D")))
    let context =  [("A",True),("B",False),("C",True),("D",False)]
    putStrLn "Escolha uma opção:"
    putStrLn "1 - Avaliar fórmula"
    putStrLn "2 - Criar tabela verdade"
    putStrLn "3 - Criar arquivo HTML com a tabela verdade"
    op <- getLine
    if op == "1"
        then putStrLn $ show (avalia context form )
        else putStr ""
    if op == "2"
        then imprimiTabela form
        else putStr ""
    if op == "3"
        then html form
        else putStr ""
    return()

-- Lista de EXTRAS
-- FEITO    (+0,5) Faca uma funcao que dado uma formula escreva toda a tabela verdade em HTML.
-- entrada? (+0,5) Faca a funcao main :: IO () que seja um menu que o usuario passa a formula e,
--              em seguida, seja capaz de realizar as operacoes de avaliar (recebe um contexto),
--              criar uma tabela verdade.
--          (+1,0) Dado um arquivo escrito de forma como escrevemos (“A ou B e C e Verdadeiro”), 
--              consiga executar a avaliacao ou gerar a tabela verdade desta formula.
-- FEITO    (+0,5) Adicionar Implic e Bicond