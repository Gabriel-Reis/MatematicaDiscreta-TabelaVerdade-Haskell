-- Gabriel Augusto Requena dos Reis - 16.2.8105
import System.IO
data Formula = Lit Bool 
        | Var String 
        | E Formula Formula 
        | Ou Formula Formula 
        | Nao Formula 
        | Implic Formula Formula 
        | Bicond Formula Formula
        deriving( Show , Eq )
type contexexto = [(String, Bool)]
type TabelaVerdade = [(contexexto, Bool)]

bd::contexexto
bd = [("A",True),("B",False)]

-- Defina a fun¸c˜ao avaliar que dado uma formula e um contexexto, diz qual ´e o resultado da f´ormula dado
-- esse contexexto.=
avalia :: contexexto -> Formula -> Bool
avalia contex (Var str) = reccontexexto contex str
avalia contex (Lit f) = f
avalia contex (Nao f) = not(avalia contex f)
avalia contex (E f1 f2) = (avalia contex f1) && (avalia contex f2)
avalia contex (Ou f1 f2) = (avalia contex f1) || (avalia contex f2)
avalia contex (Implic f1 f2) = (avalia contex (Nao f1)) || (avalia contex f2)
avalia contex (Bicond f1 f2) = (avalia contex (Implic f1 f2)) && (avalia contex (Implic f2 f1))

reccontexexto :: contexexto -> String -> Bool
reccontexexto ((x,bl):xs) str 
    |x==str = bl
    |otherwise = reccontexexto xs str

-- Defina a funcao que resolva para todos os casos de um formula, ou seja, faca a tabela verdade de uma formula.
-- -- (E (Var "A") (OU (Var "B") (Var "C")))               ENTRADA
-- -- [("A",True), ("B",True), ("C",True)]  True           SAIDA ESPERADA
-- -- [("A",True), ("B",True), ("C",False)] True
-- -- [("A",True), ("B",False),("C",True)]  True
-- -- [("A",True), ("B",False),("C",False)] False
-- -- [("A",False),("B",True), ("C",True)]  False
-- -- [("A",False),("B",True), ("C",False)] False
-- -- [("A",False),("B",False),("C",True)]  False
-- -- [("A",False),("B",False),("C",False)] False
-- -- [ (("x",x),("y",y)) | x<-["True","False"],y<-["True","False"]]      TODAS COMBINAÇOES

truthTable :: Formula -> TabelaVerdade
truthTable form = [(contex,(avalia contex form)) | contex <- possibilidades (remdup(listaVars form))]
    -- where contex = head (possibilidades (listaVars form))

-- Retorna a lista de variáveis utilizadas (COM REPETIÇÃO)
listaVars :: Formula -> [String]
listaVars (Lit _) = []
listaVars (Var f) = [f]
listaVars (Nao f) = listaVars f
listaVars (E f0 f1) = listaVars f0 ++ listaVars f1
listaVars (Ou f0 f1) = listaVars f0 ++ listaVars f1
listaVars (Implic f0 f1) = listaVars f0 ++ listaVars f1
listaVars (Bicond f0 f1) = listaVars f0 ++ listaVars f1

-- Remove duplicados
remdup :: (Eq a) => [a] -> [a]
remdup [] = []
remdup (x:xs) = x: (remdup (filter(/=x) xs))

-- Retorna todos os possíveis contexextos
possibilidades :: [String] -> [contexexto]
possibilidades []     = [[]]
possibilidades (x:xs) = [ (x,bl):v | v <- possibilidades xs, bl <- [True,False]]

-- Defina as funcoes de tautologia e contexradicao para uma formula.
tautologia :: Formula -> Bool
tautologia form = foldr1 (&&) (respostaTabelaVerdade(truthTable form))


contexradicao :: Formula -> Bool
contexradicao form = not (foldr1 (||) (respostaTabelaVerdade(truthTable form)))

respostaTabelaVerdade :: TabelaVerdade -> [Bool]
respostaTabelaVerdade [] = []
respostaTabelaVerdade ((_,b):xs) = [b] ++ respostaTabelaVerdade xs
-- EXTRAS
-- FEITO    (+0,5) Faca uma funcao que dado uma formula que escreva toda a tabela verdade em HTML.
--          (+0,5) Faca a funcao main :: IO () que seja um menu que o usuario passa a formula e, em
--              seguida, seja capaz de realizar as operacoes de avaliar (que deve receber um contexexto), criar uma
--              tabela verdade.
--          (+1,0) Dado um arquivo que esteja escrito de forma como e escrevemos (“A ou B e C e Verdadeiro”,
--              por exemplo), consiga executar a avaliacao ou gerar a tabela verdade desta formula.
-- FEITO    (+0,5) Adicionar Implic e Bicond

imprimiTabela :: TabelaVerdade -> IO() 
imprimiTabela [] = return() 
imprimiTabela (x:xs) = do
    putStrLn (cabecalho x)
    imprimiTabelaLinhas (x:xs)

cabecalho :: (contexexto, Bool) -> String
cabecalho ([],bol) = "  R"
cabecalho (((str,bl):xs),bol) = "  "++str++"   " ++ cabecalho(xs,bol)

imprimiTabelaLinhas :: TabelaVerdade -> IO() 
imprimiTabelaLinhas [] = return()
imprimiTabelaLinhas (x:xs) = do
    putStrLn (linhaTabela x)
    imprimiTabelaLinhas xs

linhaTabela :: (contexexto, Bool) -> String
linhaTabela ([],bl)
    |bl = "True"
    |otherwise = "False"
linhaTabela (((_,bl):xs),bol)
    |bl = "True  " ++ linhaTabela(xs,bol)
    |otherwise = "False " ++ linhaTabela(xs,bol)


html :: TabelaVerdade -> IO()
html tb = do
    arq <- openFile "teste.html" WriteMode
    hPutStr arq (imprimiTabelaHTML tb)

imprimiTabelaHTML :: TabelaVerdade -> IO() 
imprimiTabelaHTML [] = return() 
imprimiTabelaHTML (x:xs) = do
    putStrLn ("<table border=1 cellspacing=0 cellpadding=5>")
    putStrLn ("<thead>")
    putStrLn ("<tr>")
    putStrLn (cabecalhoHTML x)
    putStrLn ("</tr>")
    putStrLn ("</thead>")
    putStrLn ("<tbody>")
    imprimiTabelaLinhasHTML (x:xs)
    putStrLn ("</tbody>")
    putStrLn ("</table>")

cabecalhoHTML :: (contexexto, Bool) -> String
cabecalhoHTML ([],bol) = "<th>"++"R"++"</th>"
cabecalhoHTML (((str,bl):xs),bol) = "<th>"++str++"</th>" ++ cabecalhoHTML(xs,bol)

imprimiTabelaLinhasHTML :: TabelaVerdade -> IO() 
imprimiTabelaLinhasHTML [] = return()
imprimiTabelaLinhasHTML (x:xs) = do
    putStrLn ("<tr>")
    putStrLn (linhaTabelaHTML x)
    putStrLn ("</tr>")
    imprimiTabelaLinhasHTML xs

linhaTabelaHTML :: (contexexto, Bool) -> String
linhaTabelaHTML ([],bl)
    |bl = "<td>"++"True"++"</td>"
    |otherwise = "<td>"++"False"++"</td>"
linhaTabelaHTML (((_,bl):xs),bol)
    |bl = "<td>"++"True"++"</td>" ++ linhaTabelaHTML(xs,bol)
    |otherwise = "<td>"++"False"++"</td>" ++ linhaTabelaHTML(xs,bol)