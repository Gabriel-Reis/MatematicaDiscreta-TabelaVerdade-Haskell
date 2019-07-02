-- -- -- -- -- -- -- -- -- -- -- -- -- -- --LISTA EM HTML-- -- -- -- -- -- -- -- -- -- -- -- -- --

--Gatilho para imprimir (arquivo) HTML da Tabela
html :: Formula -> IO()
html fr = do
    arq <- openFile "teste.html" WriteMode
    hPutStr arq (tabelaHTML fr )
    putStrLn "Arquivo html gerado com sucesso na pasta fonte desse programa Haskell"
    putStrLn "**O conteúdo do .html pode demorar alguns segundos para aparecer no arquivo"

--Gatilho para imprimir HTML com dados base da tabela HTML
tabelaHTML :: Formula -> String
tabelaHTML fr = 
    "<!DOCTYPE html>\n<html>\n"++
    "<head>\n\t<title>Tabela verdade - Haskell</title>"++"\n"++
    "<link rel=\"stylesheet\"href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css"++
    "/bootstrap.min.css\"integrity=\"sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/"++
    "iJTQUOhcWr7x9JvoRxT2MZw1T\" crossorigin=\"anonymous\"> \n</head>"++
    "\n<body>"++
    "\n\t<table border=1 cellspacing=0 cellpadding=5 class=\"table\">\n"++
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
