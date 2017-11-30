{-|
Module : Main
Description : Módulo Haskkel que contém o código relativo ao jogo Sokoban
Copyright : José Carlos <jcm300@live.com.pt>
            Sérgio <stoj97@gmail.com>

Módulo contendo a realização de 3 tarefas referentes ao jogo Sokoban
na qual, a primeira é referente a verificar se um mapa dado é valido,
a segunda referente a simplicar um mapa válido e a terceira referente
a calcular o próximo estado do boneco dado um certo comando.
-}

module Main where

import qualified Data.Text as T
import Data.List
import Data.Char
import System.Directory

-- * Funções do mooshak
-- | Parte uma @String@ numa lista de linhas
inStr :: String -> [String]
inStr [] = []
inStr ['\n'] = [[],[]]
inStr (x:xs) = case x of
    '\n' -> []:inStr xs
    otherwise -> case inStr xs of
        y:ys -> (x:y):ys
        [] -> [[x]]

-- | Junta uma lista de linhas numa @String@
outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines (map (T.unpack . T.stripEnd . T.pack) t)

-- * Funções de teste

-- | Corre múltiplos testes para as três tarefas
correTestes :: IO ()
correTestes = do
    files1 <- getDirectoryContents "/home/jcm300/li1g163/tests/T1"
    files2 <- getDirectoryContents "/home/jcm300/li1g163/tests/T2"
    files3 <- getDirectoryContents "/home/jcm300/li1g163/tests/T3"
    let inputs1 = map ("/home/jcm300/li1g163/tests/T1/" ++) $ filter (isSuffixOf ".in") files1
    let inputs2 = map ("/home/jcm300/li1g163/tests/T2/" ++) $ filter (isSuffixOf ".in") files2
    let inputs3 = map ("/home/jcm300/li1g163/tests/T3/" ++) $ filter (isSuffixOf ".in") files3
    mapM_ (correTeste tarefa1) inputs1
    mapM_ (correTeste tarefa2) inputs2
    mapM_ (correTeste tarefa3) inputs3

-- | Corre um teste para uma tarefa
correTeste :: ([String] -> [String]) -> String -> IO ()
correTeste tarefa input = do
    -- nome do ficheiro
    let nome = reverse $ drop 3 $ reverse input
    -- texto do mapa
    inp <- readFile input
    -- resultado da tarefa
    let o = outStr (tarefa (inStr inp))
    -- resultado esperado
    esp <- readFile (nome ++ ".out")
    putStr ("[" ++ nome ++ "]: ")
    if (o == esp)   -- compara resultados
    then putStrLn "OK"
    else do
        putStrLn "FALHOU"
        putStr esp
        putStrLn o

-- * Funções Tarefas

-- | Chama as Tarefas 1, 2 e 3 
main = do inp <- getContents
          putStr (outStr (tarefa1 (inStr inp)))
--          putStr (outStr (tarefa2 (inStr inp)))
--          putStr (outStr (tarefa3 (inStr inp)))

-- * Tarefa 1
-- | Tarefa onde é suposto verificar se um dado mapa é valido ou não.
tarefa1 :: [String] -> [String]
tarefa1 linhas = if erro<0
                 then ["OK"]
                 else [show erro]
            where erro = tarefa1Erros linhas

-- * Funções da Tarefa 1
-- | Devolve o nº da linha que dá erro.
tarefa1Erros :: [String] -> Int
tarefa1Erros linhas = juntaErros okTab (juntaErros oklength (juntaErros okCoords (juntaErros okCoordsT (juntaErros okCaixas (juntaErros okBoneco okCaixasR)))))
    where
    (tab,coords) = parteMapa linhas
    okTab = validaTab 1 tab (contaListasMapa tab)
    okCoords = validaCoords tab (remInc coords) 1
    okCaixas = validaCaixas tab (remVazio coords)
    oklength = verificalength tab 1
    okCoordsT = validaCoordsT coords 1 (contaListasMapa tab)
    okBoneco = validaBoneco (daCoords 0 (reverse tab) 0) (tuploCoord (remInc coords)) (contaListasMapa tab)
    okCaixasR = validaCaixasR (tuploCoord (remInc coords)) (drop 1 (tuploCoord (remInc coords))) (contaListasMapa tab) 2

-- | Divide o mapa em tabuleiro e coordenadas. Para isso usamos a função splitAt...
--
-- @
-- splitAt :: Int -> [a] -> ([a], [a])
-- @
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])

parteMapa :: [String] -> ([String],[String])
parteMapa [] = ([],[])
parteMapa (x:xs) = splitAt (contaListasMapa (x:xs)) (x:xs)

{- | Devolve o nº de linhas do tabuleiro.

Nesta função usamos /isDigit/ pertencente ao Data.Char para verificarmos se o elemento é ou não um algarismo (0,...9). -}
contaListasMapa :: [String] -> Int
contaListasMapa [] = 0
contaListasMapa (x:xs)
        |(null x)==True = contaListasMapa xs
        |isDigit (head x)==True = contaListasMapa xs 
        |otherwise = 1 + contaListasMapa xs

{- | Devolve o nº de linhas de coordenadas.

Nesta função usamos /isDigit/ pertencente ao Data.Char para verificarmos se o elemento é ou não um algarismo (0,...9). -}
contaListasBC :: [String] -> Int
contaListasBC [] = 0
contaListasBC (x:xs)
        |(null x)==True = contaListasBC xs
        |isDigit (head x)==True = 1 + contaListasBC xs 
        |otherwise = contaListasBC xs

-- | Dá o nº da linha com o erro apos verificar as linhas do tabuleiro
validaTab :: Int -> [String] -> Int -> Int
validaTab pos [] m = -1
validaTab pos (l:ls) m = juntaErros erroLinhaTab erroLinhasTab
        where
        erroLinhaTab = validaLinhaTab pos l m
        erroLinhasTab = validaTab (pos+1) ls m

-- | Função que verifica se o tamanho das listas no tabuleiro é sempre igual.
verificalength :: [String] -> Int -> Int
verificalength [] n = -1
verificalength [l,lt] n
            |(length l)==(length lt) = -1
            |otherwise = n+1
verificalength (a:b:ab) n
            |(length a)==(length b) = verificalength (b:ab) (n+1)
            |otherwise = n+1

{- | Testa uma linha do tabuleiro. 

Nesta função usamos /ord/ que pertence ao Data.Char...

@
ord :: Char -> Int
@

Exemplos de utilização:

Se o char 'a' for um # então:
@
ord a == 35
@
Se o char 'a' for um espaço então:
@
ord a == 32
@
Se o char 'a' for um . então:
@
ord a == 46
@
-}
validaLinhaTab :: Int -> String -> Int -> Int
validaLinhaTab pos [] m = -1
validaLinhaTab pos (a:b) m |pos==1 && ord a == 35 = validaLinhaTab pos b m
                           |pos==m && ord a == 35 = validaLinhaTab pos b m
                           |pos>1 && pos<m && ord a == 35 && ord(last (a:b)) == 35 = aux pos b 
                           |otherwise = pos
                  where aux :: Int -> String -> Int
                        aux pos [] = -1
                        aux pos (x:xs) |ord x == 35 || ord x == 32 || ord x == 46 = aux pos xs
                                       |otherwise = pos 

-- | Testa as coordenadas e devolve a linha em caso de haver erros.
validaCoords :: [String] -> [String] -> Int -> Int
validaCoords _ [] n = -1
validaCoords (x:xs) (h:t) n = if (fst (liToT (sToI h))) == -1 
                              then -1
                              else if ord (devolveCaracter (contalinhas (x:xs) (snd (liToT (sToI h))) 0 (contaListasMapa (x:xs))) (fst (liToT (sToI h))) 0) == 32 || ord (devolveCaracter (contalinhas (x:xs) (snd (liToT (sToI h))) 0 (contaListasMapa (x:xs))) (fst (liToT (sToI h))) 0) == 46 
                                   then validaCoords (x:xs) t (n+1)
                                   else contaListasMapa (x:xs) + n  

-- | Verifica se o boneco está dentro do mapa.
validaBoneco :: [(Int,Int)] -> [(Int,Int)] -> Int -> Int
validaBoneco (x:xs) [] m = m+1
validaBoneco [] _ m = m+1 
validaBoneco (x:xs) (y:ys) m
            |snd(x)==snd(y) && fst(x)==fst(y) = -1
            |otherwise = validaBoneco xs (y:ys) m

-- | Verifica se as coordenadas são válidas, ou seja, se são dois números...
validaCoordsT :: [String] -> Int -> Int -> Int
validaCoordsT [] n m = -1
validaCoordsT (x:xs) n m
             |(length (words x)) == 0 = validaCoordsT xs (n+1) m
             |(length (words x)) /= 2 = m+n
             |aux x == False = m+n
             |otherwise = validaCoordsT xs (n+1) m
             where aux :: String -> Bool
                   aux [] = True
                   aux (x:xs) = if (isDigit x) || x==' ' 
                                then aux xs 
                                else False 

-- | Converte uma String numa lista de dois números.
sToI :: String -> [Int]
sToI [] = []
sToI (x:xs) = aux (words (x:xs))
          where aux :: [String] -> [Int]
                aux [] = []
                aux (x:xs) = (read x :: Int):aux xs

-- | Função que remove as coordenadas incorretas (as coordenadas que só tem um número ou as coordenadas que têm caracteres inválidos).
remInc :: [String] -> [String]
remInc [] = []
remInc (h:t) |h=="" = remInc t
             |length (words h) /= 2 = remInc t
             |aux h == False = remInc t
             |otherwise = h:(remInc t)
        where aux :: String -> Bool
              aux [] = True
              aux (x:xs) = if (isDigit x) || x==' ' 
                           then aux xs 
                           else False 

-- | Recebe uma lista de dois numeros tuplos.
liToT :: [Int] -> (Int,Int)
liToT l = if (null l) then (-1,-1) 
          else (head l,last l) 

-- | Devolve o caracter de uma certa lista
devolveCaracter :: String -> Int -> Int -> Char
devolveCaracter [] p n = ' '
devolveCaracter (x:xs) p n
        |n==p = x
        |otherwise =devolveCaracter xs p (n+1) 
       
-- | Devolve a linha correspondente ao número mas contando de baixo para cima...
contalinhas :: [String] -> Int -> Int -> Int -> String
contalinhas [] p n m = [] 
contalinhas (x:xs) p n m
        |n==(m-p-1) = x
        |otherwise = contalinhas xs p (n+1) m

-- | Verifica a correspondencia entre nº de pontos no tabuleiro e coordenadas.
validaCaixas :: [String] -> [String] -> Int
validaCaixas (x:xs) (y:ys) 
        |(contaListasBC (y:ys)-1) == numeroPontos (x:xs) = -1
        |(contaListasBC (y:ys)-1) > numeroPontos (x:xs) = (contaListasMapa (x:xs)) + numeroPontos (x:xs) + 2
        |(contaListasBC (y:ys)-1) < numeroPontos (x:xs) = (contaListasMapa (x:xs)) + contaListasBC (y:ys) + 1
validaCaixas _ _ = 1

-- | Devolve o número de pontos existentes no mapa
numeroPontos :: [String] -> Int
numeroPontos [] = 0
numeroPontos (h:t) = aux h + numeroPontos t
          where aux :: String -> Int
                aux [] = 0
                aux (h:t)
                    |ord h == 46 = 1 + aux t
                    |otherwise = aux t

-- | verifica se existe ou nao uma caixa sobreposta sobre o boneco ou sobre uma caixa.
validaCaixasR :: [(Int,Int)] -> [(Int,Int)] -> Int -> Int -> Int
validaCaixasR [] _ m n = -1
validaCaixasR _ [] m n = -1
validaCaixasR (x:xs) (y:ys) m n = juntaErros (aux x (y:ys) m n) (validaCaixasR xs ys m (n+1)) 
      where aux :: (Int,Int) -> [(Int,Int)] -> Int -> Int -> Int
            aux (x,y) [] m n = -1
            aux (x,y) (z:zs) m n
                  |x==fst(z) && y==snd(z) = m+n
                  |otherwise = aux (x,y) zs m (n+1)

-- | Dá o erro com menor
juntaErros :: Int -> Int -> Int
juntaErros i j | i<0=j
               | j<0=i
               | otherwise = min i j

-- * Tarefa 2
-- | Tarefa onde é suposto simplicar um mapa válido.
tarefa2 :: [String] -> [String]
tarefa2 linhas = colocaLetras removeTab coords 0 (contaListasMapa tab)
            where 
            (tab,coords) = parteMapa linhas
            removeTab = removeCardinais tab (daCoords 0 tab 0) tab

-- * Funções da Tarefa 2
-- | Dá as coordenadas de cada caracter do mapa numa lista.
daCoords :: Int -> [String] -> Int -> [(Int,Int)]
daCoords n [] p = []
daCoords n (h:t) p = (aux n h p) ++ (daCoords n t (p+1))
          where aux ::Int -> String -> Int -> [(Int,Int)]
                aux n [] p = []
                aux n (x:xs) p = (n,p):(aux (n+1) xs p) 

-- | Remove os cardinais desnecessários do tabuleiro.
removeCardinais :: [String] -> [(Int,Int)] -> [String] -> [String]
removeCardinais [] cd tb = []
removeCardinais (h:t) cd tb = (removedalinha h cd tb):(removeCardinais t (drop (aux h) cd) tb)
                  where aux :: String -> Int
                        aux [] = 0
                        aux (h:t) = 1+(aux t)

-- | Remove os cardinais desnecessários de uma linha...
removedalinha :: String -> [(Int,Int)] -> [String] -> String
removedalinha [] cd tb = [] 
removedalinha (h:t) cd tb = (aux h cd tb):(removedalinha t (drop 1 cd) tb)
                    where aux :: Char -> [(Int,Int)] -> [String] -> Char
                          aux h cd tb 
                                |ord (devolveCaracterB (contalinhasB tb (snd(head(cd))) 0) (fst(head(cd))) 0) == 46 = '.'
                                |ord (devolveCaracterB (contalinhasB tb (snd(head(cd))) 0) (fst(head(cd))) 0) == 32 = ' '
                                |ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))-1) 0) (fst(head(cd))) 0) == 32 || ord (devolveCaracterB (contalinhasB tb (snd(head(cd))) 0) ((fst(head(cd)))+1) 0) == 32 ||  ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))-1) 0) ((fst(head(cd)))+1) 0) == 32 || ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))-1) 0) ((fst(head(cd)))-1) 0) == 32 || ord (devolveCaracterB (contalinhasB tb (snd(head(cd))) 0) ((fst(head(cd)))-1) 0) == 32 || ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))+1) 0) (fst(head(cd))) 0) == 32 || ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))+1) 0) ((fst(head(cd)))+1) 0) == 32 || ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))+1) 0) ((fst(head(cd)))-1) 0) == 32 = '#'
                                |ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))-1) 0) (fst(head(cd))) 0) == 46 || ord (devolveCaracterB (contalinhasB tb (snd(head(cd))) 0) ((fst(head(cd)))+1) 0) == 46 ||  ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))-1) 0) ((fst(head(cd)))+1) 0) == 46 || ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))-1) 0) ((fst(head(cd)))-1) 0) == 46 || ord (devolveCaracterB (contalinhasB tb (snd(head(cd))) 0) ((fst(head(cd)))-1) 0) == 46 || ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))+1) 0) (fst(head(cd))) 0) == 46 || ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))+1) 0) ((fst(head(cd)))+1) 0) == 46 || ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))+1) 0) ((fst(head(cd)))-1) 0) == 46 = '#'
                                |otherwise = ' '

-- | Função que devolve o caracter de uma certa lista.
devolveCaracterB :: String -> Int -> Int -> Char
devolveCaracterB [] p n = '#'
devolveCaracterB (x:xs) p n
        |n==p = x
        |otherwise =devolveCaracterB xs p (n+1)

-- | Devolve a linha do nº introduzido contando de cima para baixo.
contalinhasB :: [String] -> Int -> Int -> String
contalinhasB [] p n = [] 
contalinhasB (x:xs) p n
        |n==p = x
        |otherwise = contalinhasB xs p (n+1)

-- | Transforma as strings das coordenadas numa lista de tuplos.
tuploCoord :: [String] -> [(Int,Int)] 
tuploCoord [] = []
tuploCoord (x:xs) = liToT (sToI x):tuploCoord xs

-- | Transforma uma lista de tuplos numa lista com apenas os segundos elementos.
sndCoord :: [(Int,Int)] -> [Int]
sndCoord [] = []
sndCoord ((x,y):t) = y:(sndCoord t)

-- | Devolve os tuplos correspondentes ao número introduzido...
devCoord :: Int -> [(Int,Int)] -> [(Int,Int)]
devCoord n [] = []
devCoord n (h:t)
        |n==snd(h) = h:(devCoord n t)
        |otherwise = devCoord n t  

-- | Coloca as letras no tabuleiro.
colocaLetras :: [String] -> [String] -> Int -> Int -> [String]
colocaLetras [] _ n m = []
colocaLetras (x:xs) (y:ys) n m
          |(snd(head(tuploCoord (y:ys))))==(m-n-1) = (colocaL 1 x (devCoord (m-n-1) (tuploCoord (y:ys)))):(colocaLetras xs (y:ys) (n+1) m)
          |(elem (m-n-1) (sndCoord(tuploCoord (y:ys))))==True = (colocaL 0 x (devCoord (m-n-1) (tuploCoord (y:ys)))):(colocaLetras xs (y:ys) (n+1) m)
          |otherwise = x:(colocaLetras xs (y:ys) (n+1) m)

-- | Coloca as letras numa linha..
colocaL :: Int -> String -> [(Int,Int)]-> String
colocaL n l [] = l
colocaL n l (q:w)
          |n==1 = colocaL (n-1) (colocaO l q) w
          |otherwise = colocaL n (colocaIH l q) w 

-- | Coloca o sitio do boneco.
colocaO :: String -> (Int,Int) -> String
colocaO [] (x,y) = []
colocaO (h:t) (x,y) = aux (splitAt x (h:t))
        where aux :: (String,String) -> String
              aux (a,b) = a ++ "o" ++ drop 1 b

-- | Coloca as letras I e H.
colocaIH :: String -> (Int,Int) -> String
colocaIH [] (x,y) = []
colocaIH (h:t) (x,y)
        |(ord (aux (h:t) x))==46 = auxP (splitAt x (h:t))
        |otherwise = auxE (splitAt x (h:t))
          where aux :: String -> Int -> Char
                aux [] n = ' '
                aux (h:t) 0 = h
                aux (h:t) n 
                    |n>0 = aux t (n-1)

-- | Insere o I numa lista.
auxP :: (String,String) -> String
auxP (a,b) = a ++ "I" ++ drop 1 b

-- | Insere o H numa lista.
auxE :: (String,String) -> String
auxE (a,b) = a ++ "H" ++ drop 1 b

-- * Auxiliares da Tarefa 3
-- | tipo criado em que U (Up), D(Down), L(Left), R(Right) e Nada(caso não seja nenhum dos casos)
data Command = U | D | L | R | Nada
     deriving Show

-- * Tarefa 3
-- | Tarefa onde é suposto calcular o próximo estado do boneco dado um certo comando.
tarefa3 :: [String] -> [String]
tarefa3 linhas = moveB tab coordBoneco comando coordenadasCs
            where 
            (tab,coords) = parteMapaB linhas
            coordBoneco = parteCo coords
            comando = converteC (last(remVazio coords))
            coordenadasCs = tuploCoord (remInc(take (length (drop 1 coords) - 1) (drop 1 coords)))

-- * Funções da Tarefa 3
-- | Parte mapa em tabuleiro e coordenadas com o comando.
parteMapaB :: [String] -> ([String],[String])
parteMapaB [] = ([],[])
parteMapaB (x:xs) = splitAt (contaListasMapa (x:xs)-1) (x:xs)

-- | Converte o valor da string para o tipo command.
converteC :: String -> Command
converteC [] = Nada
converteC (x:xs) =case x of
                    'U' -> U
                    'L' -> L
                    'D' -> D
                    'R' -> R
                    otherwise -> Nada

-- | Devolve a string com a coordenada do boneco inicial.
parteCo :: [String] -> String
parteCo [] = []
parteCo (x:xs) = x

-- | Remove listas vazias.
remVazio :: [String] -> [String]
remVazio [] = []
remVazio (x:xs) 
        |x=="" = remVazio xs
        |otherwise =x:(remVazio xs)

-- | Movimenta o boneco.
moveB :: [String] -> String -> Command -> [(Int, Int)] -> [String]
moveB (x:xs) bno t coords = if (fst(liToT (sToI bno)))==(-1)
                            then [bno]
                            else verificaMov (colocaCaixas (x:xs) coords) (liToT (sToI bno)) t 
moveB _ _ t _ = []

-- | Verifica se o movimento é válido...
verificaMov :: [String] -> (Int,Int) -> Command -> [String]
verificaMov (h:t) (x,y) Nada = iToS x y
verificaMov (h:t) (x,y) R   |devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x+1) 0 == '#' = iToS x y
                            |devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x+1) 0 == 'H' || devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x+1) 0 == 'I' = if(devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x+2) 0 == '#' || devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x+2) 0 == 'H' || devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x+2) 0 == 'I') 
                                                                                                                                                                        then iToS x y
                                                                                                                                                                        else iToS (x+1) y
                            |otherwise = iToS (x + 1) y

verificaMov (h:t) (x,y) L   |devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x-1) 0 == '#' = iToS x y
                            |devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x-1) 0 == 'H' || devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x-1) 0 == 'I' = if(devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x-2) 0 == '#' || devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x-2) 0 == 'H' || devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x-2) 0 == 'I') 
                                                                                                                                                                        then iToS x y
                                                                                                                                                                        else iToS (x-1) y
                            |otherwise = iToS (x - 1) y

verificaMov (h:t) (x,y) D   |devolveCaracterB (contalinhasB (reverse (h:t)) (y-1) 0) x 0 == '#' = iToS x y
                            |devolveCaracterB (contalinhasB (reverse (h:t)) (y-1) 0) x 0 == 'H' || devolveCaracterB (contalinhasB (reverse (h:t)) (y-1) 0) x 0 == 'I' = if(devolveCaracterB (contalinhasB (reverse (h:t)) (y-2) 0) x 0 == '#' || devolveCaracterB (contalinhasB (reverse (h:t)) (y-2) 0) x 0 == 'H' || devolveCaracterB (contalinhasB (reverse (h:t)) (y-2) 0) x 0 == 'I') 
                                                                                                                                                                        then iToS x y
                                                                                                                                                                        else iToS x (y-1)
                            |otherwise = iToS x (y-1)

verificaMov (h:t) (x,y) U   |devolveCaracterB (contalinhasB (reverse (h:t)) (y+1) 0) x 0 == '#' = iToS x y
                            |devolveCaracterB (contalinhasB (reverse (h:t)) (y+1) 0) x 0 == 'H' || devolveCaracterB (contalinhasB (reverse (h:t)) (y+1) 0) x 0 == 'I' = if(devolveCaracterB (contalinhasB (reverse (h:t)) (y+2) 0) x 0 == '#' || devolveCaracterB (contalinhasB (reverse (h:t)) (y+2) 0) x 0 == 'H' || devolveCaracterB (contalinhasB (reverse (h:t)) (y+2) 0) x 0 == 'I') 
                                                                                                                                                                        then iToS x y
                                                                                                                                                                        else iToS x (y+1)
                            |otherwise = iToS x (y+1)

-- | Transforma dois números inteiros numa string com um espaço no meio.
iToS :: Int -> Int -> [String]
iToS x y = [(show x) ++ " " ++ (show y)]

-- | Coloca as caixas numa linha.
colocaCaixa :: [String] -> (Int,Int) -> [String]
colocaCaixa l (x,y) = reverse (pLinha (reverse l) (x,y)) 
  where
    pLinha :: [String] -> (Int, Int) -> [String]
    pLinha (h:t) (x,y) = if(y == 0) 
                         then ((pColuna h x):t) 
                         else (h:pLinha t (x,y-1)) 

    pColuna :: String -> Int -> String
    pColuna [] _ = []
    pColuna (z:zs) n = if(n == 0) then if(z == '.') 
                                       then 'I' : zs 
                                       else 'H' : zs 
                       else (z:pColuna zs (n-1))

-- | Coloca as caixas no mapa.
colocaCaixas :: [String] -> [(Int, Int)] -> [String]
colocaCaixas l [z] = colocaCaixa l z 
colocaCaixas l (h:t) = colocaCaixas (colocaCaixa l h) t

-- | Tarefa 4
tarefa4 :: [String] -> [String]
tarefa4 (x:xs) = tarefa3 xs