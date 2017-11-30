{-|
Module : Main
Description : Módulo Haskkel que contém o código relativo à Tarefa D
Copyright : José Carlos <jcm300@live.com.pt>
            Sérgio <stoj97@gmail.com>

Módulo contendo a realização da tarefa D que tem como objetivo receber um mapa válido
que possui na ultima linha uma lista de comandos, e com essa lista, verificar se ao
realizar esses comandos o jogo Sokoban está completo ou incompleto e diz o número de
jogadas realizadas.
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

-- | Corre múltiplos testes para a tarefa
correTestes :: IO ()
correTestes = do
    files4 <- getDirectoryContents "/home/jcm300/li1g163/tests/T4"
    let inputs4 = map ("/home/jcm300/li1g163/tests/T4/" ++) $ filter (isSuffixOf ".in") files4
    mapM_ (correTeste tarefa4) inputs4

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

-- | Chama a Tarefa 4
main = do inp <- getContents
          putStr (outStr (tarefa4 (inStr inp)))

-- | tipo criado em que U (Up), D(Down), L(Left), R(Right) e Nada(caso não seja nenhum dos casos)
data Command = U | D | L | R | Nada
     deriving Show

-- * Tarefa 4
-- | Tarefa onde diz se o jogo está completo ou não e diz o número de jogadas.
tarefa4 :: [String] -> [String]
tarefa4 linhas = if verificaJogo==True 
                 then ["FIM " ++ (show nMovs)]
                 else ["INCOMPLETO " ++ (show nMovs)]
            where
            verificaJogo = verificarCI (colocaCaixas tab coordsFCaixas)
            (coordFBoneco,coordsFCaixas,nMovs) = moveBoneco tab coordBoneco comandos coordenadasCs 0
            (tab,coords) = parteMapaB linhas
            coordBoneco = (liToT (sToI (parteCo coords)))
            comandos = converteString (lastM(remVazio coords))
            coordenadasCs = tuploCoord (remInc(take (length (drop 1 coords) - 1) (drop 1 coords)))

-- * Funções da Tarefa 4
-- | Parte mapa em tabuleiro e coordenadas com o comando.
parteMapaB :: [String] -> ([String],[String])
parteMapaB [] = ([],[])
parteMapaB (x:xs) = splitAt (contaListasMapa (x:xs)-1) (x:xs)

-- | função last
lastM :: [String] -> String
lastM [] = []
lastM [x] = x
lastM (x:xs) = lastM xs

-- | Converte uma string de comandos numa lista de comandos.
converteString :: String -> [Command]
converteString [] = []
converteString (x:xs) = [converteC x] ++ converteString xs 

-- | Converte o valor do caracter para o tipo command.
converteC :: Char -> Command
converteC x = case x of
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

-- | verifica se o mapa tem todas as caixas na posição final
verificarCI :: [String] -> Bool
verificarCI [] = True
verificarCI (x:xs) = verificalinha x && verificarCI xs

-- | verifica se numa linha linha existe alguma caixa fora da posição final.
verificalinha :: String -> Bool
verificalinha [] = True
verificalinha (x:xs) 
        |(ord x)== 72 = False
        |otherwise = verificalinha xs 

-- | movimenta boneco após receber os comandos.
moveBoneco :: [String] -> (Int,Int) ->  [Command] -> [(Int, Int)] -> Int -> ((Int,Int),[(Int,Int)],Int)
moveBoneco _ (x,y) [] li n = ((x,y),li,n)
moveBoneco tab bno (t:ts) coords n = if verificarCI (colocaCaixas tab coords)==True 
                                        then (bno,coords,n)
                                        else let ((a,b),l,nf)= moveB tab bno t coords n
                                             in moveBoneco tab (a,b) ts l nf

-- | Movimenta o boneco ao receber apenas um comando.
moveB :: [String] -> (Int,Int) -> Command -> [(Int, Int)] -> Int -> ((Int,Int),[(Int,Int)],Int)
moveB (x:xs) bno t coords n = if (fst(bno))==(-1)
                            then (bno,coords,n)
                            else verificaMov (colocaCaixas (x:xs) coords) bno coords t n

-- | Verifica se o movimento é válido...
verificaMov :: [String] -> (Int,Int) -> [(Int,Int)] -> Command -> Int -> ((Int,Int),[(Int,Int)],Int)
verificaMov (h:t) (x,y) coords Nada n = ((x,y),mudaCoordsCaixas x y coords Nada,n)
verificaMov (h:t) (x,y) coords R n |devolveCR1 == '#' = ((x,y),mudaCoordsCaixas x y coords R,n)
                                   |devolveCR1 == 'H' || devolveCR1 == 'I' = if(devolveCR2 == '#' || devolveCR2 == 'H' || devolveCR2 == 'I') 
                                                                             then ((x,y),mudaCoordsCaixas x y coords R,n)
                                                                             else ((x+1,y),mudaCoordsCaixas (x+1) y coords R,n+1)
                                   |otherwise = ((x+1,y),mudaCoordsCaixas (x+1) y coords R,n+1)
                                   where
                                    devolveCR1 = devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x+1) 0
                                    devolveCR2 = devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x+2) 0
verificaMov (h:t) (x,y) coords L n |devolveCL1 == '#' = ((x,y),mudaCoordsCaixas x y coords L,n)
                                   |devolveCL1 == 'H' || devolveCL1 == 'I' = if(devolveCL2 == '#' || devolveCL2 == 'H' || devolveCL2 == 'I') 
                                                                             then ((x,y),mudaCoordsCaixas x y coords L,n)
                                                                             else ((x-1,y),mudaCoordsCaixas (x-1) y coords L,n+1)
                                   |otherwise = ((x-1,y),mudaCoordsCaixas (x-1) y coords L,n+1)
                                   where
                                    devolveCL1 = devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x-1) 0
                                    devolveCL2 = devolveCaracterB (contalinhasB (reverse (h:t)) y 0) (x-2) 0
verificaMov (h:t) (x,y) coords D n |devolveCD1 == '#' = ((x,y),mudaCoordsCaixas x y coords D,n)
                                   |devolveCD1 == 'H' || devolveCD1 == 'I' = if(devolveCD2 == '#' || devolveCD2 == 'H' || devolveCD2 == 'I') 
                                                                             then ((x,y),mudaCoordsCaixas x y coords D,n)
                                                                             else ((x,y-1),mudaCoordsCaixas x (y-1) coords D,n+1)
                                   |otherwise = ((x,y-1),mudaCoordsCaixas x (y-1) coords D,n+1)
                                   where
                                    devolveCD1 = devolveCaracterB (contalinhasB (reverse (h:t)) (y-1) 0) x 0
                                    devolveCD2 = devolveCaracterB (contalinhasB (reverse (h:t)) (y-2) 0) x 0
verificaMov (h:t) (x,y) coords U n |devolveCU1 == '#' = ((x,y),mudaCoordsCaixas x y coords U,n)
                                   |devolveCU1 == 'H' || devolveCU1 == 'I' = if(devolveCU2 == '#' || devolveCU2 == 'H' || devolveCU2 == 'I') 
                                                                             then ((x,y),mudaCoordsCaixas x y coords U,n)
                                                                             else ((x,y+1),mudaCoordsCaixas x (y+1) coords U,n+1)
                                   |otherwise = ((x,y+1),mudaCoordsCaixas x (y+1) coords U,n+1)
                                   where
                                    devolveCU1 = devolveCaracterB (contalinhasB (reverse (h:t)) (y+1) 0) x 0
                                    devolveCU2 = devolveCaracterB (contalinhasB (reverse (h:t)) (y+2) 0) x 0

-- | muda as coordenadas das caixas caso o boneco empurre uma delas
mudaCoordsCaixas :: Int -> Int -> [(Int,Int)] -> Command -> [(Int,Int)]
mudaCoordsCaixas x y [] _ = []
mudaCoordsCaixas x y t Nada = t
mudaCoordsCaixas x y ((a,b):c) U
        |x==a && y==b = ((a,b+1):c)
        |otherwise = (a,b):(mudaCoordsCaixas x y c U)
mudaCoordsCaixas x y ((a,b):c) D
        |x==a && y==b = ((a,b-1):c)
        |otherwise = (a,b):(mudaCoordsCaixas x y c D)
mudaCoordsCaixas x y ((a,b):c) L
        |x==a && y==b = ((a-1,b):c)
        |otherwise = (a,b):(mudaCoordsCaixas x y c L)
mudaCoordsCaixas x y ((a,b):c) R
        |x==a && y==b = ((a+1,b):c)
        |otherwise = (a,b):(mudaCoordsCaixas x y c R)

-- | Coloca as caixas numa linha.
colocaCaixa :: [String] -> (Int,Int) -> [String]
colocaCaixa l (x,y) = reverse (pLinha (reverse l) (x,y)) 
  where
    pLinha :: [String] -> (Int, Int) -> [String]
    pLinha [] _ = []
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
colocaCaixas l [] = l
colocaCaixas l [z] = colocaCaixa l z 
colocaCaixas l (h:t) = colocaCaixas (colocaCaixa l h) t

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

{- | Devolve o nº de linhas do tabuleiro.

Nesta função usamos /isDigit/ pertencente ao Data.Char para verificarmos se o elemento é ou não um algarismo (0,...9). -}
contaListasMapa :: [String] -> Int
contaListasMapa [] = 0
contaListasMapa (x:xs)
        |(null x)==True = contaListasMapa xs
        |isDigit (head x)==True = contaListasMapa xs 
        |otherwise = 1 + contaListasMapa xs

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