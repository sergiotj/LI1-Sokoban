{-|
Module : Main
Description : Módulo Haskkel que contém o código do Jogo Sokoban
Copyright : José Carlos <jcm300@live.com.pt>
            Sérgio <stoj97@gmail.com>

Módulo contendo a realização do jogo Sokoban.
-}
module Main where

import Graphics.Gloss -- interface principal gloss
import Graphics.Gloss.Data.Picture -- para desenhar @Picture@s
import Graphics.Gloss.Interface.Pure.Game -- para reagir a @Event@s
import qualified Data.Text as T
import Data.Char
import System.Directory

-- | Estado do jogo:
--
-- * Dimensões do mapa
-- * lista de Coordenadas do boneco no mapa
-- * lista de lista de Coordenadas
-- * Score
-- * Mapa após mover
-- * Mapa sem cardinais
-- * Se está ou não completo o nível
-- * lista com os movimentos já feitos
-- * número do mapa
type Mapa = ((Int,Int),[(Int,Int)],[[(Int,Int)]],Int,[String],[String],Bool,Int)

-- | tipo criado em que U (Up), D(Down), L(Left), R(Right) e Nada(caso não seja nenhum dos casos)
data Command = U | D | L | R | Nada
     deriving (Show,Eq)

-- | largura e altura do boneco, das caixas e das paredes em Int
tBCPi :: Int
tBCPi = 40

-- | largura e altura do boneco, das caixas e das paredes em Float
tBCPf :: Float
tBCPf = 40

-- | função principal que invoca o jogo.
main :: IO ()
main = do
         title <- loadBMP "title.bmp"
         mapas <- loadMapas
         theme <- escolheTema 
         (mcp,lMapas,nMapa) <- escolheNivel mapas
         mapaInicial <- return (defMI mcp nMapa)
         joga mapaInicial (desenhaMapa theme title) (reageF lMapas)

-- | load dos mapas para o jogo
loadMapas :: IO ([String])
loadMapas = do
              mcp1 <- readFile "/home/jcm300/li1g163/TarefaF/Niveis/nivel1"
              mcp2 <- readFile "/home/jcm300/li1g163/TarefaF/Niveis/nivel2"
              mcp3 <- readFile "/home/jcm300/li1g163/TarefaF/Niveis/nivel3"
              mcp4 <- readFile "/home/jcm300/li1g163/TarefaF/Niveis/nivel4"
              mcp5 <- readFile "/home/jcm300/li1g163/TarefaF/Niveis/nivel5"
              return [mcp1,mcp2,mcp3,mcp4,mcp5]

-- | possibilita o jogador escolher o tema
escolheTema :: IO (Picture,Picture,Picture,Picture,Picture)
escolheTema = do
                putStrLn "Select theme (Classic,Mario,Luigi,Zelda,Link): "
                theme <- getLine
                if theme=="Classic" 
                    then do
                           boneco <- loadBMP "classicB.bmp"
                           caixa <- loadBMP "classicC.bmp"
                           parede <- loadBMP "classicP.bmp"
                           pf <- loadBMP "classicPF.bmp"
                           caixaf <- loadBMP "classicCF.bmp"
                           return (boneco,caixa,parede,pf,caixaf)
                    else if theme=="Mario" 
                         then do
                                boneco <- loadBMP "marioB.bmp"
                                caixa <- loadBMP "marioC.bmp"
                                parede <- loadBMP "marioP.bmp"
                                pf <- loadBMP "marioPF.bmp"
                                caixaf <- loadBMP "marioCF.bmp"
                                return (boneco,caixa,parede,pf,caixaf)
                         else if theme=="Luigi"
                              then do
                                     boneco <- loadBMP "luigiB.bmp"
                                     caixa <- loadBMP "marioC.bmp"
                                     parede <- loadBMP "marioP.bmp"
                                     pf <- loadBMP "marioPF.bmp"
                                     caixaf <- loadBMP "marioCF.bmp"
                                     return (boneco,caixa,parede,pf,caixaf)
                              else if theme=="Zelda"
                                   then do
                                          boneco <- loadBMP "ZeldaB.bmp"
                                          caixa <- loadBMP "linkC.bmp"
                                          parede <- loadBMP "linkP.bmp"
                                          pf <- loadBMP "linkPF.bmp"
                                          caixaf <- loadBMP "linkCF.bmp"
                                          return (boneco,caixa,parede,pf,caixaf)
                                   else if theme=="Link"
                                        then do
                                               boneco <- loadBMP "linkB.bmp"
                                               caixa <- loadBMP "linkC.bmp"
                                               parede <- loadBMP "linkP.bmp"
                                               pf <- loadBMP "linkPF.bmp"
                                               caixaf <- loadBMP "linkCF.bmp"
                                               return (boneco,caixa,parede,pf,caixaf)
                                        else escolheTema                             

-- | possibilita o jogador escolher o nível
escolheNivel :: [String] -> IO ((String,[String],Int))
escolheNivel [mcp1,mcp2,mcp3,mcp4,mcp5] = do 
                 putStrLn "Select level (1,2,3,4,5): "
                 nivel <- getLine
                 if nivel=="1" then return (mcp1,mapas,1)
                    else if nivel=="2" then return (mcp2,mapas,2)
                         else if nivel=="3" then return (mcp3,mapas,3)
                              else if nivel=="4" then return (mcp4,mapas,4)
                                   else if nivel=="5" then return (mcp5,mapas,5)
                                        else escolheNivel mapas
                 where mapas = [mcp1,mcp2,mcp3,mcp4,mcp5]

-- | define o mapa inicial e alguns valores necessários
defMI :: String -> Int -> Mapa
defMI mcp nMapa = (tMapa,[posBoneco],[coordsCaixas],0,mSimplificado,removeTab,verificarCI mSimplificado,nMapa)
            where
            mcpL = lines mcp
            (tab,coords) = parteMapa mcpL
            tt = contaListasMapa tab
            (bx,by) = head (tuploCoord coords)
            coordsCaixas = drop 1 (tuploCoord (remInc coords))
            removeTab = removeCardinais tab (daCoords 0 tab 0) tab
            mSimplificado = colocaCaixas removeTab coordsCaixas 
            ts = let (x:xs)=tab
                 in length x
            tMapa = (ts*tBCPi,tt*tBCPi)
            posBoneco = (-(div (fst tMapa) 2)+bx*tBCPi,-(div (snd tMapa) 2)+(by+1)*tBCPi) 

-- | Função que cria um jogo.
joga :: mundo -> (mundo -> Picture) -> (Event -> mundo -> mundo) -> IO ()
joga mapaInicial desenha reage = play
    (InWindow "Sokoban" (1024, 768) (0, 0)) -- Tamanho da janela do jogo
    (white) -- Côr do fundo da janela
    45 -- refresh rate
    mapaInicial -- mapa inicial
    desenha -- função que desenha o mapa
    reage -- função que reage a um evento (carregar numa tecla, mover o rato, etc)
    reageTempo -- função que reage ao passar do tempo

-- | Não reage ao passar do tempo.
reageTempo :: Float -> mundo -> mundo
reageTempo t m = m

-- | Desenha o jogo dentro da janela
desenhaMapa :: (Picture,Picture,Picture,Picture,Picture) -> Picture -> Mapa -> Picture
desenhaMapa (boneco,caixa,parede,pf,caixaf) title ((xMapa,yMapa),((x,y):t),coords,n,ms,semCardTab,fon,nMapa) = Pictures [instrucoes,titulo,made,paredesCPf,figuraBoneco,score,sG]
    where
    -- desenha Titulo do Jogo
    titulo = Translate (((toEnum xMapa)/2)+115) 215 $ Scale (0.25) (0.25) $ title --Translate (((toEnum xMapa)/2)+30) (200) $ Scale (0.50) (0.50) $ Color magenta $ Text "Sokoban" 
    --made by
    made = Color black $ Pictures [Translate (((toEnum xMapa)/2)+32) (150) $ Scale (0.15) (0.15) $ Text "made by:Jose Carlos",Translate (((toEnum xMapa)/2)+120) (130) $ Scale (0.15) (0.15) $ Text "Sergio"]
    -- desenhas as instruçoes
    instrucoes = Pictures [Translate (((toEnum xMapa)/2)+30) 0 $ Color (makeColor 0 0 0 0.2) $ Polygon [(0,-500),(400,-500),(400,500),(0,500)],insmapaPrevious,insmapaNext,insundo,insrestart]
    insmapaPrevious = Color black $ Translate (((toEnum xMapa)/2)+32) (-40) $ Scale (0.14) (0.15) $ Text "'b' : prev level"
    insmapaNext = Color black $ Translate (((toEnum xMapa)/2)+32) (-80) $ Scale (0.14) (0.15) $ Text "'n' : next level"
    insundo = Color black $ Translate (((toEnum xMapa)/2)+32) (-120) $ Scale (0.15) (0.15) $ Text "'u' : undo"
    insrestart = Color black $ Translate (((toEnum xMapa)/2)+32) (-160) $ Scale (0.15) (0.15) $ Text "'r' : restart"
    -- desenha boneco
    figuraBoneco = Translate (toEnum x) (toEnum y) boneco
    -- desenha caixas, paredes e posiçoes finais
    paredesCPf = desenhaTab ms (-((fromIntegral xMapa)/2),(fromIntegral yMapa)/2) (parede,pf,caixa,caixaf)
    -- desenha score
    score = Color red $ Translate (((toEnum xMapa)/2)+40) (0) $ Scale (0.25) (0.25) $ Text ("Moves: " ++ show n)
    -- desenha estado do nivel
    sG = stateGame (xMapa,yMapa) fon

-- | Desenha as caixas, paredes e posiçoes finais
desenhaTab :: [String] -> (Float,Float) -> (Picture,Picture,Picture,Picture) -> Picture
desenhaTab [] _ _ = (Pictures [])
desenhaTab (h:t) (x,y) (parede,pf,caixa,caixaf) = Pictures [desenhaLinha h (x,y) (parede,pf,caixa,caixaf),desenhaTab t (x,y-tBCPf) (parede,pf,caixa,caixaf)]

-- | Desenha uma linha de caixas, paredes e posiçoes finais
desenhaLinha :: String -> (Float,Float) -> (Picture,Picture,Picture,Picture) -> Picture
desenhaLinha [] _ _ = (Pictures [])
desenhaLinha (c:cs) (x,y) (parede,pf,caixa,caixaf)
        |(ord c)==35 = Pictures [Translate x y (parede), desenhaLinha cs (x+tBCPf,y) (parede,pf,caixa,caixaf)]
        |(ord c)==46 = Pictures [Translate x y (pf), desenhaLinha cs (x+tBCPf,y) (parede,pf,caixa,caixaf)]
        |(ord c)==72 = Pictures [Translate x y (caixa), desenhaLinha cs (x+tBCPf,y) (parede,pf,caixa,caixaf)]
        |(ord c)==73 = Pictures [Translate x y (caixaf), desenhaLinha cs (x+tBCPf,y) (parede,pf,caixa,caixaf)]
        |otherwise = desenhaLinha cs (x+tBCPf,y) (parede,pf,caixa,caixaf)

-- | desenha o estado do jogo
stateGame ::(Int,Int) -> Bool -> Picture
stateGame (xMapa,yMapa) fon = if fon==True 
                              then  Color (makeColorI 0 128 0 1) $ Pictures [Translate (((toEnum xMapa)/2)+30) (80) $ Scale (0.15) (0.15) $ Text "LEVEL COMPLETE.", Translate (((toEnum xMapa)/2)+30) (60) $ Scale (0.15) (0.15) $ Text "Try another one ('b','n')."]
                              else  Translate (((toEnum xMapa)/2)+30) (80) $ Scale (0.15) (0.15) $ Color red $ Text "Level incomplete..."

-- | funçao que reage ao 'teclar' do jogador
reageF :: [String] -> Event -> Mapa -> Mapa
reageF lMapas (EventKey (Char 'r') Down _ _) mapaIns = restart mapaIns --restart
reageF lMapas (EventKey (Char 'R') Down _ _) mapaIns = restart mapaIns --restart
reageF lMapas (EventKey (Char 'u') Down _ _) (tMapa,((xBoneco,yBoneco):t),[c],n,ms,tab,fon,nMapa) = (tMapa,((xBoneco,yBoneco):t),[c],n,ms,tab,fon,nMapa) -- undo
reageF lMapas (EventKey (Char 'U') Down _ _) (tMapa,((xBoneco,yBoneco):t),[c],n,ms,tab,fon,nMapa) = (tMapa,((xBoneco,yBoneco):t),[c],n,ms,tab,fon,nMapa) -- undo
reageF lMapas (EventKey (Char 'u') Down _ _) (tMapa,((xBoneco,yBoneco):t),(c:cs),n,ms,tab,fon,nMapa) = undo (tMapa,((xBoneco,yBoneco):t),cs,n,ms,tab,fon,nMapa) -- undo
reageF lMapas (EventKey (Char 'U') Down _ _) (tMapa,((xBoneco,yBoneco):t),(c:cs),n,ms,tab,fon,nMapa) = undo (tMapa,((xBoneco,yBoneco):t),cs,n,ms,tab,fon,nMapa) -- undo
reageF lMapas e (tMapa,((xBoneco,yBoneco):t),(c:cs),n,ms,tab,fon,nMapa)
      |fon==True = mapaIns
      |otherwise = mcpMapa lMapas nMapa (reageEvento e mapaIns) e
      where mapaIns = (tMapa,((xBoneco,yBoneco):t),(c:cs),n,ms,tab,fon,nMapa)

-- | funçao restart que reinicia o nivel
restart :: Mapa -> Mapa
restart (tMapa,((xBoneco,yBoneco):t),[c],n,ms,tab,fon,nMapa) = (tMapa,((xBoneco,yBoneco):t),[c],n,ms,tab,fon,nMapa)
restart (tMapa,((xBoneco,yBoneco):t),(c:cs),n,ms,tab,fon,nMapa) = restart (undo (tMapa,((xBoneco,yBoneco):t),cs,n,ms,tab,fon,nMapa))

-- | funçao undo que volta um passo atrás
undo :: Mapa -> Mapa
undo (tMapa,[(xBoneco,yBoneco)],[coords],n,ms,tab,fon,nMapa) = (tMapa,[(xBoneco,yBoneco)],[coords],n,ms,tab,fon,nMapa)
undo (tMapa,((xBoneco,yBoneco):t),(c:cs),n,ms,tab,fon,nMapa) = (tMapa,t,(c:cs),n-1,colocaCaixas tab c,tab,verificarCI (colocaCaixas tab c),nMapa)

-- | permite passar para o mapa anterior ou seguinte
mcpMapa :: [String] -> Int -> Mapa -> Event -> Mapa
mcpMapa [mcp1,mcp2,mcp3,mcp4,mcp5] nMapa fazmove (EventKey (Char 'n') Down _ _) -- mapa seguinte
      |nMapa==1 = defMI mcp2 2
      |nMapa==2 = defMI mcp3 3
      |nMapa==3 = defMI mcp4 4
      |nMapa==4 = defMI mcp5 5
      |nMapa==5 = defMI mcp1 1
mcpMapa [mcp1,mcp2,mcp3,mcp4,mcp5] nMapa fazmove (EventKey (Char 'b') Down _ _) -- mapa anterior
      |nMapa==1 = defMI mcp5 5
      |nMapa==2 = defMI mcp1 1
      |nMapa==3 = defMI mcp2 2
      |nMapa==4 = defMI mcp3 3
      |nMapa==5 = defMI mcp4 4
mcpMapa _ nMapa fazmove _ = fazmove

-- | Reage ao pressionar das setas do teclado, movendo o boneco 40 pixéis numa direção
reageEvento :: Event -> Mapa -> Mapa
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                |devolveCD1 == '#' = mapaI
                                |devolveCD1 == 'H' || devolveCD1 == 'I' = if(devolveCD2 == '#' || devolveCD2 == 'H' || devolveCD2 == 'I') 
                                                                          then mapaI
                                                                          else (tMapa,moveBD:coordsB,mudaCD:coords,n+1,colocaCaixas tab mudaCD,tab,verificarCI (colocaCaixas tab mudaCD),nMapa)
                                |otherwise = (tMapa,moveBD:coordsB,mudaCD:coords,n+1,colocaCaixas tab mudaCD,tab,verificarCI (colocaCaixas tab mudaCD),nMapa)
                                where
                                ((xBoneco,yBoneco):t) = coordsB
                                devolveCD1 = devolveCaracterB (contalinhasB (reverse ms) ((convY tMapa yBoneco)-1) 0) (convX tMapa xBoneco) 0
                                devolveCD2 = devolveCaracterB (contalinhasB (reverse ms) ((convY tMapa yBoneco)-2) 0) (convX tMapa xBoneco) 0
                                mapaI = (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                moveBD = moveBoneco (0,-tBCPi) (xBoneco,yBoneco)
                                mudaCD = mudaCoordsCaixas (convX tMapa xBoneco) ((convY tMapa yBoneco)-1) (head coords) D
reageEvento (EventKey (SpecialKey KeyUp)  Down _ _) (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                |devolveCU1 == '#' = mapaI
                                |devolveCU1 == 'H' || devolveCU1 == 'I' = if(devolveCU2 == '#' || devolveCU2 == 'H' || devolveCU2 == 'I') 
                                                                          then mapaI
                                                                          else (tMapa,moveBU:coordsB,mudaCU:coords,n+1,colocaCaixas tab mudaCU,tab,verificarCI (colocaCaixas tab mudaCU),nMapa)
                                |otherwise = (tMapa,moveBU:coordsB,mudaCU:coords,n+1,colocaCaixas tab mudaCU,tab,verificarCI (colocaCaixas tab mudaCU),nMapa)
                                where
                                ((xBoneco,yBoneco):t) = coordsB
                                devolveCU1 = devolveCaracterB (contalinhasB (reverse ms) ((convY tMapa yBoneco)+1) 0) (convX tMapa xBoneco) 0
                                devolveCU2 = devolveCaracterB (contalinhasB (reverse ms) ((convY tMapa yBoneco)+2) 0) (convX tMapa xBoneco) 0
                                mapaI = (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                moveBU = moveBoneco (0,tBCPi) (xBoneco,yBoneco)
                                mudaCU = mudaCoordsCaixas (convX tMapa xBoneco) ((convY tMapa yBoneco)+1) (head coords) U
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                |devolveCL1 == '#' = mapaI
                                |devolveCL1 == 'H' || devolveCL1 == 'I' = if(devolveCL2 == '#' || devolveCL2 == 'H' || devolveCL2 == 'I') 
                                                                          then mapaI
                                                                          else (tMapa,moveBL:coordsB,mudaCL:coords,n+1,colocaCaixas tab mudaCL,tab,verificarCI (colocaCaixas tab mudaCL),nMapa)
                                |otherwise = (tMapa,moveBL:coordsB,mudaCL:coords,n+1,colocaCaixas tab mudaCL,tab,verificarCI (colocaCaixas tab mudaCL),nMapa)
                                where
                                ((xBoneco,yBoneco):t) = coordsB
                                devolveCL1 = devolveCaracterB (contalinhasB (reverse ms) (convY tMapa yBoneco) 0) ((convX tMapa xBoneco)-1) 0
                                devolveCL2 = devolveCaracterB (contalinhasB (reverse ms) (convY tMapa yBoneco) 0) ((convX tMapa xBoneco)-2) 0
                                mapaI = (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                moveBL = moveBoneco (-tBCPi,0) (xBoneco,yBoneco)
                                mudaCL = mudaCoordsCaixas ((convX tMapa xBoneco)-1) (convY tMapa yBoneco) (head coords) L
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                |devolveCR1 == '#' = mapaI
                                |devolveCR1 == 'H' || devolveCR1 == 'I' = if(devolveCR2 == '#' || devolveCR2 == 'H' || devolveCR2 == 'I') 
                                                                          then mapaI
                                                                          else (tMapa,moveBR:coordsB,mudaCR:coords,n+1,colocaCaixas tab mudaCR,tab,verificarCI (colocaCaixas tab mudaCR),nMapa)
                                |otherwise = (tMapa,moveBR:coordsB,mudaCR:coords,n+1,colocaCaixas tab mudaCR,tab,verificarCI (colocaCaixas tab mudaCR),nMapa)
                                where
                                ((xBoneco,yBoneco):t) = coordsB
                                devolveCR1 = devolveCaracterB (contalinhasB (reverse ms) (convY tMapa yBoneco) 0) ((convX tMapa xBoneco)+1) 0
                                devolveCR2 = devolveCaracterB (contalinhasB (reverse ms) (convY tMapa yBoneco) 0) ((convX tMapa xBoneco)+2) 0
                                mapaI = (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                moveBR = moveBoneco (tBCPi,0) (xBoneco,yBoneco)
                                mudaCR = mudaCoordsCaixas ((convX tMapa xBoneco)+1) (convY tMapa yBoneco) (head coords) R
reageEvento _ mapa = mapa -- ignora qualquer outro evento

-- | Move o boneco uma coordenada para o lado
moveBoneco :: (Int,Int) -> (Int,Int) -> (Int,Int)
moveBoneco (x,y) (xBoneco,yBoneco) = (x + xBoneco,y + yBoneco)

-- | Converte de Pixeis para coordenada x do mapa em listas de strings
convX :: (Int,Int) -> Int -> Int
convX tMapa x = div (x+(div (fst tMapa) 2)) tBCPi

-- | Converte de Pixeis para coordenada y do mapa em listas de strings
convY :: (Int,Int) -> Int -> Int
convY tMapa y = (div (y+(div (snd tMapa) 2)) tBCPi)-1

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

-- | Converte uma String numa lista de dois números.
sToI :: String -> [Int]
sToI [] = []
sToI (x:xs) = aux (words (x:xs))
          where aux :: [String] -> [Int]
                aux [] = []
                aux (x:xs) = (read x :: Int):aux xs

-- | Recebe uma lista de dois numeros tuplos.
liToT :: [Int] -> (Int,Int)
liToT l = if (null l) then (-1,-1) 
          else (head l,last l)

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
                                |ordCar == 46 = '.'
                                |ordCar == 32 = ' '
                                |ordCarD == 32 || ordCarR == 32 ||  ordCarDR == 32 || ordCarDL == 32 || ordCarL == 32 || ordCarU == 32 || ordCarUR == 32 || ordCarUL == 32 = '#'
                                |ordCarD == 46 || ordCarR == 46 ||  ordCarDR == 46 || ordCarDL == 46 || ordCarL == 46 || ordCarU == 46 || ordCarUR == 46 || ordCarUL == 46 = '#'
                                |otherwise = ' '
                              where 
                                ordCar = ord (devolveCaracterB (contalinhasB tb (snd(head(cd))) 0) (fst(head(cd))) 0)
                                ordCarD = ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))-1) 0) (fst(head(cd))) 0)
                                ordCarR = ord (devolveCaracterB (contalinhasB tb (snd(head(cd))) 0) ((fst(head(cd)))+1) 0)
                                ordCarDR = ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))-1) 0) ((fst(head(cd)))+1) 0)
                                ordCarDL = ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))-1) 0) ((fst(head(cd)))-1) 0)
                                ordCarL = ord (devolveCaracterB (contalinhasB tb (snd(head(cd))) 0) ((fst(head(cd)))-1) 0)
                                ordCarU = ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))+1) 0) (fst(head(cd))) 0)
                                ordCarUR = ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))+1) 0) ((fst(head(cd)))+1) 0)
                                ordCarUL = ord (devolveCaracterB (contalinhasB tb ((snd(head(cd)))+1) 0) ((fst(head(cd)))-1) 0)

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