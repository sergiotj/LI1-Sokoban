{-|
Module : Main
Description : Módulo Haskkel que contém o código relativo à Tarefa E
Copyright : José Carlos <jcm300@live.com.pt>
            Sérgio <stoj97@gmail.com>

Módulo contendo a realização da tarefa E na qual recebe Pictures e devolve
a largura e a altura do menor retangulo envolvendo essas Pictures.
-}

module Main where

import qualified Data.Text as T
import Graphics.Gloss
import GlossExtras
import Data.List
import Test.HUnit hiding (Path)

-- * Funções de teste
-- Dado uma picture devolve largura e altura numa string e arredondado às unidades.
fTeste :: Picture -> String
fTeste input = let (x,y) = tarefa5 input
               in (show (round x) ++ " " ++ show (round y))

tests = TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14]
test1 = TestCase (assertEqual "for Pictures [Circle 20,Pictures [Blank,Translate 10.3 10.3 (Circle 20)]]," "50 50" (fTeste (Pictures [Circle 20,Pictures [Blank,Translate 10.3 10.3 (Circle 20)]]))) 
test2 = TestCase (assertEqual "for Blank," "0 0" (fTeste (Blank)))
test3 = TestCase (assertEqual "for Circle 20," "40 40" (fTeste (Circle 20)))
test4 = TestCase (assertEqual "for Line [(0,10),(20,30),(-12,23)]," "32 20" (fTeste (Line [(0,10),(20,30),(-12,23)])))
test5 = TestCase (assertEqual "for Translate (-20) (40) (Line [(0,10),(20,30),((-12),23)])," "32 20" (fTeste (Translate (-20) (40) (Line [(0,10),(20,30),((-12),23)]))))
test6 = TestCase (assertEqual "for Rotate 90 (Line [(0,10),(20,30),((-12),23)])," "20 32" (fTeste (Rotate 90 (Line [(0,10),(20,30),((-12),23)]))))
test7 = TestCase (assertEqual "for Scale (0.50) 4 (Line [(0,10),(20,30),((-12),23)])," "16 80" (fTeste (Scale (0.50) 4 (Line [(0,10),(20,30),((-12),23)]))))
test8 = TestCase (assertEqual "for Color white (Circle 20)," "40 40" (fTeste (Color white (Circle 20))))
test9 = TestCase (assertEqual "for Rotate 57 (Circle 20)," "40 40" (fTeste (Rotate 57 (Circle 20))))
test10 = TestCase (assertEqual "for Rotate 34 (Scale 23 24 (Translate 32 24 (Color white Blank)))," "0 0" (fTeste (Rotate 34 (Scale 23 24 (Translate 32 24 (Color white Blank))))))
test11 = TestCase (assertEqual "for Pictures [Blank,Circle 30,line [(0,0),(21,10)],Polygon [(0,0),(15,30),(15,0),(0,30)]]," "60 60" (fTeste (Pictures [Blank,Circle 30,line [(0,0),(21,10)],Polygon [(0,0),(15,30),(15,0),(0,30)]])))
test12 = TestCase (assertEqual "for Pictures []," "0 0" (fTeste (Pictures [])))
test13 = TestCase (assertEqual "for Translate 20 23 (Scale 10 13 (Rotate 23 (Color white Blank))))," "0 0" (fTeste (Translate 20 23 (Scale 10 13 (Rotate 23 (Color white Blank)))))) 
test14 = TestCase (assertEqual "for Pictures [ThickCircle 20 20, Arc 10 13 11, ThickArc 33 21 14 23, Text ...]," "0 0" (fTeste (Pictures [ThickCircle 20 20, Arc 10 13 11, ThickArc 33 21 14 23, Text "Teste"])))

-- | Chama a Tarefa 5
main = do inp <- getContents
          let (x,y) = tarefa5 (readPicture inp)
          putStrLn (show (round x) ++ " " ++ show (round y))

-- * Tarefa 5
-- | Tarefa onde devolve a altura e a largura do menor retangulo envolvendo todas as pictures.
tarefa5 :: Picture -> (Float, Float)
tarefa5 pic = larAlt (toPath pic)

-- * Funções da Tarefa 5
-- | Recebe uma lista de pontos e devolve a largura e a altura a partir dos maximos e minimos de x e y
larAlt :: Path -> (Float, Float)
larAlt [] = (0.0,0.0)
larAlt ps = let (x,y) = unzip ps
            in ((maximum x)-(minimum x),(maximum y)-(minimum y))

-- | Transforma uma lista de Pictures em conjunto de pontos
picLA :: [Picture] -> Path
picLA [] = []
picLA [Pictures []] = []
picLA [Pictures pics] = picLA pics
picLA (Blank:pics) =picLA pics
picLA (pic:pics) = (toPath pic)++(picLA pics)

-- | Transforma as diferentes figuras em conjunto de pontos
toPath :: Picture -> Path
toPath Blank = []
toPath (Polygon p) = p
toPath (Line p) = p
toPath (Circle r) = map (\x -> (r*(cos (toRad x)),r*(sin (toRad x)))) [0,5..360]
toPath (Bitmap x y bit b) = [(-(fromIntegral x)/2,-(fromIntegral y)/2),(-(fromIntegral x)/2,(fromIntegral y)/2),((fromIntegral x)/2,-(fromIntegral y)/2),((fromIntegral x)/2,(fromIntegral y)/2)]
toPath (Color c pic) = toPath pic
toPath (Translate x y pic) = translateM x y (toPath pic)
toPath (Rotate x pic) = rotateM x (toPath pic)
toPath (Scale x y pic) = scaleM x y (toPath pic)
toPath (Pictures l) = picLA l
toPath _ = []

-- | Faz translate de uma lista de pontos
translateM :: Float -> Float -> Path -> Path
translateM x y [] = []
translateM x y ((h1,h2):t) = (h1+x,h2+y):translateM x y t  

-- | Faz rotate de uma lista de pontos
rotateM :: Float -> Path -> Path
rotateM x [] = []
rotateM x ((h1,h2):t) =(h1*(cos (toRad x))-h2*(sin (toRad x)),h1*(sin (toRad x))+h2*(cos (toRad x))):rotateM x t

-- | Transforma graus em radianos
toRad :: Float -> Float
toRad x = -(pi*x)/180

-- | Faz scale de uma lista de pontos
scaleM :: Float -> Float -> Path -> Path
scaleM x y [] = []
scaleM x y ((h1,h2):t) = (h1*x,h2*y):scaleM x y t