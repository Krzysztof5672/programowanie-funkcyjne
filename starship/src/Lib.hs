module Lib where


import Data.Map as Map
import System.Random
import Control.Monad.State.Strict

data Direction = LEFT | RIGHT | NON deriving (Eq, Ord)
type StarShip = [(Int, Int)]
type Monster = [(Int, Int)]
type ShotM = [(Int,Int)]
type ShotP = [(Int,Int)]
type St= StateT Direction IO

cols::Int
rows::Int
cols = 50
rows = 33

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = Map.fromList $ zip [ LEFT, RIGHT, NON] 
                                        [(-1, 0), (1, 0), (0,0)]

data GameState = GameState      { getStarShip :: StarShip 
                                , getMonster :: Monster                              
                                , getShotM :: ShotM
                                , getShotP :: ShotP
                                , getPoints :: Int
                                , getDirection :: Direction
                                , isGameOver :: Bool
                                , getLifes:: Int
                                , getRecord::Int 
                                , getRandom :: StdGen
                                }

move':: (Int,Int)->StarShip->StarShip
move' (_,_) []=[]
move' (a,b) ((c,d):xs) =(a+c,b+d): move' (a,b) xs

newMonster :: (Int,Int) -> Monster
newMonster (a,b) = [(a,b),(a+1,b),(a-1,b),(a,b+1),(a,b-1)]

moveMonster :: GameState -> (Monster,StdGen)
moveMonster (GameState _ monster _ _ _ _ _ _ _ random) = (newMonster (newX ,y), newRandom)
    where 
        (x,y) = head monster
        newX
            | x==1 = 2
            | x==32 = 31
            | otherwise = x + r
        (r,newRandom) = randomR (-1,1) random

shotP :: GameState -> GameState
shotP (GameState starShip monster sm sp p dir over lifes record random) = GameState starShip monster sm newsp p dir over lifes record random
    where
        (x,y) = head starShip
        newsp = [(x,y-1)]


newShotP :: Int->Int->ShotP
newShotP x y =[(x,y-1)]

moveShotP :: GameState -> ShotP
moveShotP (GameState _ _ _ shotP _ _ _ _ _ _) 
    | shotP == [] = []
    | otherwise = costam
        where 
            costam 
                | y== 1 =[]
                | otherwise = newShotP x y            
            (x,y) = head shotP



newShotM :: Int->Int -> ShotM
newShotM x y = [(x,y+1)]

moveShotM :: GameState -> ShotM
moveShotM (GameState _ monster shotM _ _ _ _ _ _ _) = newShotM newX newY 
    where
        (x,y) = head shotM 
        (xM,yM) = head monster
        newY 
            | y>=cols = yM + 1
            | otherwise = y
        newX
            | y>=cols = xM
            | otherwise = x

hitMonster :: GameState->Bool 
hitMonster (GameState _ monster _ shotP _ _ _ _ _ _) = (xM == xSP && (ySP==yM || ySP == yM-1 || ySP==yM+1 ) ) ||
                                                    (yM==ySP && (xSP==xM-1 || xSP==xM+1) ) 
    where 
        (xSP,ySP) = head shotP
        (xM,yM) = head monster


move :: GameState->GameState
move (GameState starShip m sm sp p dir ov lifes re random) =  if wasHit (GameState starShip m sm sp p dir ov lifes re random)
                then GameState starShip m sm sp p dir ov (lifes-1) re random
                else GameState newstarShip m sm sp p dir ov lifes re random
    where   
        newstarShip = move' (directionVectorMap ! dir) starShip
        

wasHit :: GameState -> Bool
wasHit (GameState starShip _ shotM _ _ _ _ _ _ _) =  y==yS && (x==xS || x-1 == xS || x+1 == xS)
    where 
        (x,y) = head starShip
        (xS,yS) = head shotM

checkGameOver :: GameState -> Bool
checkGameOver (GameState starShip monster shotM shotP p dir over lifes record random) =   
                      (wasHit (GameState starShip monster shotM shotP p dir over lifes record random) && lifes<1)


changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState s m sm sp p _  o l re ran) newDir = GameState s m sm sp p newDir o l re ran

initialGameState :: Bool->Int -> GameState
initialGameState gameOver record= GameState   { getStarShip = [  (starShipX, starShipY), 
                                                        (starShipX, starShipY - 1),
                                                        (starShipX - 1, starShipY ), 
                                                        (starShipX + 1, starShipY)]
                                        , getMonster = [(monsterX,monsterY),
                                                        (monsterX,monsterY+1),
                                                        (monsterX,monsterY-1),
                                                        (monsterX+1,monsterY),
                                                        (monsterX-1,monsterY)]
                                        , getShotM =[(monsterX,monsterY+1)]
                                        , getShotP = []
                                        , getPoints =0
                                        , getDirection = NON
                                        , isGameOver = gameOver
                                        , getLifes=3
                                        , getRecord=record
                                        , getRandom = mkStdGen 100 }  
        where   starShipX = cols `div` 2
                starShipY = 28
                monsterX = cols `div` 2
                monsterY = 6

readInts :: String -> Int
readInts  = read 
