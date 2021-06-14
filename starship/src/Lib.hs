module Lib where


import Data.Map as Map
import System.Random
import Control.Monad.State.Strict

data Direction = LEFT | RIGHT | NON deriving (Eq, Ord)
type StarShip = [(Int, Int)]
type Monster = [(Int, Int)]
type ShotM = (Int,Int)
type ShotP = (Int,Int)
type MultiShotP = [ShotP]
type MultiMonster =[Monster]
type MultiShotM = [ShotM]
type St= StateT Direction IO

cols::Int
rows::Int
cols = 50
rows = 33

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = Map.fromList $ zip [ LEFT, RIGHT, NON] 
                                        [(-1, 0), (1, 0), (0,0)]

data GameState = GameState      { getStarShip :: StarShip 
                                , getMonster :: MultiMonster                              
                                , getShotM :: MultiShotM
                                , getShotP :: MultiShotP
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

startMonster :: (Int,Int) -> Monster
startMonster (a,b) = [(a,b),(a+1,b),(a-1,b),(a,b+1),(a,b-1)]

newMonster :: Int -> MultiMonster -> MultiShotP -> MultiMonster
newMonster _ [] _ = []
newMonster a (m:ms) shotP 
    |hitMonsterPom [m] shotP = newMonster a ms shotP
    |otherwise =  [(x+a,y),(x+1+a,y),(x-1+a,y),(x+a,y+1),(x+a,y-1)] : newMonster a ms shotP
    where
        (x,y) = head m

moveMonster :: GameState -> (MultiMonster,StdGen)
moveMonster (GameState _ monster _ shotP _ _ _ _ _ random) = (newMonster r monster shotP, newRandom)
    where 
     {-   (x,y) = head monster
        newX
            | x==1 = 2
            | x==32 = 31
            | otherwise = x + r
       -} (r,newRandom) = randomR (-1,1) random

shotP :: GameState -> GameState
shotP (GameState starShip monster sm sp p dir over lifes record random) = GameState starShip monster sm newsp p dir over lifes record random
    where
        (x,y) = head starShip
        newsp = (x,y-1):sp


newShotP :: Int->Int->ShotP
newShotP x y =(x,y-1)

funPom :: MultiShotP->MultiMonster -> MultiShotP
funPom [] _ = []
funPom ((x,y):sp) m  
    | y==1 = funPom sp m
    | hitMonsterPom m [(x,y)] = funPom sp m
    | otherwise = newShotP x y : funPom sp m
        

moveShotP :: GameState -> MultiShotP
moveShotP (GameState _ monster _ shotP _ _ _ _ _ _) = funPom shotP monster
{-    | shotP == [] = []
    | otherwise = costam
        where 
            costam 
                | y== 1 =[]
                | otherwise = newShotP x y            
            (x,y) = head shotP
-}

newShotM :: Int->Int -> ShotM
newShotM x y = (x,y+1)

moveShotM :: GameState -> MultiShotM
moveShotM (GameState _ [] _ _ _ _ _ _ _ _) = []
moveShotM (GameState _ (m:ms) [] _ _ _ _ _ _ _) = [newShotM x y]
    where 
        (x,y) = head m
moveShotM (GameState starShip (m:ms) ((x,y):xs) sp p dir over lifes record random) =
                                     newShotM newX newY : moveShotM(GameState starShip ms xs sp p dir over lifes record random)
    where
        (xM,yM) = head m
        newY 
            | y>=cols = yM + 1
            | otherwise = y
        newX
            | y>=cols = xM
            | otherwise = x

hitMonsterPom :: MultiMonster -> MultiShotP -> Bool
hitMonsterPom _ [] = False
hitMonsterPom [] _ = False
hitMonsterPom (m:ms) ((xSP,ySP) : xs) = (xM == xSP && (ySP==yM || ySP == yM-1 || ySP==yM+1 ) ) ||
                                    (yM==ySP && (xSP==xM-1 || xSP==xM+1) ) || hitMonsterPom [m] xs || hitMonsterPom ms ((xSP,ySP) : xs) 
        where
            (xM,yM) = head m

hitMonster :: GameState->Bool 
hitMonster (GameState _ monster _ shotP _ _ _ _ _ _) = hitMonsterPom monster shotP
   {- (xM == xSP && (ySP==yM || ySP == yM-1 || ySP==yM+1 ) ) ||
                                                    (yM==ySP && (xSP==xM-1 || xSP==xM+1) ) 
    where 
        (xSP,ySP) = head shotP
        (xM,yM) = head monster
-}

move :: GameState->GameState
move (GameState starShip m sm sp p dir ov lifes re random) =  if wasHitPlayer (GameState starShip m sm sp p dir ov lifes re random)
                then GameState starShip m sm sp p dir ov (lifes-1) re random
                else GameState newstarShip m sm sp p dir ov lifes re random
    where   
        newstarShip = move' (directionVectorMap ! dir) starShip
        

wasHitPlayer :: GameState -> Bool
wasHitPlayer (GameState starShip _ [] _ _ _ _ _ _ _) = False
wasHitPlayer (GameState starShip m ((xS,yS):sx) sp  p dir ov lifes re random) =  y==yS && (x==xS || x-1 == xS || x+1 == xS)
                                                                  ||  wasHitPlayer (GameState starShip m sx sp  p dir ov lifes re random)
    where 
        (x,y) = head starShip
        

checkGameOver :: GameState -> Bool
checkGameOver (GameState starShip monster shotM shotP p dir over lifes record random) = 
                      (wasHitPlayer (GameState starShip monster shotM shotP p dir over lifes record random) && lifes<1)

changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState s m sm sp p _  o l re ran) newDir = GameState s m sm sp p newDir o l re ran

initialGameState :: Bool->Int -> GameState
initialGameState gameOver record= GameState   { getStarShip = [  (starShipX, starShipY), 
                                                        (starShipX, starShipY - 1),
                                                        (starShipX - 1, starShipY ), 
                                                        (starShipX + 1, starShipY)]
                                        , getMonster = [startMonster (monsterX,monsterY),startMonster (monsterX+5,monsterY),startMonster (monsterX-5,monsterY),
                                                        startMonster (monsterX,monsterY+4),startMonster (monsterX+5,monsterY+4),startMonster (monsterX-5,monsterY+4),
                                                        startMonster (monsterX+10,monsterY+2),startMonster (monsterX-10,monsterY+2),startMonster (monsterX-15,monsterY+2)]
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
