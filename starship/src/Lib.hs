module Lib where


import Data.Map as Map hiding (map)
import System.Random
import Control.Monad.State.Strict

data Direction = LEFT | RIGHT | NON deriving (Eq, Ord,Show)
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
cols = 70
rows = 35

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
                                , getStart :: Bool
                                } deriving Show

move':: (Int,Int)->StarShip->StarShip
move' (_,_) []=[]
move' (a,b) ((c,d):xs) =(a+c,b+d): move' (a,b) xs

startMonster :: (Int,Int) -> Monster
startMonster (a,b) = [(a,b),(a+1,b),(a-1,b),(a,b+1),(a,b-1)]

monstersX :: MultiMonster -> [Int]
monstersX  = map (fst . head ) 

monstersY :: MultiMonster -> [Int]
monstersY  = map (snd . head ) 

minMonsterX :: MultiMonster -> Int
minMonsterX monster =  minimum (monstersX monster)

minMonsterY :: MultiMonster -> Int
minMonsterY monster =  minimum (monstersY monster)

maxMonsterX :: MultiMonster -> Int
maxMonsterX monster = maximum (monstersX monster)

maxMonsterY :: MultiMonster -> Int
maxMonsterY monster = maximum (monstersY monster)

newMonsterX :: Int -> MultiMonster -> MultiShotP -> MultiMonster
newMonsterX _ [] _ = []
newMonsterX a (m:ms) playerShot
    |hitMonsterPom [m] playerShot = newMonsterX a ms playerShot
    |otherwise =   map (\(x,y)->(x+a,y)) m: newMonsterX a ms playerShot

newMonsterY :: Int -> MultiMonster -> MultiShotP -> MultiMonster
newMonsterY _ [] _ = []
newMonsterY a (m:ms) playerShot 
    |hitMonsterPom [m] playerShot = newMonsterY a ms playerShot
    |otherwise =   map (\(x,y)->(x,y+a)) m: newMonsterY a ms playerShot


moveMonster :: GameState -> (MultiMonster,StdGen)
moveMonster (GameState _ monster _ shotPlayer _ _ _ _ _ random1 _) 
    |even los = (newMonsterX rNew1 monster shotPlayer, newRandom)
    |otherwise = (newMonsterY rNew2 monster shotPlayer, newRandom)
    where 
        (r,newRandom) = randomR (-1,1) random1
        los = fst (next random1 ) 
        x1 = minMonsterX monster
        x2 = maxMonsterX monster
        y1 = minMonsterY monster
        y2 = maxMonsterY monster
        rNew1
            | x1 == 1 = 1
            | x2 == (cols -1) = -1
            | otherwise = r
        rNew2
            | y1 <= 3= 1
            | y2 >= 16 = -1
            | otherwise = r    
       

shotP :: GameState -> GameState
shotP (GameState starShip monster sm sp p dir over lifes record random1 start)  
    | not over = GameState starShip monster sm newsp (p-2) dir over lifes record random1 start
    | otherwise = GameState starShip monster sm newsp p dir over lifes record random1 start
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
moveShotP (GameState _ monster _ shotPlayer _ _ _ _ _ _ _) = funPom shotPlayer monster


newShotM :: Int->Int -> ShotM
newShotM x y = (x,y+1)

moveShotM :: GameState -> MultiShotM
moveShotM (GameState _ [] _ _ _ _ _ _ _ _ _) = []
moveShotM (GameState _ (m:_) [] _ _ _ _ _ _ _ _) = [newShotM x y]
    where 
        (x,y) = head m
moveShotM (GameState starShip (m:ms) ((x,y):xs) sp p dir over lifes record random1 start) =
                                     newShotM newX newY : moveShotM(GameState starShip ms xs sp p dir over lifes record random1 start)
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
hitMonster (GameState _ monster _ shotPlayer _ _ _ _ _ _ _) = hitMonsterPom monster shotPlayer

move :: GameState->GameState
move (GameState starShip m sm sp p dir ov lifes re random1 start) 
    |   wasHitPlayer (GameState starShip m sm sp p dir ov lifes re random1 start) = GameState starShip m sm sp p dir ov (lifes-1) re random1 start
    |   x == 2 && dir == LEFT = GameState starShip m sm sp p dir ov lifes re random1 start
    |   x== cols-2 && dir==RIGHT = GameState starShip m sm sp p dir ov lifes re random1 start
    |   otherwise = GameState newstarShip m sm sp p dir ov lifes re random1 start
 
        where   
            (x,_) = head starShip
            newstarShip = move' (directionVectorMap ! dir) starShip
            

wasHitPlayer :: GameState -> Bool
wasHitPlayer (GameState _ _ [] _ _ _ _ _ _ _ _) = False
wasHitPlayer (GameState starShip m ((xS,yS):sx) sp  p dir ov lifes re random1 start) =  y==yS && (x==xS || x-1 == xS || x+1 == xS)
                                                                  ||  wasHitPlayer (GameState starShip m sx sp  p dir ov lifes re random1 start)
    where 
        (x,y) = head starShip
        

checkGameOver :: GameState -> Bool
checkGameOver (GameState starShip monster shotM shotPlayer p dir over lifes record random1 start) = wasHitPlayer (GameState starShip monster shotM shotPlayer p dir over lifes record random1 start) && lifes<=1


changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState s m sm sp p _  o l re ran start) newDir = GameState s m sm sp p newDir o l re ran start



initialGameState :: Bool->Bool->Int -> GameState
initialGameState startGame gameOver record= GameState   { getStarShip = [  (starShipX, starShipY), 
                                                        (starShipX, starShipY - 1),
                                                        (starShipX - 1, starShipY ), 
                                                        (starShipX + 1, starShipY)]
                                        , getMonster = [startMonster (monsterX,monsterY),startMonster (monsterX+5,monsterY),startMonster (monsterX-5,monsterY), startMonster (monsterX-10,monsterY),
                                                        startMonster (monsterX+2,monsterY+4),startMonster (monsterX+7,monsterY+4),startMonster (monsterX-3,monsterY+4),startMonster (monsterX-8,monsterY+4),
                                                        startMonster (monsterX+2,monsterY-4),startMonster (monsterX+7,monsterY-4),startMonster (monsterX-3,monsterY-4),startMonster (monsterX-8,monsterY-4),
                                                        startMonster (monsterX,monsterY+8),startMonster (monsterX+5,monsterY+8),startMonster (monsterX-5,monsterY+8), startMonster (monsterX-10,monsterY+8)
                                                        ]
                                        
                                        , getShotM =[(monsterX,monsterY+1)]
                                        , getShotP = []
                                        , getPoints =0
                                        , getDirection = NON
                                        , isGameOver = gameOver
                                        , getLifes=2
                                        , getRecord=record
                                        , getRandom = mkStdGen 100 
                                        , getStart = startGame}  
        where   starShipX = 4
                starShipY = 28
                monsterX = cols `div` 2
                monsterY = 6

