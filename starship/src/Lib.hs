module Lib where


import Data.Map as Map
import Control.Monad.State.Strict

data Direction = LEFT | RIGHT | NON deriving (Eq, Ord)
type StarShip = [(Int, Int)]
type Monster = [(Int, Int)]
type St= StateT Direction IO

cols::Int
rows::Int
cols = 70
rows = 33

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = Map.fromList $ zip [ LEFT, RIGHT, NON] 
                                        [(-1, 0), (1, 0), (0,0)]

data GameState = GameState      { getStarShip :: StarShip 
                                , getMonster :: Monster                              
                                , getPoints :: Int
                                , getDirection :: Direction
                                , isGameOver :: Bool
                                , getLifes:: Int
                                , getRecord::Int }

move':: (Int,Int)->StarShip->StarShip
move' (_,_) []=[]
move' (a,b) ((c,d):xs) =(a+c,b+d): move' (a,b) xs

newMonster :: (Int,Int) -> Monster
newMonster (a,b) = [(a,b),(a+1,b),(a-1,b),(a,b+1),(a,b-1)]

move :: GameState->GameState
move (GameState starShip m p dir ov lifes re) =  GameState newstarShip m p dir ov lifes re
    where   newstarShip = move' (directionVectorMap ! dir) starShip


checkGameOver :: GameState -> Bool
checkGameOver (GameState starShip _  _ _ _ lifes _) =  lifes<1

changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState s m p _  o l re) newDir = GameState s m p newDir o l re

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
                                        , getPoints =0
                                        , getDirection = NON
                                        , isGameOver = gameOver
                                        , getLifes=3
                                        , getRecord=record}
        where   starShipX = cols `div` 2
                starShipY = 28
                monsterX = cols `div` 2
                monsterY = 6

readInts :: String -> Int
readInts  = read 
