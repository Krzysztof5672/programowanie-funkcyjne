module Lib where


import Data.Map as Map
import Control.Monad.State.Strict

data Direction = LEFT | RIGHT | NON deriving (Eq, Ord)
type StarShip = [(Int, Int)]
type St= StateT Direction IO

cols::Int
rows::Int
cols = 50
rows = 33

directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = Map.fromList $ zip [ LEFT, RIGHT, NON] 
                                        [(-1, 0), (1, 0), (0,0)]

data GameState = GameState      { getStarShip :: StarShip                              
                                , getPoints :: Int
                                , getDirection :: Direction
                                , isGameOver :: Bool
                                , getLifes:: Int
								}

move':: (Int,Int)->StarShip->StarShip
move' (_,_) []=[]
move' (a,b) ((c,d):xs) =(a+c,b+d): move' (a,b) xs

move :: GameState->GameState
move (GameState starShip p dir ov lifes ) =  GameState newstarShip p dir ov lifes
    where   newstarShip = move' (directionVectorMap ! dir) starShip


checkGameOver :: GameState -> Bool
checkGameOver (GameState starShip _ _ _ lifes) =   lifes < 0


changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState s p _  o l) newDir = GameState s p newDir o l

initialGameState :: Bool -> GameState
initialGameState gameOver= GameState   { getStarShip = [  (starShipX, starShipY), 
                                                        (starShipX, starShipY - 1),
                                                        (starShipX - 1, starShipY ), 
                                                        (starShipX + 1, starShipY)]
                                        , getPoints =0
                                        , getDirection = NON
                                        , isGameOver = gameOver
                                        , getLifes=1
										}
        where   starShipX = cols `div` 2
                starShipY = 28
