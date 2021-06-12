module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

window :: Display
window = InWindow "Haskell StarShip Game" (1100, 700) (100, 100)

background :: Color
background = black

render :: GameState -> IO Picture
render gameState =return (pictures $   [ fillRectangle black (16, 0) (0,0)
                                ] ++
                                  fmap (convertToPicture white) starShip ++ 
                                  fmap (convertToPicture green) monster ++ 
                                  fmap (convertToPicture yellow) shotM ++ 
                                  fmap (convertToPicture blue) shotP ++ 
                                  pointsPicture++
                                  lifesPicture++
                                  recordPicture++
                                  gameOverPicture)
    where   starShip = getStarShip gameState 
            monster = getMonster gameState
            shotM = getShotM gameState
            shotP = getShotP gameState
            point = getPoints gameState   
            life = getLifes gameState
            record=getRecord gameState 
            convertToPicture :: Color -> (Int, Int) -> Picture
            convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) (20, 20)
            fillRectangle color' (tx, ty) (w, h) =  color color' $ 
                                                    scale 1 (-1) $ 
                                                    translate (tx * 20 - 500) (ty * 20 - 300) $ 
                                                    rectangleSolid w h
            toFloat (x, y) = (fromIntegral x, fromIntegral y)
            gameOverPicture =   if isGameOver gameState 
                                then [  color red $ 
                                        translate (-200) 0 $ 
                                        scale 0.5 0.5 $ 
                                        text "GAME OVER"
                                     ,  color blue $ 
                                        translate (-175) (-50) $ 
                                        scale 0.2 0.2 $ 
                                        text "Press ENTER to try again." ] 
                                else []
            pointsPicture=     [color yellow $ 
                                translate 500 300 $ 
                                scale 0.25 0.25 $ 
                                text (show point)]
            recordPicture=     [color yellow $ 
                                translate 500 250 $ 
                                scale 0.25 0.25 $ 
                                text (show record)]
            lifesPicture =     [color red $ 
                                translate (-500) 300 $ 
                                scale 0.25 0.25 $ 
                                text (show life)]
                                                        
update :: Float -> GameState -> IO GameState
update _ gameState =  do
                        writeFile "record.txt" (show record)
                        if gameOver
                            then return gameState
                            else return (GameState newStarShip newMonster newshotM newShotP newPoints direction newGameOver newlifes newRecord newRandom)
    where  
            points = getPoints gameState 
            (newMonster,newRandom) = moveMonster gameState
            direction = NON
            shotP = getShotP gameState 
            newShotP = moveShotP gameState
            newshotM = moveShotM gameState
            gameOver = isGameOver gameState
            record = getRecord gameState
            GameState newStarShip  _ _ _ _ _ _ newlifes _ _= move gameState
            newGameOver = checkGameOver gameState
            hit
              |shotP==[] = False
              |otherwise = hitMonster gameState
             
            newPoints  
                | hit = (points+1)
                | otherwise = points
            newRecord
                |record<newPoints=newPoints
                |otherwise=record
                
handleKeys::Event->GameState->IO GameState
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) gameState = return (changeDirection gameState LEFT)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = return (changeDirection gameState RIGHT)  
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState = return (shotP gameState ) 

handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) gameState =    if isGameOver gameState then return (initialGameState False record) else return gameState
    where record=getRecord gameState
handleKeys _ gameState = return gameState

main :: IO ()
main =do
    r<-fmap (read::String->Int) (readFile "record.txt") 
    playIO window background 10 (initialGameState False r) render handleKeys update
