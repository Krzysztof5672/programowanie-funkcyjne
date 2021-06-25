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
                                  fmap (convertToPicture green) m ++
                                  fmap (convertToPicture yellow) shotM ++ 
                                  fmap (convertToPicture blue) shotP1 ++ 
                                  pointsPicture++
                                  lifesPicture++
                                  recordPicture++
                                  menuPicture++
                                  finalPointsPicture)
    where   starShip = getStarShip gameState 
            monster = getMonster gameState
            m = concat monster
            shotM = getShotM gameState
            shotP1 = getShotP gameState
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
            menuPicture
                  | null monster = 
                                 [  color red $ 
                                        translate (-200) 0 $ 
                                        scale 0.5 0.5 $ 
                                        text "YOU WIN"
                                     ,  color blue $ 
                                        translate (-175) (-50) $ 
                                        scale 0.2 0.2 $ 
                                        text "Your score is "                                          
                                     ,  color blue $ 
                                        translate (-175) (-75) $ 
                                        scale 0.2 0.2 $ 
                                        text "Press ENTER to try again." ] 
                                
                  | isGameOver gameState && getStart gameState =
                                 [  color red $ 
                                        translate (-200) 0 $ 
                                        scale 0.5 0.5 $ 
                                        text "GAME OVER"
                                     ,  color blue $ 
                                        translate (-175) (-75) $ 
                                        scale 0.2 0.2 $ 
                                        text "Press ENTER to try again."
                                     ,  color blue $ 
                                        translate (-175) (-50) $ 
                                        scale 0.2 0.2 $ 
                                        text "Your score is " ]
                  | not (getStart gameState) = 
                                [   color red $ 
                                    translate (-200) 20 $ 
                                    scale 0.4 0.4 $ 
                                    text "Press ENTER to start game"
                                    , color blue $
                                    translate (-200) (-50) $
                                    scale 0.2 0.2 $
                                    text "To move Right click Right Arrow"
                                  , color blue $
                                    translate (-200) (-80) $
                                    scale 0.2 0.2 $
                                    text "To move Left click Left Arrow"
                                  , color blue $
                                    translate (-200) (-110) $
                                    scale 0.2 0.2 $
                                    text "To shot click Up Arrow, every shot cost you 2 points"
                        
                                ] 
                  | otherwise = [] 
            finalPointsPicture 
                            | isGameOver gameState || null monster = 
                                    [color blue $
                                    translate 20 (-50) $ 
                                    scale 0.2 0.2 $ 
                                    text (show point) ]
                            | otherwise = []                
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
                        if gameOver || null monster || not (getStart gameState)
                            then return gameState
                            else return (GameState newStarShip newMonster newshotM newShotP1 newPoints direction newGameOver newlifes newRecord newRandom start1)

    where  
            monster = getMonster gameState
            points = getPoints gameState 
            (newMonster,newRandom) = moveMonster gameState
            direction = NON 
            newShotP1 = take 3 (moveShotP gameState)
            newshotM = moveShotM gameState
            gameOver = isGameOver gameState
            record = getRecord gameState
            start1 = getStart gameState
            GameState newStarShip  _ _ _ _ _ _ newlifes _ _ _= move gameState
            newGameOver = checkGameOver gameState

            newPoints  
                | hitMonster gameState = points+10
                | otherwise = points
            newRecord
                |record<newPoints=newPoints
                |otherwise=record
                
handleKeys::Event->GameState->IO GameState
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) gameState = return (changeDirection gameState LEFT)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = return (changeDirection gameState RIGHT)  
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState = return (shotP gameState ) 

handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) gameState 
    | not (getStart gameState) = return (initialGameState True False record)
    | (isGameOver gameState || null monster) && getStart gameState = return (initialGameState True False record)
    | otherwise = return gameState
    
    where 
        record=getRecord gameState
        monster = getMonster gameState
handleKeys _ gameState = return gameState

main :: IO ()
main =do
    r<-fmap (read::String->Int) (readFile "record.txt") 
    playIO window background 15 (initialGameState False False r) render handleKeys update
