module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

window :: Display
window = InWindow "Haskell StarShip Game" (1000, 600) (100, 100)

background :: Color
background = black

render :: GameState -> IO Picture
render gameState =return (pictures $   [ fillRectangle black (16, 0) (0,0)
                                ] ++
                                  fmap (convertToPicture white) starShip ++ 
                                  pointsPicture++
                                  lifesPicture++
                                  gameOverPicture)
    where   starShip = getStarShip gameState 
            point = getPoints gameState   
            life = getLifes gameState
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
            lifesPicture =     [color red $ 
                                translate (-500) 300 $ 
                                scale 0.25 0.25 $ 
                                text (show life)]
                                                        
update :: Float -> GameState -> IO GameState
update _ gameState =  
                        if gameOver
                            then return gameState
                            else return (GameState newStarShip newPoints direction newGameOver newlifes)
    where   
            direction = NON
            gameOver = isGameOver gameState
            GameState newStarShip  _ _ _ newlifes= move gameState
            newGameOver = checkGameOver gameState
            newPoints =1

                
handleKeys::Event->GameState->IO GameState
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) gameState = return (changeDirection gameState LEFT)
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = return (changeDirection gameState RIGHT)  
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) gameState =    if isGameOver gameState then return (initialGameState False) else return gameState
handleKeys _ gameState = return gameState

main :: IO ()
main =do
    
    playIO window background 10 (initialGameState False) render handleKeys update

