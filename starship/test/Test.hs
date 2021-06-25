module Test where

import Test.QuickCheck

import Lib
import System.Random
import Data.Map

instance Arbitrary Direction where
    arbitrary = elements [LEFT,RIGHT, NON]

instance Arbitrary GameState where
    arbitrary = do 
        starShipX <- chooseInt (2,10)
        monster_X <- chooseInt (6,cols-6)
        monster_Y <- chooseInt (3,16)
        shotM_X <- chooseInt (1,cols-6)
        shotM_Y <- chooseInt(monster_Y,25)
        shotP_X <- chooseInt (1,cols-1)
        shotP_Y <- chooseInt (1, 25)
        direction <- elements [LEFT,RIGHT, NON]
        points <- chooseInt (0,5)
        record <- chooseInt ( 6,10)
        lifes <- chooseInt (1,3)
        gameOver <- elements [True,False]
        random1 <-chooseInt (1,100)
        return (GameState (starShip starShipX) (monster monster_X monster_Y) (shotM shotM_X shotM_Y monster_X monster_Y) (shotP1 shotP_X shotP_Y starShipX)
                                                 points direction gameOver lifes record  (mkStdGen random1) )
        where
            starShip starShipX = [(starShipX,28), (starShipX,27), (starShipX - 1,28), (starShipX + 1,28)]
            monster monster_X monster_Y = [startMonster(monster_X,monster_Y), startMonster(monster_X+5,monster_Y), startMonster(monster_X-5,monster_Y)]
            shotM shotM_X shotM_Y monster_X monster_Y = [newShotM monster_X monster_Y, newShotM shotM_X shotM_Y]
            shotP1 shotP_X shotP_Y starShipX = [(shotP_X,shotP_Y),newShotP starShipX 27]

prop_directionVectorMap :: Direction -> Bool
prop_directionVectorMap LEFT = directionVectorMap ! LEFT == (-1 ,0)
prop_directionVectorMap RIGHT = directionVectorMap ! RIGHT == (1 ,0)
prop_directionVectorMap NON = directionVectorMap ! NON == (0 ,0)

prop_newShotM :: Int-> Int -> Bool
prop_newShotM x y =
    newShotM x y == (x,y+1)

prop_newShotP :: Int-> Int -> Bool
prop_newShotP x y =
    newShotP x y == (x,y-1)

prop_move' :: [(Int,Int)] -> Bool
prop_move' starShip = 
    move' (-1,0) (move' (1,0) starShip ) == starShip

prop_startMonster :: (Int,Int) -> Bool
prop_startMonster (x,y) =
    startMonster (x,y) == [(x,y),(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

prop_changeDirection :: GameState -> Direction -> Bool
prop_changeDirection gameState direction =
    getDirection (changeDirection gameState direction) == direction

prop_shotP :: GameState -> Bool
prop_shotP gameState =
    getShotP (shotP gameState) == (x,y-1):xs && getPoints (shotP gameState) == p-2
    where
      xs = getShotP gameState
      p = getPoints gameState
      (x,y) = head (getStarShip gameState)


prop_wasHitPlayer :: GameState -> Bool
prop_wasHitPlayer gameState = 
    wasHitPlayer gameState == checkHit (getShotM gameState)
    where 
        checkHit :: MultiShotM -> Bool
        checkHit shot = elem (x,y) shot ||  elem (x+1,y) shot ||  elem (x-1,y) shot
        (x,y) = head (getStarShip gameState)
 

prop_gameOver :: GameState -> Bool
prop_gameOver gameState =
    checkGameOver gameState == (checkHit (getShotM gameState) && getLifes gameState <= 1) || isGameOver gameState 
    where 
        checkHit :: MultiShotM -> Bool
        checkHit shot = elem (x,y) shot ||  elem (x+1,y) shot ||  elem (x-1,y) shot
        (x,y) = head (getStarShip gameState)

prop_move :: GameState -> Bool
prop_move (GameState starShip monsters shotM shotP1 points dir over lifes record random1) = 
    getStarShip ( move (GameState starShip monsters shotM shotP1 points dir over lifes record random1) ) == moveTest &&
     getLifes ( move (GameState starShip monsters shotM shotP1 points dir over lifes record random1) ) == lifeTest
     where 
        checkHit :: MultiShotM -> Bool
        checkHit shot = elem (x,y) shot ||  elem (x+1,y) shot ||  elem (x-1,y) shot
        (x,y) = head starShip
        moveTest 
            | checkHit shotM = starShip
            |  x == 2 && dir == LEFT = starShip
            |  x== cols-2 && dir==RIGHT = starShip
            | otherwise =  newstarShip
                where
                    newstarShip = move' (directionVectorMap ! dir) starShip
        lifeTest 
            | checkHit shotM = lifes -1
            | otherwise = lifes

prop_hitMonster :: GameState -> Bool
prop_hitMonster gameState = 
    hitMonster gameState == hitTest (getMonster gameState) (getShotP gameState)
    where 
        hitTest :: MultiMonster -> MultiShotP -> Bool
        hitTest [] _ = False
        hitTest _ [] = False
        hitTest (m:ms) shot = elem (xM,yM) shot || elem (xM+1,yM) shot || elem (xM-1,yM) shot || elem (xM,yM-1) shot || elem (xM,yM+1) shot || hitTest ms shot
            where 
                (xM,yM) = head m

prop_newMonster :: GameState -> Bool
prop_newMonster gamestate = 
    newMonsterX x (getMonster gamestate) (getShotP gamestate) == monsterTest x (getMonster gamestate) (getShotP gamestate)
    where   
        x = fst (randomR (-1,1) (getRandom gamestate) )
        monsterTest _ [] _ = []
        monsterTest a (monster:ms) shot 
            | hitTest [monster] shot = monsterTest a ms shot 
            | otherwise = [(mX+a,mY),(mX+1+a,mY),(mX-1+a,mY),(mX+a,mY+1),(mX+a,mY-1)]:monsterTest a ms shot 
            where 
                (mX,mY) = head monster
        hitTest :: MultiMonster -> MultiShotP -> Bool
        hitTest [] _ = False
        hitTest _ [] = False
        hitTest (m:ms) shot = elem (xM,yM) shot || elem (xM+1,yM) shot || elem (xM-1,yM) shot || elem (xM,yM-1) shot || elem (xM,yM+1) shot || hitTest ms shot
            where 
                (xM,yM) = head m   

test :: IO ()
test =  quickCheck prop_directionVectorMap >> quickCheck prop_move' >> quickCheck prop_startMonster >> quickCheck prop_newShotM >> quickCheck prop_newShotP 
        >> quickCheck prop_changeDirection >> quickCheck prop_shotP >> quickCheck prop_wasHitPlayer >> quickCheck prop_gameOver >> quickCheck prop_move
        >> quickCheck prop_hitMonster >> quickCheck prop_newMonster 
