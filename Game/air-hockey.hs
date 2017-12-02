{- 
=============================
CS312 Project 2 - Air Hockey
=============================

- Written by Harvey, Malinda, Cham

- Uses the FunGEn library 
    http://www.cin.ufpe.br/~haskell/fungen
    Copyright (C) 2001  Andre Furtado <awbf@cin.ufpe.br>

- Written using pong example as a reference
    http://fungen.joyful.com/site/example.html

-}

-- import libraries
module Main where
import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Paths_FunGEn (getDataFileName)

-- window settings
window_width = 400
window_height = 600
w = fromIntegral window_width :: GLdouble
h = fromIntegral window_height :: GLdouble
win_x_pos = 500
win_y_pos = 10
window_title = "Air Hockey"

-- background constants
background_file_path = "Game/ice-tile.bmp"
background_tile_width = 256
background_tile_height = 256

-- game constants
data GameAttribute = Score Int Int -- define algebraic type for Score
fps_delay = 30
puck_radius = 15.0
puck_speed = 7
puck_friction = 0.1

strikerA_radius = 30.0
strikerA_friction = -4

strikerB_radius = 30.0
strikerB_friction = -4

-- initialize game
main :: IO ()
main = do
  background_tile <- getDataFileName background_file_path                       -- getDataFileName :: FilePath -> IO FilePath
  let win_config = ((win_x_pos, win_y_pos), (window_width, window_height), window_title)
      img_list = [(background_tile, Nothing)]                                   -- [(FilePath, InvList)] (we don't need InvList)
      game_map = textureMap 0 background_tile_width background_tile_height w h  -- create a PreTextureMap to cover the screen

      -- create object groups (currently empty)
      strikers = objectGroup "strikerGroup" [createStrikerA, createStrikerB]
      puck = objectGroup "puckGroup" [createPuck]                           
      
      game_score = Score 0 0                                                    -- initialize game score to 0 for both players
      input_map = [(SpecialKey KeyRight, StillDown, moveStrikerARight),
        (SpecialKey KeyLeft, StillDown, moveStrikerALeft),
        (SpecialKey KeyUp, StillDown, moveStrikerAUp),
        (SpecialKey KeyDown, StillDown, moveStrikerADown),
        (Char 'd', StillDown, moveStrikerBRight),
        (Char 'a', StillDown, moveStrikerBLeft),      
        (Char 'w', StillDown, moveStrikerBUp),
        (Char 's', StillDown, moveStrikerBDown)
        ]                                                            -- define key mapping to functions
  
  -- initialize game with defined properties above
  funInit win_config game_map [strikers, puck] () game_score input_map gameCycle (Timer fps_delay) img_list


createStrikerA :: GameObject ()
createStrikerA = 
  let strikerA_pic = Basic (Circle strikerA_radius 1.0 0.0 0.0 Filled)
  in object "strikerA" strikerA_pic False (w/2, 30) (0, 0) ()

createStrikerB :: GameObject ()
createStrikerB = 
  let strikerB_pic = Basic (Circle strikerB_radius 0.0 0.0 1.0 Filled)
  in object "strikerB" strikerB_pic False (w/2, h-30) (0, 0) ()

createPuck :: GameObject ()
createPuck = 
  let puck_pic = Basic (Circle puck_radius 0.0 0.0 0.0 Filled)
  in object "puck" puck_pic False (w/2, h/2) (0, 0) ()

-- moves Striker A to the Right
moveStrikerARight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveStrikerARight _ _ = do
  obj     <- findObject "strikerA" "strikerGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 10 <= w)
   then (setObjectPosition ((pX + (10+strikerA_friction)),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

-- moves Striker A to the left
moveStrikerALeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveStrikerALeft _ _ = do
  obj     <- findObject "strikerA" "strikerGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 10 >= 0)
    then (setObjectPosition ((pX - (10+strikerA_friction)),pY) obj)
    else (setObjectPosition (sX/2,pY) obj)

-- moves Striker A to the Up
moveStrikerAUp :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveStrikerAUp _ _ = do
  obj     <- findObject "strikerA" "strikerGroup"
  (pX,pY) <- getObjectPosition obj
  (_,sY)  <- getObjectSize obj
  if (pY + (sY/2) + 10 <= (h/2))
    then (setObjectPosition (pX,(pY + (10+strikerA_friction))) obj)
    else (setObjectPosition (pX,pY) obj)

-- moves Striker A to the Down
moveStrikerADown :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveStrikerADown _ _ = do
  obj     <- findObject "strikerA" "strikerGroup"
  (pX,pY) <- getObjectPosition obj
  (_,sY)  <- getObjectSize obj
  if (pY - (sY/2) - 10 >= 0)
    then (setObjectPosition (pX,(pY - (10+strikerA_friction))) obj)
    else (setObjectPosition (pX,pY) obj)    


-- moves Striker B to the Right
moveStrikerBRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveStrikerBRight _ _ = do
  obj     <- findObject "strikerB" "strikerGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 10 <= w)
   then (setObjectPosition ((pX + (10+strikerA_friction)),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

-- moves Striker B to the left
moveStrikerBLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveStrikerBLeft _ _ = do
  obj     <- findObject "strikerB" "strikerGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 10 >= 0)
    then (setObjectPosition ((pX - (10+strikerA_friction)),pY) obj)
    else (setObjectPosition (sX/2,pY) obj)
    
-- moves Striker B to the Up
moveStrikerBUp :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveStrikerBUp _ _ = do
  obj     <- findObject "strikerB" "strikerGroup"
  (pX,pY) <- getObjectPosition obj
  (_,sY)  <- getObjectSize obj
  if (pY + (sY/2) + 10 <= h)
    then (setObjectPosition (pX,(pY + (10+strikerA_friction))) obj)
    else (setObjectPosition (pX,pY) obj)

-- moves Striker B to the Down
moveStrikerBDown :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveStrikerBDown _ _ = do
  obj     <- findObject "strikerB" "strikerGroup"
  (pX,pY) <- getObjectPosition obj
  (_,sY)  <- getObjectSize obj
  if (pY - (sY/2) - 10 >= (h/2))
    then (setObjectPosition (pX,(pY - (10+strikerA_friction))) obj)
    else (setObjectPosition (pX,pY) obj)        

-- handle puck interaction with Striker A
puckHitA :: Double -> Double -> Double -> Double -> GameObject() -> IOGame GameAttribute () () () ()
puckHitA sX sY pX pY puck
   | (pX == sX && pY >= sY) = setObjectSpeed (0,puck_speed) puck
   | (pX < sX && pY > sY) = setObjectSpeed (-puck_speed,puck_speed) puck
   | (pX < sX && pY == sY) = setObjectSpeed (-puck_speed,0) puck
   | (pX > sX && pY > sY) = setObjectSpeed (puck_speed,puck_speed) puck
   | (pX > sX && pY == sY) = setObjectSpeed (puck_speed,0) puck
   | (pX == sX && pY <= sY) = setObjectSpeed (0,-puck_speed) puck
   | (pX < sX && pY < sY) = setObjectSpeed (-puck_speed,-puck_speed) puck
   | (pX > sX && pY < sY) = setObjectSpeed (puck_speed,-puck_speed) puck
   | otherwise = setObjectSpeed (0,puck_speed) puck

-- handle puck interaction with Striker B
puckHitB :: Double -> Double -> Double -> Double -> GameObject() -> IOGame GameAttribute () () () ()
puckHitB sX sY pX pY puck
   | (pX == sX && pY <= sY) = setObjectSpeed (0,-puck_speed) puck
   | (pX < sX && pY < sY) = setObjectSpeed (-puck_speed,-puck_speed) puck
   | (pX < sX && pY == sY) = setObjectSpeed (-puck_speed,0) puck
   | (pX > sX && pY < sY) = setObjectSpeed (puck_speed,-puck_speed) puck
   | (pX > sX && pY == sY) = setObjectSpeed (puck_speed,0) puck
   | (pX == sX && pY >= sY) = setObjectSpeed (0,puck_speed) puck
   | (pX < sX && pY > sY) = setObjectSpeed (-puck_speed,puck_speed) puck
   | (pX > sX && pY > sY) = setObjectSpeed (puck_speed,puck_speed) puck
   | otherwise = setObjectSpeed (0,puck_speed) puck

-- handles puck friction
puckFriction :: Double -> Double -> GameObject() -> IOGame GameAttribute () () () ()
puckFriction vX vY puck
   | (vX < 0) = setObjectSpeed (vX+puck_friction, vY) puck
   | (vX > 0) = setObjectSpeed (vX-puck_friction, vY) puck
   | (vY < 0) = setObjectSpeed (vX, vY+puck_friction) puck
   | (vY > 0) = setObjectSpeed (vX, vY-puck_friction) puck
   | otherwise = setObjectSpeed (vX, vY) puck

-- game loop
gameCycle :: IOGame GameAttribute () () () ()
gameCycle = do
    (Score x y) <- getGameAttribute
    printOnScreen (show x) TimesRoman24 (10,10) 1.0 1.0 1.0
    printOnScreen (show y) TimesRoman24 (10,h-30) 1.0 1.0 1.0
    

    strikerA <- findObject "strikerA" "strikerGroup"
    strikerB <- findObject "strikerB" "strikerGroup"
    puck <- findObject "puck" "puckGroup"
    
    col1 <- objectLeftMapCollision puck
    col2 <- objectRightMapCollision puck

    (vX,vY) <- getObjectSpeed puck
    do puckFriction vX vY puck

    when col1 
      (do
        (pX,pY)<-getObjectPosition puck
        setObjectPosition(pX + 5, pY) puck
        (reverseXSpeed puck))

    when col2 
      (do
        (pX,pY)<-getObjectPosition puck
        setObjectPosition(pX - 5, pY) puck
        (reverseXSpeed puck))

    col3 <- objectTopMapCollision puck
    col4 <- objectBottomMapCollision puck
    when col3
       (do
           setGameAttribute (Score (x+1) y)
           setObjectPosition (w/2,h/2) puck
           setObjectSpeed (0,0) puck)
    when col4
       (do
           setGameAttribute (Score x (y+1))
           setObjectPosition (w/2,h/2) puck
           setObjectSpeed (0,0) puck)

    col5 <- objectsCollision puck strikerA
    col6 <- objectsCollision puck strikerB
    when col5 
       (do 
           (sX,sY)<-getObjectPosition strikerA 
           (pX,pY)<-getObjectPosition puck 
           puckHitA sX sY pX pY puck)
    when col6
       (do
           (sX,sY)<-getObjectPosition strikerB 
           (pX,pY)<-getObjectPosition puck 
           puckHitB sX sY pX pY puck)

    showFPS TimesRoman24 (w-40,0) 1.0 0.0 0.0
