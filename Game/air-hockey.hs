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

-- initialize game
main :: IO ()
main = do
  background_tile <- getDataFileName background_file_path                       -- getDataFileName :: FilePath -> IO FilePath
  let win_config = ((100, 80), (window_width, window_height), window_title)
      img_list = [(background_tile, Nothing)]                                   -- [(FilePath, InvList)] (we don't need InvList)
      game_map = textureMap 0 background_tile_width background_tile_height w h  -- create a PreTextureMap to cover the screen

      -- create object groups (currently empty)
      strikers = objectGroup "strikerGroup" []
      puck = objectGroup "puckGroup" [createPuck]                                    
      
      game_score = Score 0 0                                                    -- initialize game score to 0 for both players
      input_map = []                                                            -- define key mapping to functions
  
  -- initialize game with defined properties above
  funInit win_config game_map [strikers, puck] () game_score input_map gameCycle (Timer fps_delay) img_list


createPuck :: GameObject ()
createPuck = 
  let puck_pic = Basic (Circle puck_radius 0.0 1.0 0.0 Filled)
  in object "puck" puck_pic False (w/2, h/2) (0, 0) ()

-- game loop
gameCycle :: IOGame GameAttribute () () () ()
gameCycle = do
    printOnScreen (show 5) TimesRoman24 (0,0) 1.0 1.0 1.0
    showFPS TimesRoman24 (w-40,0) 1.0 0.0 0.0
