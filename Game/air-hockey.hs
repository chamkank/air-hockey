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
window_height = 1000
w = fromIntegral window_width :: GLdouble
h = fromIntegral window_height :: GLdouble
window_title = "Air Hockey"

-- background constants
background_file_path = "Game/ice-tile.bmp"
background_tile_width = 256
background_tile_height = 256

-- game constants
data GameAttribute = Score Int -- define algebraic type for Score