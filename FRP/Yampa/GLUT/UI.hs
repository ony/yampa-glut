module FRP.Yampa.GLUT.UI 
    ( UI
    , redisplay, reshaped
    ) where

import Control.Arrow

import FRP.Yampa (SF)
import FRP.Yampa.Event

import FRP.Yampa.GLUT.InternalUI

import Graphics.Rendering.OpenGL

-- | Re-display request from GLUT
redisplay :: SF (Event UI) (Event ())
redisplay = arr $ tagWith () . filterE (GlutDisplay==)

-- | Re-shape request from GLUT
reshaped :: SF (Event UI) (Event Size)
reshaped = arr $ mapFilterE f where
    f (GlutReshape sz) = Just sz
    f _ = Nothing

