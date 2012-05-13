
-- Copyright   :  (c) Nikolay Orlyuk 2012
-- License     :  GNU GPLv3 (see COPYING)

module FRP.Yampa.GLUT.UI 
    ( UI
    , redisplay, reshaped
    , mousePosition, simpleMousePosition
    ) where

import Control.Arrow

import FRP.Yampa (SF, hold)
import FRP.Yampa.Event

import FRP.Yampa.GLUT.InternalUI

import Graphics.Rendering.OpenGL

-- | Re-display request from GLUT
redisplay :: SF (Event UI) (Event ())
redisplay = arr $ tagWith () . filterE (GlutDisplay==)

-- | Re-shape request from GLUT
reshaped :: SF (Event UI) (Event Size)
reshaped = arr (mapFilterE f) where
    f (GlutReshape sz) = Just sz
    f _ = Nothing

-- | Window size
windowSize :: SF (Event UI) Size
windowSize = hold (Size 1 1) <<< reshaped

-- | Latest mouse position in window
mousePosition :: SF (Event UI) Position
mousePosition = hold (Position 0 0) <<< arr (mapFilterE f) where
    f (GlutMotion p) = Just p
    f (GlutPassiveMotion p) = Just p
    f _ = Nothing

-- | Latest mouse position in window with simple coord transform (i.e. unit)
simpleMousePosition :: Fractional a => SF (Event UI) (Vector2 a)
simpleMousePosition = windowSize &&& mousePosition >>> arr f where
    f (Size w h, Position x y) = Vector2 x' y' where
        {-
        b = fromIntegral (w `min` h)
        x' = (2 * fromIntegral x - fromIntegral w) / b
        y' = (fromIntegral h - 2 * fromIntegral y) / b
        -}
        b = realToFrac (w `min` h)
        x' = (2 * realToFrac x - realToFrac w) / b
        y' = (realToFrac h - 2 * realToFrac y) / b

{-# SPECIALIZE simpleMousePosition :: SF (Event UI) (Vector2 GLfloat) #-}
{-# SPECIALIZE simpleMousePosition :: SF (Event UI) (Vector2 GLdouble) #-}
{-# SPECIALIZE simpleMousePosition :: SF (Event UI) (Vector2 Float) #-}
{-# SPECIALIZE simpleMousePosition :: SF (Event UI) (Vector2 Double) #-}
