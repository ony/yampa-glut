
-- Copyright   :  (c) Nikolay Orlyuk 2012
-- License     :  GNU GPLv3 (see COPYING)

module FRP.Yampa.GLUT.UI 
    ( UI
    , redisplay, reshaped, windowSize
    , mousePosition, simpleMousePosition
    , keyAction, mouseButtonAction, modifiers
    , keyPress, keyPressed, mouseButtonPressed
    , crossing
    ) where

import Control.Arrow

import FRP.Yampa (SF, hold)
import FRP.Yampa.Event

import FRP.Yampa.GLUT.InternalUI

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT.Callbacks

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
    f (GlutKeyboardMouse _ _ _ p) = Just p
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


-- | Key action events
keyAction :: SF (Event UI) (Event (KeyState, Either Char SpecialKey))
keyAction = arr (mapFilterE f) where
    f (GlutKeyboardMouse (Char c) ks _ _) = Just (ks, Left c)
    f (GlutKeyboardMouse (SpecialKey k) ks _ _) = Just (ks, Right k)
    f _ = Nothing

-- | Mouse buttons action events
mouseButtonAction :: SF (Event UI) (Event (KeyState, MouseButton))
mouseButtonAction = arr (mapFilterE f) where
    f (GlutKeyboardMouse (MouseButton mb) ks _ _) = Just (ks, mb)
    f _ = Nothing

-- | State of modifiers associated with keyboard/mouse event
modifiers :: SF (Event UI) (Event Modifiers)
modifiers = arr (mapFilterE f) where
    f (GlutKeyboardMouse _ _ m _) = Just m
    f _ = Nothing

-- | Key press events
keyPress :: SF (Event UI) (Event (Either Char SpecialKey))
keyPress = keyAction >>^ fmap snd . filterE ((==Down) . fst)

-- | Key pressed state for specific key
keyPressed :: Either Char SpecialKey -> SF (Event UI) Bool
keyPressed key = hold False <<< mapFilterE f ^<< keyAction where
    f (x, key') | key == key' = Just (x == Down)
    f _ = Nothing

-- | Mouse button pressed state for specific button
mouseButtonPressed :: MouseButton -> SF (Event UI) Bool
mouseButtonPressed button = hold False <<< mapFilterE f ^<< mouseButtonAction where
    f (x, button') | button == button' = Just (x == Down)
    f _ = Nothing

-- | Crossing/leaving event
crossing :: SF (Event UI) (Event Crossing)
crossing = arr (mapFilterE f) where
    f (GlutCrossing c) = Just c
    f _ = Nothing
