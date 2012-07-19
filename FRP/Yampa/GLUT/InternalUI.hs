{-# LANGUAGE BangPatterns #-}

-- Copyright   :  (c) Nikolay Orlyuk 2012
-- License     :  GNU GPLv3 (see COPYING)

module FRP.Yampa.GLUT.InternalUI where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT.Callbacks

data UI = GlutDisplay
        | GlutReshape !Size
        | GlutMotion !Position
        | GlutPassiveMotion !Position
        | GlutKeyboardMouse !Key !KeyState !Modifiers !Position
        | GlutCrossing !Crossing
    deriving (Eq, Show)

