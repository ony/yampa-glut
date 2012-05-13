module FRP.Yampa.GLUT.InternalUI where

import Graphics.Rendering.OpenGL

data UI = GlutDisplay
        | GlutReshape Size
    deriving (Eq, Show)

