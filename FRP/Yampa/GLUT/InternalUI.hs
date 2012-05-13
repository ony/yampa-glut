
-- Copyright   :  (c) Nikolay Orlyuk 2012
-- License     :  GNU GPLv3 (see COPYING)

module FRP.Yampa.GLUT.InternalUI where

import Graphics.Rendering.OpenGL

data UI = GlutDisplay
        | GlutReshape Size
    deriving (Eq, Show)

