{-# LANGUAGE Arrows #-}
import Control.Arrow
import Data.Monoid

import Graphics.UI.GLUT

import FRP.Yampa.GLUT.Adapter
import FRP.Yampa.Event


main = adaptSimple "Simple" leaveMainLoop simple

simple :: Reaction
simple = proc ev -> do
    displayAction <- arr (tagWith (actionIO display)) <<< redisplay -< ev
    reshapedAction <- arr (fmap (actionIO . reshape)) <<< reshaped -< ev
    returnA -< mconcat [displayAction, reshapedAction]


display = do
    clear [ ColorBuffer, DepthBuffer ]
    
    renderObject Solid (Teapot 1)

    swapBuffers

reshape sz@(Size w h) = do
    let b = fromIntegral (w `min` h) * 2
        w' = fromIntegral w / b
        h' = fromIntegral h / b

    viewport $= (Position 0 0, sz)

    matrixMode $= Projection
    loadIdentity
    -- perspective 45 (w2/h2) 1 1000
    frustum (-w') w' (-h') h' 2 100
    -- ortho2D (-width') width' (-height') height' 

    matrixMode $= Modelview 0
    loadIdentity

    translate (Vector3 0 0 (-4 :: GLfloat))

