{-# LANGUAGE Arrows #-}
import Control.Applicative
import Control.Arrow
import Data.Monoid

import Graphics.UI.GLUT

import FRP.Yampa.GLUT.Adapter
import FRP.Yampa (SF, integral )
import FRP.Yampa.Event
import FRP.Yampa.Utilities


main = do
    simpleInit "Simple"
    adapt leaveMainLoop simple

simple :: Reaction
simple = proc ev -> do
    pos <- ball -< ev
    displayAction <- arr (tagWith (actionIO . display)) <<< redisplay -< ev
    reshapedAction <- arr (fmap (actionIO . reshape)) <<< reshaped -< ev
    returnA -< mconcat [fmap (\f -> f pos) displayAction, reshapedAction]


display (x, y) = do
    clear [ ColorBuffer, DepthBuffer ]

    preservingMatrix $ do
        translate (Vector3 (realToFrac x) (realToFrac y) (0 :: GLfloat))
        renderObject Solid (Teapot 0.1)

    swapBuffers

reshape sz@(Size w h) = do
    let b = fromIntegral (w `min` h) * 2
        w' = fromIntegral w / b
        h' = fromIntegral h / b

    viewport $= (Position 0 0, sz)

    matrixMode $= Projection
    loadIdentity
    frustum (-w') w' (-h') h' 2 100

    matrixMode $= Modelview 0
    loadIdentity

    translate (Vector3 0 0 (-4 :: GLfloat))


ball :: SF (Event UI) (Float, Float)
ball = proc ev -> do
    rec
        let speed = (0.05, 0.05)
        position <- integral -< speed
    returnA -< position