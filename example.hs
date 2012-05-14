{-# LANGUAGE Arrows #-}
import Control.Arrow
import Data.Monoid

import Data.VectorSpace

import Graphics.UI.GLUT

import FRP.Yampa.GLUT.Adapter
import FRP.Yampa (SF, integral, delay, initially)
import FRP.Yampa.Event
import FRP.Yampa.Utilities


main = do
    simpleInit "Simple"
    adapt leaveMainLoop simple

simple :: Reaction
simple = proc ev -> do
    pos <- ball -< ev
    displayAction <- arr (uncurry tag) <<< first redisplay -< (ev, actionIO . display $ pos)
    reshapedAction <- arr (fmap (actionIO . reshape)) <<< reshaped -< ev
    returnA -< mconcat [displayAction, reshapedAction]

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
            (Vector2 tx ty) <- simpleMousePosition -< ev
            let mpos = (tx, ty)
                dpos = mpos ^-^ pos
                speed = normalized dpos ^* 0.5
            pos <- integral <<< delay 0.2 zeroV -< speed
        returnA -< pos
