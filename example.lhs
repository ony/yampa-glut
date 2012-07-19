Useful syntax for working with @Arrow@ and associated combinators

> {-# LANGUAGE Arrows #-}
> import Control.Arrow

We know that our @Action@ objects have @Monoid@ property. Use that fact by
combining them through @mconcat@

> import Data.Monoid

Nice set of operators for Vectors manipulation

> import Data.VectorSpace
> import Data.VectorSpace.OpenGL

GLUT supplies us with a lot of already predefined objects (among them is teapot and sphere).

> import Graphics.UI.GLUT

Here we get our @Reaction@ processing and @simpleMousePosition@ signal
function.

> import FRP.Yampa.GLUT.Adapter

> import FRP.Yampa (SF, integral, delay, initially, edge, accumHoldBy)
> import FRP.Yampa.Event

Our small Simple game

> main = do
>     simpleInit "Simple"
>     cullFace $= Just Back
>     frontFace $= CW
>     shadeModel $= Smooth
>
>     blend $= Enabled
>     blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
>
>     lighting $= Enabled
>     ambient (Light 0) $= Color4 0.1 0.1 0.1 (1::GLfloat)
>     diffuse (Light 0) $= Color4 0.9 0.9 0.9 (1::GLfloat)
>     position (Light 0) $= Vertex4 0.5 0.5 (-10) 0
>     light (Light 0) $= Enabled
>     colorMaterial $= Just (Front, AmbientAndDiffuse)
>
>     adapt leaveMainLoop simple

Description of events flow through our game

> simple :: Reaction
> simple = proc ev -> do
>     pos <- ball -< ev
>     disp <- redisplay -< ev
>     reshapedAction <- fmap (actionIO . reshape) ^<< reshaped -< ev
>     returnA -< mconcat [
>       reshapedAction,
>       disp `tag` actionIO (display pos)
>       ]

Display takes our in-game state and draws it

> display (x, y) = do
>     clear [ ColorBuffer, DepthBuffer ]
>
>     preservingMatrix $ do
>         translate (Vector3 (realToFrac x) (realToFrac y) (0 :: GLfloat))
>         color $ Color4 0.2 0.8 0 (0.5::GLfloat)
>         renderObject Solid (Sphere' 0.1 50 50)
>
>     swapBuffers

Our reshape callback is trying to keep aspect ratio.

> reshape sz@(Size w h) = do
>     let b = fromIntegral (w `min` h) * 2
>         w' = fromIntegral w / b
>         h' = fromIntegral h / b
>
>     viewport $= (Position 0 0, sz)
>
>     matrixMode $= Projection
>     loadIdentity
>     frustum (-w') w' (-h') h' 2 100
>
>     matrixMode $= Modelview 0
>     loadIdentity
>
>     translate (Vector3 0 0 (-4 :: GLfloat))

Internal state of in-game object represented by two @Float@'s. That's recursive
signal function that affected by user input.

> ball :: SF (Event UI) (Float, Float)
> ball = proc ev -> do
>         d <- moveDirection -< ev
>         (Vector2 tx ty) <- simpleMousePosition -< ev
>         rec
>             let mpos = (tx, ty)
>                 dpos = mpos ^-^ pos
>                 speed = normalized dpos ^* 0.5 ^* d
>             pos <- integral <<< delay 0.2 zeroV -< speed
>         returnA -< pos

Direction

> moveDirection :: SF (Event UI) Float
> moveDirection = mouseButtonPressed LeftButton >>> edge >>> accumHoldBy (const . negate) 1
