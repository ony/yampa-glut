{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- Copyright   :  (c) Nikolay Orlyuk 2012
-- License     :  GNU GPLv3 (see COPYING)

module FRP.Yampa.GLUT.Adapter
    ( adaptSimple, adapt, simpleInit
    , Action, Reaction
    , actionIO, actionExit
    , module FRP.Yampa.GLUT.UI
    ) where

import Control.Arrow
import Control.Newtype
import Data.IORef
import Data.Monoid

import Graphics.UI.GLUT

import FRP.Yampa (SF, reactInit, react)
import FRP.Yampa.Event

import FRP.Yampa.GLUT.InternalUI
import FRP.Yampa.GLUT.UI

-- | Adapter to connect @FRP.Yampa@ with @Graphics.UI.GLUT@ and does
-- @simpleInit@.
adaptSimple :: String -> IO () -> Reaction -> IO ()
adaptSimple title fini sf = simpleInit title >> adapt fini sf

-- | Adapter to connect @FRP.Yampa@ with @Graphics.UI.GLUT@. Assumes that
-- GLUT have been initialized.
adapt :: IO () -> Reaction -> IO ()
adapt fini sf = do
    timeRef <- newIORef (0 :: Int)

    let rInit = return NoEvent
        rActuate _ _ NoEvent = return False
        rActuate _ _ (Event (Action b)) = b

    rh <- reactInit rInit rActuate sf

    let reactEvent ev = do
            time <- get timeRef
            time' <- get elapsedTime
            let dt = fromIntegral (time' - time) / 1000
            b <- react rh (dt, Just (Event ev))
            if b then fini
                 else writeIORef timeRef time'

    -- set callbacks
    displayCallback $= reactEvent GlutDisplay
    reshapeCallback $= Just (reactEvent . GlutReshape)
    motionCallback $= Just (reactEvent . GlutMotion)
    passiveMotionCallback $= Just (reactEvent . GlutPassiveMotion)
    keyboardMouseCallback $= Just (\k ks m p -> reactEvent (GlutKeyboardMouse k ks m p))
    crossingCallback $= Just (reactEvent . GlutCrossing)

    mainLoop

-- | Simple initialization of GLUT with fixed frame rate 60 fps
simpleInit :: String -> IO ()
simpleInit title = do
    _ <- getArgsAndInitialize
    gameModeCapabilities $= [ Where' GameModeBitsPerPlane IsEqualTo 24 ]
    initialDisplayMode $= [ RGBMode, DoubleBuffered, WithDepthBuffer ]
    
    _ <- createWindow title
    actionOnWindowClose $= MainLoopReturns

    let scheduleTick = do
            let fps = 60
            addTimerCallback (1000 `div` fps) tick

        tick = do
            postRedisplay Nothing
            scheduleTick

    scheduleTick
        
-- | Action to perform in response to something
newtype Action = Action (IO Bool)

-- | Simple IO action that do not control mainLoop life-time
actionIO :: IO () -> Action
actionIO = Action . fmap (const False)

-- | Terminate mainLoop action
actionExit :: Action
actionExit = Action (return True)

-- | Top level reaction signul function
type Reaction = SF (Event UI) (Event Action)

-- Monoid instances to combine actions, reactions etc
instance Newtype Action (IO Bool) where
    pack = Action
    unpack (Action x) = x

instance Monoid Action where
    mempty = Action (return False)
    a `mappend` b = Action (unpack a >>= \x -> if x then return True else unpack b)

instance Monoid a => Monoid (Event a) where
    mempty = Event mempty
    NoEvent `mappend` b' = b'
    Event a `mappend` b' = Event (a `mappend` f b') where
        f NoEvent = mempty
        f (Event b) = b

instance Monoid b => Monoid (SF a b) where
    mempty = arr mempty
    sfX `mappend` sfY = (sfX &&& sfY) >>> arr (uncurry mappend)

