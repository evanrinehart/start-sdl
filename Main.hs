module Main where

import SDL
import StartSDL
import qualified Data.Vector.Unboxed as VU
import System.Exit
import Data.IORef

main = do
  worldVar <- newIORef 0
  let srate = 44100
  let samples = 1024
  startSDL
    . configBootCb (onBoot worldVar)
    . configEventCb (onEvent worldVar)
    . configTimeCb (onTimeDelta worldVar)
    . configRenderCb (onRender worldVar)
    . configSoundCb (audioCb srate samples worldVar)
    . configSampleCount samples
    . configSampleRate srate
    $ configDimensions 640 480

onBoot wv = do
  return ()

onEvent wv ev = case eventPayload ev of
  QuitEvent -> exitSuccess
  other -> do
    print other

onTimeDelta wv dt = do
  return ()

onRender wv ren = do
  clear ren

audioCb srate samples wv = do
  return (VU.replicate (2 * samples) 0)




