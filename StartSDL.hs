{-# LANGUAGE GADTs #-}
module StartSDL where

import SDL
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Text as T
import Control.Monad (forM_, when)
import Data.Int
import Foreign.Storable

data StartSDLConfig = MkStartSDLConfig
  { _screenW :: Int
  , _screenH :: Int
  , _windowTitle :: String
  , _bootCb :: IO ()
  , _timeCb :: Int -> IO ()
  , _eventCb :: Event -> IO ()
  , _renderCb :: Renderer -> IO ()
  , _sampleRate :: Int
  , _sampleCount :: Int
  , _soundCb :: IO (VU.Vector Int16) }

configDimensions :: Int -> Int -> StartSDLConfig
configDimensions w h = MkStartSDLConfig w h "" noop f f f 44100 samples silence where
  noop = return ()
  f _ = return ()
  samples = 1024
  silence = return (VU.replicate (2*samples) 0)

configWindowTitle x c = c { _windowTitle = x }
configBootCb cb c = c { _bootCb = cb }
configTimeCb cb c = c { _timeCb = cb }
configEventCb cb c = c { _eventCb = cb }
configRenderCb cb c = c { _renderCb = cb }
configSoundCb cb c = c { _soundCb = cb }
configSampleRate x c = c { _sampleRate = x }
configSampleCount x c = c { _sampleCount = x }

startSDL :: StartSDLConfig -> IO a
startSDL (MkStartSDLConfig w h title boot updateBy handleEv render srate bsize getSamples) = go where
  go = do
    initializeAll
    win <- createWindow (T.pack title) defaultWindow
    ren <- createRenderer win (-1) (defaultRenderer { rendererType = AcceleratedVSyncRenderer })
    (dev,_) <- openAudioDevice $ OpenDeviceSpec
      { openDeviceFreq     = Mandate (fromIntegral srate)
      , openDeviceFormat   = Mandate Signed16BitLEAudio
      , openDeviceChannels = Mandate Stereo
      , openDeviceSamples  = fromIntegral bsize
      , openDeviceCallback = audioCallback
      , openDeviceUsage    = ForPlayback
      , openDeviceName     = Nothing }
    setAudioDevicePlaybackState dev Play
    boot
    prev <- ticks
    mainLoop ren prev
  mainLoop ren prev = do
    -- possibly do a time passing
    now <- ticks
    let delta = now - prev
    when (delta > 0) $ do
      updateBy (fromIntegral delta)
    -- handle events
    es <- pollEvents
    forM_ es handleEv
    -- show a picture
    render ren
    present ren
    -- repeat
    mainLoop ren now
  audioCallback :: AudioFormat x -> VSM.IOVector x -> IO ()
  audioCallback Signed16BitLEAudio outVector = do
    samples <- getSamples
    VU.imapM_ (VSM.write outVector) samples

