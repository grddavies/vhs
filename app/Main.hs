module Main where

import App (Tick (..), app, initState)
import qualified Brick.BChan as BC
import Brick.Main (customMain)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import GHC.Conc (forkIO)
import Graphics.Vty (defaultConfig)
import qualified Graphics.Vty.CrossPlatform as V

main :: IO ()
main = do
  eventChan <- BC.newBChan 10

  _ <- forkIO $ forever $ do
    BC.writeBChan eventChan Tick
    -- This is the frame rate is microseconds
    threadDelay 100000
  let buildVty = V.mkVty defaultConfig
  initialVty <- buildVty
  initialState <- initState
  _finalState <-
    customMain
      initialVty
      buildVty
      (Just eventChan)
      app
      initialState
  return ()
