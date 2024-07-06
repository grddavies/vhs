{-# LANGUAGE TemplateHaskell #-}

module App (AppState (..), initState, app, Tick (..)) where

import qualified Sprite as S

import Brick (App (..), BrickEvent (AppEvent, VtyEvent), EventM, Location (..), ViewportType (Both), continueWithoutRedraw, modify, neverShowCursor, translateBy, viewport, vpSize)
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import Brick.Widgets.Core (raw)
import Graphics.Vty (Image)
import qualified Graphics.Vty as V
import Lens.Micro.Platform (makeLenses, (&), (.~), (^.))

type Coord = (Int, Int)

data AppState = AppState
  { _sprite :: Image
  , _spriteP :: Coord
  , _spriteV :: Coord
  , _viewportSize :: Coord
  }
  deriving (Show)

makeLenses ''AppState

-- Unit time
data Tick = Tick

data Name = VP deriving (Show, Eq, Ord)

drawUI :: AppState -> [Widget Name]
drawUI st = [viewport VP Both $ renderSpriteAt (st ^. sprite) x y]
 where
  (x, y) = _spriteP st

renderSpriteAt :: Image -> Int -> Int -> Widget n
renderSpriteAt img x y = translateBy (Location (x, y)) (raw img)

app :: App AppState Tick Name
app =
  App
    { M.appDraw = drawUI
    , M.appChooseCursor = neverShowCursor
    , M.appHandleEvent = handleEvent
    , M.appStartEvent = return ()
    , M.appAttrMap = const theMap
    }

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

updatePosition :: AppState -> AppState
updatePosition st =
  let (dx, dy) = st ^. spriteV
      (x, y) = st ^. spriteP
      newPos = (x + dx, y + dy)
   in st & spriteP .~ newPos

checkCollision :: Image -> Coord -> Coord -> (Bool, Bool)
checkCollision s (x, y) (vw, vh) = ((x + sw) >= (vw - 1) || x <= 0, (y + sh) >= (vh - 1) || y <= 0)
 where
  sh = V.imageHeight s
  sw = V.imageWidth s

updateVelocity :: AppState -> AppState
updateVelocity st =
  let (x, y) = st ^. spriteV
   in case checkCollision (st ^. sprite) (st ^. spriteP) (st ^. viewportSize) of
        (False, False) -> st
        (True, False) -> st & spriteV .~ (-x, y)
        (False, True) -> st & spriteV .~ (x, -y)
        (True, True) -> st & spriteV .~ (-x, -y)

updateViewport :: Coord -> AppState -> AppState
updateViewport wh = viewportSize .~ wh

handleEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleEvent (AppEvent Tick) = do
  vp <- M.lookupViewport VP
  case vp of
    Just v -> do
      modify $ updatePosition . updateVelocity . updateViewport (v ^. vpSize)
    Nothing -> return ()
handleEvent (VtyEvent (V.EvKey _ _)) = M.halt
handleEvent _ = continueWithoutRedraw

-- Will do random in the future, hence IO
initState :: IO AppState
initState = do
  return AppState{_sprite = S.haskellySprite, _spriteP = (1, 1), _spriteV = (1, 1), _viewportSize = (5, 5)}
