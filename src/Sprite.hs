module Sprite where

import Graphics.Vty (Image, string, vertJoin)
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Image (emptyImage)

heartSprite :: Image
heartSprite = string defAttr "💜"

haskellySprite :: Image
haskellySprite =
  foldl
    vertJoin
    emptyImage
    $ string defAttr
      <$> [ "  _____   _____"
          , "  \\    \\  \\    \\"
          , "   \\    \\  \\    \\"
          , "    \\    \\  \\    \\"
          , "     \\    \\  \\    \\  \\-----------|"
          , "      \\    \\  \\    \\  \\          |"
          , "       \\    \\  \\    \\  \\---------|"
          , "       /    /  /     \\"
          , "      /    /  /       \\  \\-------|"
          , "     /    /  /    ^    \\  \\      |"
          , "    /    /  /    / \\    \\  \\ ----|"
          , "   /    /  /    /   \\    \\"
          , "  /____/  /____/     \\____\\"
          ]
