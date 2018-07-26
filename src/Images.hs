module Images where

import Graphics.Gloss.Game (png)
import qualified Graphics.Gloss as Gloss (blank)
import System.IO.Unsafe
import Paths_hatch
import Layout

loadImage :: String -> Image
loadImage name = Leaf $ png $ unsafePerformIO $ getDataFileName $ name ++ ".png"

cat :: Image
cat = loadImage "cat"

dog :: Image
dog = loadImage "dog"

blank :: Image
blank = Leaf $ Gloss.blank
