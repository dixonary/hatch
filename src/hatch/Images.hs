module Images where

import Graphics.Gloss.Game (png)
import qualified Graphics.Gloss as Gloss (blank)
import System.IO.Unsafe
import Paths_hatch
import Layout
import Transforms

loadImage :: String -> Image
loadImage name = scale 0.1 $ Leaf $ png $ unsafePerformIO $ getDataFileName $ name ++ ".png"

cat :: Image
cat = loadImage "cat"

dog :: Image
dog = loadImage "dog"

duck :: Image
duck = loadImage "duck"

goose :: Image
goose = loadImage "goose"


blank :: Image
blank = Leaf $ Gloss.blank
