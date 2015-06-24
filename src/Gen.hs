import Data.Default
import Data.Either (Either(..))
import Data.Map (Map)

import Lexer  hiding (main)
import Parser hiding (main)

type LabelMap = Either String (Map String Int)

labels :: [Instruction] -> LabelMap
labels = foldr addLabels (return def)
    where addLabels :: Instruction -> LabelMap -> LabelMap
          addLabels = const $ const $ Left "asd"

main :: IO ()
main = getContents >>= print . labels . parse . scan