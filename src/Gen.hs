import Data.Default
import Data.Functor
import           Data.Map (Map)
import           Data.Monoid ((<>))
import qualified Data.Map as Map
import Control.Monad ((>=>), ap, liftM)
import Control.Monad.Trans.Either (EitherT(..), eitherT, hoistEither)
import Control.Monad.Trans.Class (lift)

import           Lexer  (scan, showPos)
import qualified Lexer  as L
import           Parser (parse)
import qualified Parser as P



type LabelMap a = Either String (Map String a)
type AccMap   = LabelMap (L.AlexPosn, Int)

addLabel :: Int -> L.Token -> AccMap -> AccMap
addLabel instrNumber (L.Label pos label) labelMap = do
    rightMap <- labelMap
    case Map.lookup label rightMap of
        Just (oldPos, _) -> fail   $ "Label `" <> label <> "' redefinition [" <> showPos oldPos <> ", " <> showPos pos <> "]"
        _                -> return $ Map.insert label (pos, instrNumber) rightMap

addLabels :: (Int, AccMap) -> P.Instruction -> (Int, AccMap)
addLabels (instrNumber, labelMap) instr = (instrNumber + 1, newMap) where
    newMap = foldr (addLabel instrNumber) labelMap $ P.labelList instr

labels :: [P.Instruction] -> LabelMap Int
labels instrs = do 
    v <- snd $ foldl addLabels (0, return def) instrs
    return $ snd <$> v


genAST :: String -> Either String [P.Instruction]
genAST = parse . scan

genCode :: [P.Instruction] -> LabelMap Int -> Either String String
genCode = undefined

compile :: String -> Either String String
compile = genAST >=> ap genCode labels


main :: IO ()
main = print . either id show . compile =<< getContents