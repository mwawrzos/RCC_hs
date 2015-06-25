{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative ((<|>))
import Control.Lens.Setter
import Control.Lens.Type
import Data.Default
import Data.Functor
import GHC.Generics
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

data Label a = Label { labelVal :: a }
             | Equ   { labelVal :: a }
             deriving Show
instance Functor Label where
    fmap f (Label a) = Label $ f a
    fmap f (Equ   a) = Equ   $ f a


type LabMap a = Map String (Label a)
type ErrInfo a = Either String (PreprocesInfo a)
type PosLabel = (L.AlexPosn, Int)

data PreprocesInfo a = Info { _start :: P.Expr , _labels :: LabMap a , _code :: [P.Instruction] } deriving  (Generic, Show)

start  :: Lens' (PreprocesInfo a) P.Expr
start f (Info start labels code)  = (\start'  -> Info start' labels code) <$> f start
labels :: Lens' (PreprocesInfo a) (LabMap a)
labels f (Info start labels code) = (\labels' -> Info start labels' code) <$> f labels
code   :: Lens' (PreprocesInfo a) [P.Instruction]
code f (Info start labels code)   = (\code'   -> Info start labels code') <$> f code

instance Default (PreprocesInfo a) where
    def = Info (P.Term $ P.Number 0) def def

findLabel :: String -> PreprocesInfo a -> Maybe (Label a)
findLabel key (Info _ labels _) =
    Map.lookup key labels

genAST :: String -> Either String [P.Instruction]
genAST = parse . scan

addLabel :: (PosLabel -> Label PosLabel) -> Int -> L.Token -> ErrInfo PosLabel -> ErrInfo PosLabel
addLabel ctor instrNumber (L.Label pos label) errInfo = do
    info <- errInfo
    case findLabel label info of
        Just posLabel -> fail   $ "Label `" <> label <> "' redefinition [" <> (showPos $ fst $ labelVal posLabel) <> ", " <> showPos pos <> "]"
        _          -> return $ labels %~ Map.insert label (ctor (pos, instrNumber)) $ info


process :: P.Instruction -> (Int, ErrInfo PosLabel) -> (Int, ErrInfo PosLabel)
process instr (instrNumber, info) = do
    case P.opcode $ P.operation instr of
            P.ORG ->                     (instrNumber    , pushLabels Label $ setInfo info $ start .~ P.fstExpr instr     )
            P.END -> case instr of
                P.OneField _ _ _ expr -> (instrNumber    , pushLabels Label $ setInfo info $ (start .~ expr) . (code .~ []))
                _                     -> (instrNumber    , pushLabels Label $ setInfo info $                    code .~ [])
            P.EQU ->                     (instrNumber    , pushLabels Equ   info                                          )
            _     ->                     (instrNumber + 1, pushLabels Label $ setInfo info $ code %~ (instr:)             )
    where
        setInfo :: ErrInfo PosLabel -> (PreprocesInfo PosLabel -> PreprocesInfo PosLabel) -> ErrInfo PosLabel
        setInfo errInfo setter = do
            info <- errInfo
            return $ setter info
        pushLabels :: (PosLabel -> Label PosLabel) -> ErrInfo PosLabel -> ErrInfo PosLabel
        pushLabels ctor newInfo = foldr (addLabel ctor instrNumber) newInfo $ P.labelList instr

preprocessor :: [P.Instruction] -> ErrInfo Int
preprocessor instrs = do
    info <- snd $ foldr process (0, return def) $ instrs
    let newLabels =  (snd <$>) <$> (_labels info)
    return $ Info (_start info) newLabels $ _code info

genCode :: ErrInfo Int -> Either String String
genCode = undefined

compile :: String -> ErrInfo Int-- Either String String
compile = genAST >=> preprocessor -- >=> ap genCode labels


main :: IO ()
main = print . either id show . compile =<< getContents