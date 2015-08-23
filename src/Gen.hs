--{-# LANGUAGE DeriveGeneric   #-}
--{-# LANGUAGE TemplateHaskell #-}

--import Control.Applicative ((<|>))
--import Control.Lens.Setter
--import Control.Lens.Type
--import Data.Default
--import Data.Functor
--import Data.Foldable (foldMap)
--import Data.Monoid (Monoid, (<>), mempty, mappend)
--import GHC.Generics
--import           Data.Map (Map)
--import           Data.Monoid ((<>))
--import qualified Data.Map as Map
--import Control.Monad ((>=>), ap, liftM)
--import Control.Monad.Trans.Either (EitherT(..), eitherT, hoistEither)
--import Control.Monad.Trans.Class (lift)

--import           Lexer  (scan, showPos)
--import qualified Lexer  as L
--import           Parser (parse)
--import qualified Parser as P

--data Label a = Label { labelVal :: a }
--             | Equ   { labelVal :: a }
--             deriving Show
--instance Functor Label where
--    fmap f (Label a) = Label $ f a
--    fmap f (Equ   a) = Equ   $ f a


--type LabMap a = Map String (Label a)
--type ErrInfo a = Either String (PreprocesInfo a)
--type PosLabel = (L.AlexPosn, Int)

--data PreprocesInfo a = Info { _start :: P.Expr , _labels :: LabMap a , _code :: [P.Line] } deriving  (Show)

--start  :: Lens' (PreprocesInfo a) P.Expr
--start f (Info start labels code)  = (\start'  -> Info start' labels code) <$> f start
--labels :: Lens' (PreprocesInfo a) (LabMap a)
--labels f (Info start labels code) = (\labels' -> Info start labels' code) <$> f labels
--code   :: Lens' (PreprocesInfo a) [P.Line]
--code f (Info start labels code)   = (\code'   -> Info start labels code') <$> f code

--instance Default (PreprocesInfo a) where
--    def = Info (P.Term $ P.Number 0) def def

--findLabel :: String -> PreprocesInfo a -> Maybe (Label a)
--findLabel key (Info _ labels _) =
--    Map.lookup key labels

--genAST :: String -> Either String [P.Line]
--genAST = parse . scan

--addLabel :: (PosLabel -> Label PosLabel) -> Int -> L.Token -> ErrInfo PosLabel -> ErrInfo PosLabel
--addLabel ctor instrNumber (L.Label pos label) errInfo = do
--    info <- errInfo
--    case findLabel label info of
--        Just posLabel -> fail   $ "Label `" <> label <> "' redefinition [" <> (showPos $ fst $ labelVal posLabel) <> ", " <> showPos pos <> "]"
--        _          -> return $ labels %~ Map.insert label (ctor (pos, instrNumber)) $ info


--process :: P.Line -> (Int, ErrInfo PosLabel) -> (Int, ErrInfo PosLabel)
--process line (instrNumber, info) =
--    case line of
--        P.Instruction instr -> case P.opcode $ P.operation instr of
--            P.ORG ->                       (instrNumber    , pushLabels Label instr $ setInfo info $ start .~ P.fstExpr instr     )
--            P.END -> case instr of
--                P.OneField _ _ _ expr _ -> (instrNumber    , pushLabels Label instr $ setInfo info $ (start .~ expr) . (code .~ []))
--                _                       -> (instrNumber    , pushLabels Label instr $ setInfo info $                    code .~ [])
--            P.EQU ->                       (instrNumber    , pushLabels Equ   instr info                                          )
--            _     ->                       (instrNumber + 1, pushLabels Label instr $ setInfo info $ code %~ (line:)              )
--        _ -> (instrNumber, setInfo info $ code %~ (line:))
--    where
--        setInfo :: ErrInfo PosLabel -> (PreprocesInfo PosLabel -> PreprocesInfo PosLabel) -> ErrInfo PosLabel
--        setInfo errInfo setter = do
--            info <- errInfo
--            return $ setter info
--        pushLabels :: (PosLabel -> Label PosLabel) -> P.Instruction -> ErrInfo PosLabel -> ErrInfo PosLabel
--        pushLabels ctor instr newInfo = foldr (addLabel ctor instrNumber) newInfo $ P.labelList instr

--preprocessor :: [P.Line] -> ErrInfo Int
--preprocessor instrs = do
--    info <- snd $ foldr process (0, return def) $ instrs
--    let newLabels =  (snd <$>) <$> (_labels info)
--    return $ Info (_start info) newLabels $ _code info

----data PreprocesInfo a = Info { _start :: P.Expr , _labels :: LabMap a , _code :: [P.Instruction] } deriving  (Generic, Show)

--instance (Monoid a, Monoid b) => Monoid (Either a b) where
--    mempty = return mempty
--    mappend eelement eacum = do
--        element <- eelement
--        acum    <- eacum
--        return $ element <> acum

--genInstruction :: P.Instruction -> String
---- END terminates red code file
--genInstruction (P.NoField   _ (NoField END)                             comment) = ""
--genInstruction (P.OneField  _ operation fstMode fstExpr                 comment) = (genOperation operation) (genMode fstMode) (genExpr fstExpr)
--genInstruction (P.TwoFields _ operation fstMode fstExpr sndMode sndExpr comment) = (genOperation operation) (genMode fstMode) (genExpr fstExpr) (genMode)

--genCode :: PreprocesInfo Int -> Either String String
--genCode (Info start labels code) = foldMap genInstruction code where
--    genInstruction :: P.Line -> Either String String
--    genInstruction line = case line of
--        P.Comment s     -> return ""
--        P.Instruction i -> return ""

--type LInstr = P.TwoFields
--type LOp    = P.Modified

--translateInstruction :: P.Instruction -> [P.Instruction]

--translateInstruction (P.NoField _ (P.NoModified END)) = []

--translateInstruction (P.OneField)

--translateLine :: P.Line -> [P.Line]

--translateLine (Instruction instr) -> translateInstruction

--translateLine                     -> return

--translate :: PreprocesInfo Int -> [P.Line]
--translate (Info start labels code) = 
--    [P.Instruction $ LInstr [] (LOp P.ORG P.I) P.Direct start P.Direct start ""]
--    <> foldMap translateLine code

--compile :: String -> Either String String
--compile = genAST >=> preprocessor >=> translate

import Data.Functor

import           Lexer  (scan, showPos)
import qualified Lexer  as L
import           Parser (parse)
import qualified Parser as P

import Interpreter.Types

main :: IO ()
main = do
    ast <- parse . scan <$> getContents
    print $ ast