{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

import Data.Functor

import           Lexer         (scan, showPos)
import qualified Lexer         as L
import           Parser        (parse)
import qualified Parser        as P
import           Data.Either   (either)
import           Control.Monad (mapM)
import           Data.Map      (Map, insert, (!), fromList)
import Control.Lens.TH (makeLenses)
import Control.Lens.Setter ((.~), (%~))
import Control.Lens.Getter ((^.))
import Control.Lens (_1)
import Data.Monoid (mempty, (<>))
import Data.Maybe (fromMaybe)

import qualified Interpreter.Types as I
import Interpreter.AST       as I
import Control.Applicative ((<*>))



type Cb = String -> I.Address

data Context = Context { _labels :: Map String I.Address , _org :: I.Address }
             deriving (Show)
makeLenses ''Context


translate :: [P.Line] -> [I.Instruction]
translate code = a
    where (a,_,b) = translateCode code (Context rtvar 0) 0
--translate code = (^. _1) $ translateCode code (Context rtvar 0) 0
          rtvar = fromList [("CORESIZE", 4000), ("MINDISTANCE", 1), ("MAXLENGTH", 300)]

translateCode :: [P.Line] -> Context -> Address -> ([I.Instruction], Cb, Context)
translateCode (P.Comment     _ : lx) ctx lineNum = translateCode lx ctx lineNum
translateCode (P.Instruction i : lx) ctx lineNum = (i' <> ret, cb, c)
    where eCtx   = handlePseudocode lineNum i cb ctx
          (ret, cb, c) = translateCode code newCtx (succ lineNum)
          (code, newCtx, i') = either continue (lx, ,[]) eCtx
          continue True  = (lx, labels .~ newMap $ ctx, [translateInstruction lineNum i cb])
          continue False = ([], ctx, [])
          newMap         = putLabels lineNum (L.lab <$> P.labelList i) $ ctx ^. labels
translateCode [] ctx _ = ([], cb, ctx)
    where cb = (!) $ ctx ^. labels

handlePseudocode :: Address -> P.Instruction -> Cb -> Context -> Either Bool Context
handlePseudocode _  (P.NoField  _  (P.NoModified P.END)  _) = const $ const $ Left False
handlePseudocode _  (P.NoField  _  (P.Modified P.END _)  _) = const $ const $ Left False
handlePseudocode ln (P.OneField ll op _ a _)                = handleOperation ln ll (P.opcode op) a
handlePseudocode _  _                                       = const $ const $ Left True

handleOperation :: Address -> [L.Token] -> P.Opcode -> P.Expr -> Cb -> Context -> Either Bool Context
handleOperation ln ll P.EQU e cb ctx = Right $ labels %~ putLabels addr (L.lab <$> ll) $ ctx
    where addr      = eval ln e cb
handleOperation ln _  P.ORG e cb ctx = Right $ org .~ eval ln e cb $ ctx
handleOperation _  _  P.END _ _  _   = Left False
handleOperation _  _  _     _ _  _   = Left True

eval :: Address -> P.Expr -> Cb -> I.Address
eval l (P.Term t)  cb = evalTerm l t cb
eval l (P.Sum a b) cb = mod ((        eval l a cb)  +      (eval l b cb)) 4000
eval l (P.Sub a b) cb = mod ((4000 + (eval l a cb)) -      (eval l b cb)) 4000
eval l (P.Mul a b) cb = mod ((        eval l a cb)  *      (eval l b cb)) 4000
eval l (P.Div a b) cb = mod ((        eval l a cb)  `quot` (eval l b cb)) 4000
eval l (P.Mod a b) cb = mod ((        eval l a cb)  `mod`  (eval l b cb)) 4000

evalTerm :: Address -> P.Term -> Cb -> I.Address
evalTerm l (P.Label  t) cb = ((4000 + (cb $ L.lab t)) - l) `mod` 4000
evalTerm _ (P.Number n) _  = n
evalTerm l (P.Par    p) cb = eval l p cb

putLabels :: Address -> [String] -> Map String Address -> Map String Address
putLabels = flip . foldr . flip insert

translateInstruction :: Address -> P.Instruction -> Cb -> I.Instruction
translateInstruction lineNum (P.TwoFields ll op am ae bm be _) cb = I.Instruction opcode modifier aMode aNumber bMode bNumber
    where (opcode, modifier) = translateOperation op am bm
          (aMode   : bMode   : []) = translateMode <$> [am, bm]
          (aNumber : bNumber : []) = eval lineNum  <$> [ae, be] <*> [cb]
translateInstruction lineNum (P.OneField  ll op am ae comment) cb = case P.opcode op of
    P.DAT -> translateInstruction lineNum (P.TwoFields ll op P.Immediate zero am ae comment) cb
    _     -> translateInstruction lineNum (P.TwoFields ll op am ae P.Immediate zero comment) cb
    where zero = P.Term (P.Number 0)

translateOperation :: P.Operation -> P.Mode -> P.Mode -> (I.Opcode, I.Modifier)
translateOperation (P.Modified   opc m) _ _ = (translateOPC opc, translateModifier m)
translateOperation (P.NoModified P.DAT) _ _ = (I.DAT, I.F)
translateOperation (P.NoModified P.JMP) _ _ = (I.JMP, I.B)
translateOperation (P.NoModified P.JMZ) _ _ = (I.JMZ, I.B)
translateOperation (P.NoModified P.JMN) _ _ = (I.JMN, I.B)
translateOperation (P.NoModified P.DJN) _ _ = (I.DJN, I.B)
translateOperation (P.NoModified P.SPL) _ _ = (I.SPL, I.B)
translateOperation (P.NoModified P.SLT) P.Immediate _ = (I.SLT, I.AB)
translateOperation (P.NoModified P.SLT) _ _ = (I.SLT, I.B)
translateOperation (P.NoModified P.MOV) P.Immediate _ = (I.MOV, I.AB)
translateOperation (P.NoModified P.MOV) _ P.Immediate = (I.MOV, I.B )
translateOperation (P.NoModified P.MOV) _ _ = (I.MOV, I.I)
translateOperation (P.NoModified P.CMP) P.Immediate _ = (I.CMP, I.AB)
translateOperation (P.NoModified P.CMP) _ P.Immediate = (I.CMP, I.B )
translateOperation (P.NoModified P.CMP) _ _ = (I.CMP, I.I)
translateOperation (P.NoModified opc  ) a b = translateGroup opc a b
translateOperation (P.NoModified opc  ) a b = translateGroup opc a b
translateOperation (P.NoModified opc  ) a b = translateGroup opc a b
translateOperation (P.NoModified opc  ) a b = translateGroup opc a b
translateOperation (P.NoModified opc  ) a b = translateGroup opc a b

translateGroup :: P.Opcode -> P.Mode -> P.Mode -> (I.Opcode, I.Modifier)
translateGroup opc P.Immediate _ = (translateOPC opc, I.AB)
translateGroup opc _           _ = (translateOPC opc, I.B )

translateOPC :: P.Opcode -> I.Opcode
translateOPC P.DAT = I.DAT
translateOPC P.MOV = I.MOV
translateOPC P.CMP = I.CMP
translateOPC P.ADD = I.ADD
translateOPC P.SUB = I.SUB
translateOPC P.MUL = I.MUL
translateOPC P.DIV = I.DIV
translateOPC P.MOD = I.MOD
translateOPC P.SLT = I.SLT
translateOPC P.JMP = I.JMP
translateOPC P.JMZ = I.JMZ
translateOPC P.JMN = I.JMN
translateOPC P.DJN = I.DJN
translateOPC P.SPL = I.SPL

translateModifier :: P.Modifier -> I.Modifier
translateModifier P.A  = I.A
translateModifier P.B  = I.B
translateModifier P.AB = I.AB
translateModifier P.BA = I.BA
translateModifier P.F  = I.F
translateModifier P.X  = I.X
translateModifier P.I  = I.I

translateMode :: P.Mode -> I.Mode
translateMode P.Immediate       = I.IMMEDIATE
translateMode P.Direct          = I.DIRECT
translateMode P.Indirect        = I.INDIRECT
translateMode P.PredecIndirect  = I.DECREMENT
translateMode P.PostincIndirect = I.INCREMENT
translateMode P.Empty           = I.DIRECT

main :: IO ()
main = do
    --c <- getContents
    --print $ parse $ scan c
    result <- parse . scan <$> getContents
    case result of
        Left  err -> putStr  err
        Right ast -> do
            let a = translate ast
            print  `mapM` a
            print  $ I.start [a]
        --Right ast -> print $ either id id $ I.start [translate ast]
        --Left  err -> putStr `mapM` [err]
        --Right ast -> print  `mapM` translate ast
    ----print `mapM` (translate result)
    putStr ""


