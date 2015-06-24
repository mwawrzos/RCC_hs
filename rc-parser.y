{
module Main (main) where

import Data.Maybe (Maybe(..), maybe, maybeToList)
import Control.Monad

import           Lexer (Token, scan)
import qualified Lexer as L
}

%name parse
%tokentype { Token }
%error { parseError }

%token
        ADD         { L.ADD             _    }
        CMP         { L.CMP             _    }
        DAT         { L.DAT             _    }
        DIV         { L.DIV             _    }
        DJN         { L.DJN             _    }
        END         { L.END             _    }
        EQU         { L.EQU             _    }
        JMN         { L.JMN             _    }
        JMP         { L.JMP             _    }
        JMZ         { L.JMZ             _    }
        MOD         { L.MOD             _    }
        MOV         { L.MOV             _    }
        MUL         { L.MUL             _    }
        ORG         { L.ORG             _    }
        SLT         { L.SLT             _    }
        SPL         { L.SPL             _    }
        SUB         { L.SUB             _    }
        A           { L.A               _    }
        B           { L.B               _    }
        AB          { L.AB              _    }
        BA          { L.BA              _    }
        F           { L.F               _    }
        X           { L.X               _    }
        I           { L.I               _    }
        comment     { L.Comment         _ $$ }
        label       { L.Label           _ _  }
        number      { L.WholeNumber     _ $$ }
        '+'         { L.Add             _    }
        '-'         { L.Sub             _    }
        '*'         { L.Mul             _    }
        '/'         { L.Div             _    }
        '%'         { L.Mod             _    }
        '('         { L.LP              _    }
        ')'         { L.RP              _    }
        '#'         { L.Immediate       _    }
        '$'         { L.Direct          _    }
        '@'         { L.Indirect        _    }
        '<'         { L.PredecIndirect  _    }
        '>'         { L.PostincIndirect _    }
        ','         { L.COMMA           _    }
        
        
%%

AssemblyFile :: { [Instruction] }
             : List                                                 { $1 }
             
List         :: { [Instruction] }
             : Line                                                 { maybeToList $1     }
             | Line List                                            { maybe $2 (: $2) $1 }
             
Line         :: { Maybe Instruction }
             : comment                                              { Nothing }
             | Instruction                                          { Just $1 }
          

Instruction  :: { Instruction }
             : LabelList Operation Mode Expr comment                { OneField  $1 $2 $3 $4       }
             | LabelList END       Mode      comment                { OneField  $1 (NoModified END) $3 (Term $ Number 0) }
             | LabelList Operation Mode Expr ',' Mode Expr comment  { TwoFields $1 $2 $3 $4 $6 $7 }
             
-- LabelList    :: { [PLabel] }
--              : label LabelList                                      { PLabel (L.pos $1) $1 : $2 }
--              | label comment LabelList                              { PLabel (L.pos $1) $1 : $2 }
--              | {- empty -}                                          { []                        }
             
LabelList    :: { [L.Token] }
             : label LabelList                                      { $1 : $2 }
             | label comment LabelList                              { $1 : $3 }
             | {- empty -}                                          { []      }
             
Operation    :: { Operation }
             : Opcode                                               { NoModified $1    }
             | Opcode Modifier                                      { Modified   $1 $2 }
             
Opcode       :: { Opcode }
             : ADD                                                  { ADD }
             | CMP                                                  { CMP }
             | DAT                                                  { DAT }
             | DIV                                                  { DIV }
             | DJN                                                  { DJN }
             | END                                                  { END }
             | EQU                                                  { EQU }
             | JMN                                                  { JMN }
             | JMP                                                  { JMP }
             | JMZ                                                  { JMZ }
             | MOD                                                  { MOD }
             | MOV                                                  { MOV }
             | MUL                                                  { MUL }
             | ORG                                                  { ORG }
             | SLT                                                  { SLT }
             | SPL                                                  { SPL }
             | SUB                                                  { SUB }
               
Modifier     :: { Modifier }
             : A                                                    { A  }
             | B                                                    { B  }   
             | AB                                                   { AB } 
             | BA                                                   { BA }  
             | F                                                    { F  }  
             | X                                                    { X  } 
             | I                                                    { I  } 

Mode         :: { Mode }
             : '#'                                                  { Immediate       }    
             | '$'                                                  { Direct          }    
             | '@'                                                  { Indirect        }    
             | '<'                                                  { PredecIndirect  }    
             | '>'                                                  { PostincIndirect }    
             | {- empty -}                                          { Empty           }

Expr         :: { Expr }
             : Term                                                 { Term $1    }
             | Term '+' Expr                                        { Sum  $1 $3 }
             | Term '-' Expr                                        { Sub  $1 $3 }
             | Term '*' Expr                                        { Mul  $1 $3 }
             | Term '/' Expr                                        { Div  $1 $3 }
             | Term '%' Expr                                        { Mod  $1 $3 }
             
             
Term         :: { Term }
             : label                                                { Label  $ L.lab $1 }
             | Number                                               { Number $1 }
             | '(' Expr ')'                                         { Par    $2 }

Number       :: { Int }
             : number                                               { $1 }
             | SignedNumber                                         { $1 }

SignedNumber :: { Int }
             : '+' number                                           {  $2 }
             | '-' number                                           { -$2 }

{
parseError :: [Token] -> a
parseError asd = error $ "parseError " ++ (show (lin, col)) ++  " " ++ (show $ head asd)
    where
        L.AlexPn _ lin col = L.pos $ head asd

data Instruction = OneField  [L.Token] Operation Mode Expr
                 | TwoFields [L.Token] Operation Mode Expr Mode Expr
                 deriving Show

-- data PLabel       = PLabel L.AlexPosn String deriving Show
                 
data Operation   = Modified   Opcode Modifier
                 | NoModified Opcode
                 deriving Show
  
data Opcode      = ADD
                 | CMP
                 | DAT
                 | DIV
                 | DJN
                 | END
                 | EQU
                 | JMN
                 | JMP
                 | JMZ
                 | MOD
                 | MOV
                 | MUL
                 | ORG
                 | SLT
                 | SPL
                 | SUB
                 deriving Show
  
   
data Modifier    = A  
                 | B 
                 | AB
                 | BA
                 | F 
                 | X 
                 | I
                 deriving Show
   
data Mode        = Immediate      
                 | Direct         
                 | Indirect       
                 | PredecIndirect 
                 | PostincIndirect
                 | Empty    
                 deriving Show
          
data Expr        = Term Term
                 | Sum Term Expr
                 | Sub Term Expr
                 | Mul Term Expr
                 | Div Term Expr
                 | Mod Term Expr
                 deriving Show
          
data Term        = Label  String
                 | Number Int
                 | Par    Expr
                 deriving Show

main = getContents >>= (mapM print) . parse . scan
}