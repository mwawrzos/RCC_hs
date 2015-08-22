{
module Parser where

import Data.Maybe (Maybe(..), maybe, maybeToList)
import Control.Monad

import           Lexer (Token, scan)
import qualified Lexer as L
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { Either String }

%left '+' '-'
%left '%' '*' '/'

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

AssemblyFile :: { [Line] }
             : List                                                 { $1 }
             
List         :: { [Line] }
             : Line                                                 { [$1]    }
             | Line List                                            { $1 : $2 }
             
Line         :: { Line }
             : comment                                              { Comment     $1 }
             | Instruction                                          { Instruction $1 }
          

Instruction  :: { Instruction }
             : LabelList Operation Mode Expr               comment  { OneField  $1 $2 $3 $4 $5                  }
             | LabelList END            Expr               comment  { OneField  $1 (NoModified END) Empty $3 $4 }
             | LabelList END                               comment  { NoField   $1 (NoModified END) $3          }
             | LabelList Operation Mode Expr ',' Mode Expr comment  { TwoFields $1 $2 $3 $4 $6 $7 $8            }
             
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
             | Expr '+' Expr                                        { Sum  $1 $3 }
             | Expr '-' Expr                                        { Sub  $1 $3 }
             | Expr '*' Expr                                        { Mul  $1 $3 }
             | Expr '/' Expr                                        { Div  $1 $3 }
             | Expr '%' Expr                                        { Mod  $1 $3 }
             
             
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
parseError :: [Token] -> Either String a
parseError asd = Left $ "parseError " ++ (show (lin, col)) ++  " " ++ (show $ head asd)
    where
        L.AlexPn _ lin col = L.pos $ head asd

data Line        = Instruction Instruction
                 | Comment     String
                 deriving Show

data Instruction = NoField   { labelList :: [L.Token] , operation :: Operation                                                                         , comment :: String }
                 | OneField  { labelList :: [L.Token] , operation :: Operation , fstMode :: Mode , fstExpr :: Expr                                     , comment :: String }
                 | TwoFields { labelList :: [L.Token] , operation :: Operation , fstMode :: Mode , fstExpr :: Expr , sndMode :: Mode , sndExpr :: Expr , comment :: String }
                 deriving Show

data Operation   = Modified   { opcode :: Opcode , modifier :: Modifier }
                 | NoModified { opcode :: Opcode                        }
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
                 | Sum Expr Expr
                 | Sub Expr Expr
                 | Mul Expr Expr
                 | Div Expr Expr
                 | Mod Expr Expr
                 deriving Show
          
data Term        = Label  String
                 | Number Int
                 | Par    Expr
                 deriving Show
}