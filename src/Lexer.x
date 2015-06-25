{
module Lexer where

import Control.Monad
}

%wrapper "posn"

$EOL            = \n
$v              = [^$EOL]
@comment        = \;$v* $EOL | $EOL
$numeral        = 0-9      
$alpha          = [a-zA-Z_]
@alphanumeral   = $numeral|$alpha
@whole_number   = $numeral+

tokens :-
    [aA][dD][dD]                { const . ADD                }
    [cC][mM][pP]                { const . CMP                }
    [dD][aA][tT]                { const . DAT                }
    [dD][iI][vV]                { const . DIV                }
    [dD][jJ][nN]                { const . DJN                }
    [eE][nN][dD]                { const . END                }
    [eE][qQ][uU]                { const . EQU                }
    [jJ][mM][nN]                { const . JMN                }
    [jJ][mM][pP]                { const . JMP                }
    [jJ][mM][zZ]                { const . JMZ                }
    [mM][oO][dD]                { const . MOD                }
    [mM][oO][vV]                { const . MOV                }
    [mM][uU][lL]                { const . MUL                }
    [oO][rR][gG]                { const . ORG                }
    [sS][lL][tT]                { const . SLT                }
    [sS][pP][lL]                { const . SPL                }
    [sS][uU][bB]                { const . SUB                }
    \.[aA]                      { const . A                  }
    \.[bB]                      { const . B                  }
    \.[aA][bB]                  { const . AB                 }
    \.[bB][aA]                  { const . BA                 }
    \.[fF]                      { const . F                  }
    \.[xX]                      { const . X                  }
    \.[iI]                      { const . I                  }
    "#"                         { const . Immediate          }
    "$"                         { const . Direct             }
    "@"                         { const . Indirect           }
    "<"                         { const . PredecIndirect     }
    ">"                         { const . PostincIndirect    }
    "+"                         { const . Add                }
    "-"                         { const . Sub                }
    "*"                         { const . Mul                }
    "/"                         { const . Div                }
    "%"                         { const . Mod                }
    "("                         { const . LP                 }
    ")"                         { const . RP                 }
    ","                         { const . COMMA              }
	$alpha @alphanumeral*       { Label                      }
    @whole_number               { readNumber                 }
    @comment                    { \p s -> Comment p $ init s }
    ($white # $EOL)+            ;
    .                           { \p s -> Unknown p $ head s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = EOL
           | Label           { pos :: AlexPosn , lab :: String  }
           | WholeNumber     { pos :: AlexPosn , int :: Int     }
           | Comment         { pos :: AlexPosn , txt :: String  }
           | ADD             { pos :: AlexPosn                  }
           | CMP             { pos :: AlexPosn                  }
           | DAT             { pos :: AlexPosn                  }
           | DIV             { pos :: AlexPosn                  }
           | DJN             { pos :: AlexPosn                  }
           | END             { pos :: AlexPosn                  }
           | EQU             { pos :: AlexPosn                  }
           | JMN             { pos :: AlexPosn                  }
           | JMP             { pos :: AlexPosn                  }
           | JMZ             { pos :: AlexPosn                  }
           | MOD             { pos :: AlexPosn                  }
           | MOV             { pos :: AlexPosn                  }
           | MUL             { pos :: AlexPosn                  }
           | ORG             { pos :: AlexPosn                  }
           | SLT             { pos :: AlexPosn                  }
           | SPL             { pos :: AlexPosn                  }
           | SUB             { pos :: AlexPosn                  }
           | A               { pos :: AlexPosn                  }
           | B               { pos :: AlexPosn                  }
           | AB              { pos :: AlexPosn                  }
           | BA              { pos :: AlexPosn                  }
           | F               { pos :: AlexPosn                  }
           | X               { pos :: AlexPosn                  }
           | I               { pos :: AlexPosn                  }
           | Immediate       { pos :: AlexPosn                  }
           | Direct          { pos :: AlexPosn                  }
           | Indirect        { pos :: AlexPosn                  }
           | PredecIndirect  { pos :: AlexPosn                  }
           | PostincIndirect { pos :: AlexPosn                  }
           | Add             { pos :: AlexPosn                  }
           | Sub             { pos :: AlexPosn                  }
           | Mul             { pos :: AlexPosn                  }
           | Div             { pos :: AlexPosn                  }
           | Mod             { pos :: AlexPosn                  }
           | LP              { pos :: AlexPosn                  }
           | RP              { pos :: AlexPosn                  }
           | DOT             { pos :: AlexPosn                  }
           | COMMA           { pos :: AlexPosn                  }
           | Unknown         { pos :: AlexPosn , char :: Char   }
           | ConstantTooBig  { pos :: AlexPosn , ctb :: Integer }
           deriving (Eq,Show)

showPos :: AlexPosn -> String
showPos (AlexPn _ line col) = show (line, col)

readNumber :: AlexPosn -> String -> Token
readNumber pos number =
    if integer > toInteger (maxBound :: Int)
        then ConstantTooBig pos integer
        else WholeNumber pos $ fromIntegral integer
    where integer = read number :: Integer
           
scan :: String -> [Token]
scan = alexScanTokens . (++ ['\n'])
           
main = do
  s <- getContents
  mapM print $ scan s
}