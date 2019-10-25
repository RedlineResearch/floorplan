{
module Language.Floorplan.Parser where

import Language.Floorplan.Syntax
import Language.Floorplan.Token
}

%name layers
%tokentype { Token }
%error { parseError }

%token
    seq       { TokenSeq }
    union     { TokenUnion }
    'contains' { TokenContains }
    bits      { TokenBits }
    bytes     { TokenBytes }
    words     { TokenWords }
    pages     { TokenPages }
    'ptr'     { TokenPtr }
    enum      { TokenEnum }
    '->'  		{ TokenArrow }
		'@('      { TokenAtParen }
		')@'      { TokenParenAt }
		'@|'      { TokenAtBar }
		'|@'      { TokenBarAt }
		'('       { TokenLParen }
		')'       { TokenRParen }
		'<'       { TokenLess }
		'>'       { TokenGreater }
		'<$'      { TokenLessD }
		'$>'      { TokenGreaterD }
		'|'       { TokenBar }
		'||'      { TokenBarBar }
		'{'       { TokenLCurl }
		'}'       { TokenRCurl }
		'#'       { TokenPound }
		':'       { TokenColon }
		','       { TokenComma }
		'+'       { TokenPlus }
		'-'       { TokenMinus }
		'*'       { TokenTimes }
		'/'       { TokenDiv }
		'^'       { TokenExponent }
    literal   { TokenNum $$ }
    UpperID   { TokenUpperID $$ }
    LowerID   { TokenLowerID $$ }

-- %left '+' '-'
-- %left '*' '/'
-- %left '^'
%%

{- ------------------------------------------------------------------------- -}
{- Floorplan layers -}

layers 	: {- empty -}  { [] }
				| layers layer { $2 : $1 }

layerID 	: UpperID { $1 }
fieldID 	: LowerID { $1 }
formalID 	: LowerID { $1 }
flagID   	: UpperID { $1 }

layer : layerSimple         { $1 }
      | '(' layerSimple ')' { $2 }

layerSimple : layerID formals mag align contains  '->' demarc { Layer $1 $2 $3 $4 Nothing $5 $7 }
            | layerID formals align mag contains  '->' demarc { Layer $1 $2 $4 $3 Nothing $5 $7 }
            | layerID formals magAlign contains   '->' demarc { Layer $1 $2 Nothing Nothing $3 $4 $6 }
            | layerID formals mag contains        '->' demarc { Layer $1 $2 $3 Nothing Nothing $4 $6 }
            | layerID formals align contains      '->' demarc { Layer $1 $2 Nothing $3 Nothing $4 $6 }
            | layerID formals contains            '->' demarc { Layer $1 $2 Nothing Nothing Nothing $3 $5 }

mag : '||' sizeArith '||' { Just $2 }

align : '@(' sizeArith ')@' { Just $2 }

magAlign : '@|' sizeArith '|@' { Just $2 }

contains : {- empty -} { [] }
         | contains 'contains' '(' layerID ')' { $4 : $1 }

formals : {- empty -} { [] }
        | '<' formalsInner     '>' { reverse $2 }
        | '<' formalsInner ',' '>' { reverse $2 }

formalsInner  : formalID { [$1] }
              | formalsInner ',' formalID     { $3 : $1 }

{- ------------------------------------------------------------------------- -}

demarcVal : enum   '{' enumExps       '}'  { Enum  (reverse $3) }
          | enum   '{' enumExps  '|'  '}'  { Enum  (reverse $3) }
          | bits   '{' bitsExps       '}'  { Bits  (reverse $3) }
          | bits   '{' bitsExps  ','  '}'  { Bits  (reverse $3) }
          | union  '{' unionExps      '}'  { Union (reverse $3) }
          | union  '{' unionExps '|'  '}'  { Union (reverse $3) }
          | seq    '{' seqExps        '}'  { Seq   (reverse $3) }
          | seq    '{' seqExps   ','  '}'  { Seq   (reverse $3) }
          | sizeArith                 { Blob  $1 }
          | grafting                  { Graft $1 }
          | ptr     { $1 }
          | field   { $1 }
          | layer   { $1 }

seqExps : demarc                  { [$1] }
        | seqExps ',' demarc      { $3 : $1 }

unionExps : demarc                  { [$1] }
          | unionExps '|' demarc      { $3 : $1 }

demarc : '#' demarcVal      { Pound $2 }
       | demarcVal          { $1 }
       | formalID demarcVal { Repetition $1 $2 }

field : fieldID ':' demarc { Field $1 $3 }

ptr : layerID 'ptr' { PtrL $1 }
    | fieldID 'ptr' { PtrF $1 }

grafting  : layerID '<$' args     '$>' { ($1, reverse $3) }
          | layerID '<$' args ',' '$>' { ($1, reverse $3) }
          | layerID                  { ($1, []) }

args : formalID    { [ArgF $1] }
     | literal     { [ArgL $1] }
     | args ',' formalID { (ArgF $3) : $1 }
     | args ',' literal  { (ArgL $3) : $1 }

--     formalOrLiteral              { [$1] }
--     | args ',' formalOrLiteral     { $3 : $1 }
--
--formalOrLiteral : formalID { ArgF $1 }
--                | literal  { ArgL $1 }

enumExps  : flagID                   { [$1] }
          | enumExps '|' flagID      { $3 : $1 }

bitsExps : bitsExp { [$1] }
         | bitsExps ',' bitsExp     { $3 : $1 }

bitsExp  : flagID ':' sizeArith { ($1, $3) }

{- ------------------------------------------------------------------------- -}

sizePrim : prim     { $1 }
         | archPrim { $1 }

prim : bits { Bit }
     | bytes { Byte }

archPrim : words { Word }
         | pages { Page }

litArith : litArith '+' term    { Plus $1 $3 }
         | litArith '-' term    { Minus $1 $3 }
         | term                 { $1 }

term     : term '*' factor      { Times $1 $3 }
         | term '/' factor      { Div $1 $3 }
         | factor               { $1 }

factor   : factor '^' exponent  { Exponent $1 $3 }
         | exponent             { $1 }

exponent : '(' litArith ')'     { $2 }
         |     literal          { Lit $1 }

sizeArith : '(' sizeArith ')'     { $2 }
          | sizeArith '+' termSz  { SizePlus $1 $3 }
          | sizeArith '-' termSz  { SizeMinus $1 $3 }
          | termSz                { $1}

termSz    : litArith  sizePrim { SizeLit (Just $1) $2 }
          |           sizePrim { SizeLit Nothing   $1 }

{- ------------------------------------------------------------------------- -}

{

parseError :: [Token] -> a
parseError xs = error $ "Parse error with remaining input: " ++ show xs

parseLayers :: String -> [Demarc]
parseLayers = reverse . layers . scanTokens
}

