{
{-# OPTIONS_GHC -w #-}
module Language.Floorplan.Token (Token(..), scanTokens) where
import Language.Floorplan.Syntax
import qualified Debug.Trace as D
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
  $eol      ;
  $white+   ;
  "//".*    ;
  seq          { \s -> TokenSeq }
  union        { \s -> TokenUnion }
  contains     { \s -> TokenContains }
  bits         { \s -> TokenBits }
  bytes        { \s -> TokenBytes }
  words        { \s -> TokenWords }
  pages        { \s -> TokenPages }
  ptr          { \s -> TokenPtr }
  enum         { \s -> TokenEnum }
  "->"         { \s -> TokenArrow }
  "@("         { \s -> TokenAtParen }
  ")@"         { \s -> TokenParenAt }
  "@|"         { \s -> TokenAtBar }
  "|@"         { \s -> TokenBarAt }
  "<$"         { \s -> TokenLessD }
  "$>"         { \s -> TokenGreaterD }
  \(           { \s -> TokenLParen }
  \)           { \s -> TokenRParen }
  \<           { \s -> TokenLess }
  \>           { \s -> TokenGreater }
  \|           { \s -> TokenBar }
  \|\|         { \s -> TokenBarBar }
  \{           { \s -> TokenLCurl }
  \}           { \s -> TokenRCurl }
  \#           { \s -> TokenPound }
  \:           { \s -> TokenColon }
  \,           { \s -> TokenComma }
  [\+]         { \s -> TokenPlus }
  [\-]         { \s -> TokenMinus }
  [\*]         { \s -> TokenTimes }
  [\/]         { \s -> TokenDiv }
  [\^]         { \s -> TokenExponent }
  "&&"         { \s -> TokenAndAnd }
  "!"          { \s -> TokenBang }
  "%transition" { \s -> TokenTransition }
  "%begin"     { \s -> TokenBeginScope }
  "%end"       { \s -> TokenEndScope }
  "%filterout" { \s -> TokenFilterOut } -- filter out rules
  noglobal     { \s -> TokenNoGlobal }
  \"[^\"]*\"   { \s -> TokenStringLiteral $ tail $ init s }
  $digit+      { \s -> TokenIntNum (read s) }
  0b [01]+     { \s -> TokenBinNum (bin2int s) }
  [a-z] [$alpha $digit \_]* { \s -> TokenLowerID s }
  [A-Z] [$alpha $digit \_]* { \s -> TokenUpperID s}

{
data Token =
    TokenSeq
  | TokenUnion
  | TokenContains
  | TokenBits
  | TokenBytes
  | TokenWords
  | TokenPages
  | TokenPtr
  | TokenEnum
  | TokenArrow
  | TokenAtParen
  | TokenParenAt
  | TokenAtBar
  | TokenBarAt
  | TokenLParen
  | TokenRParen
  | TokenLess
  | TokenGreater
  | TokenLessD
  | TokenGreaterD
  | TokenBar
  | TokenBarBar
  | TokenLCurl
  | TokenRCurl
  | TokenPound
  | TokenColon
  | TokenComma
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenExponent
  | TokenIntNum Int | TokenBinNum Int
  | TokenUpperID String
  | TokenLowerID String
  | TokenBeginScope | TokenEndScope | TokenNoGlobal
  | TokenStringLiteral String | TokenFilterOut
  | TokenHeader String | TokenFooter String | TokenTransition | TokenAndAnd | TokenBang
  deriving (Eq, Ord, Show)

-- | Returns a tuple of all contents before the first instance of '%end' along with
--   the rest of the input string *after* the '%end'.
getUntilEnd :: String -> String -> (String, String)
getUntilEnd _ ('%':'e':'n':'d':rest) = ("", rest)
getUntilEnd kind "" = error $ "Reached end of input while parsing " ++ kind
getUntilEnd kind (c:cs) =
  let (got, rest) = getUntilEnd kind cs
  in  (c : got, rest)

getHeader = getUntilEnd "header"
getFooter = getUntilEnd "footer"

-- | Special case for parsing C-like or Rust-like headers and footers of output.
scanTokens :: String -> [Token]
scanTokens "" = []
scanTokens s =
  case alexScan ('\n',[],s) 0 of
    AlexEOF -> []
    (AlexError e) -> error $ show e -- TODO: bad failure mode
    (AlexSkip (_,_,s') _) -> scanTokens s'
    (AlexToken (_,_,s') len act) ->
      case act (take len s) of
        TokenBeginScope -> checkBegin s'
        tok -> tok : scanTokens s' -- Fall-through case

checkBegin :: String -> [Token]
checkBegin s =
  case alexScan ('\n', [], s) 0 of
    AlexEOF -> []
    (AlexError e) -> error $ show e -- TODO: bad failure mode
    (AlexSkip (_,_,s') _) -> checkBegin s'
    (AlexToken (_,_,s') len act) ->
      case act (take len s) of
        TokenLowerID "header" ->
          let (header, s'') = getHeader s'
          in  TokenHeader header : scanTokens s''
        TokenLowerID "footer" ->
          let (footer, s'') = getFooter s'
          in  TokenFooter footer : scanTokens s''
        -- Fall-through case: it wasn't a header or a footer, so we need
        -- to include the '%begin' scope token that we already parsed in the input.
        tok -> TokenBeginScope : tok : scanTokens s'

}
