{
{-# OPTIONS_GHC -w #-}
module Language.Floorplan.Token (Token(..), scanTokens) where
import Language.Floorplan.Syntax
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
  $eol      ;
  $white+   ;
  "//".*    ;
  seq       { \s -> TokenSeq }
  union     { \s -> TokenUnion }
  contains  { \s -> TokenContains }
  bits      { \s -> TokenBits }
  bytes     { \s -> TokenBytes }
  words     { \s -> TokenWords }
  pages     { \s -> TokenPages }
  ptr       { \s -> TokenPtr }
  enum      { \s -> TokenEnum }
  "->"      { \s -> TokenArrow }
  "@("      { \s -> TokenAtParen }
  ")@"      { \s -> TokenParenAt }
  "@|"      { \s -> TokenAtBar }
  "|@"      { \s -> TokenBarAt }
  "<$"      { \s -> TokenLessD }
  "$>"      { \s -> TokenGreaterD }
  \(        { \s -> TokenLParen }
  \)        { \s -> TokenRParen }
  \<        { \s -> TokenLess }
  \>        { \s -> TokenGreater }
  \|        { \s -> TokenBar }
  \|\|      { \s -> TokenBarBar }
  \{        { \s -> TokenLCurl }
  \}        { \s -> TokenRCurl }
  \#        { \s -> TokenPound }
  \:        { \s -> TokenColon }
  \,        { \s -> TokenComma }
  [\+]      { \s -> TokenPlus }
  [\-]      { \s -> TokenMinus }
  [\*]      { \s -> TokenTimes }
  [\/]      { \s -> TokenDiv }
  [\^]      { \s -> TokenExponent }
  $digit+   { \s -> TokenNum (read s) }
  0b [01]+  { \s -> TokenNum (bin2int s) }
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
  | TokenNum Int
  | TokenUpperID String
  | TokenLowerID String
  deriving (Eq, Ord, Show)

scanTokens = alexScanTokens
}

