{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                                ;
  "--".*                                 ;
  program                                { \s -> Program }
  "{"                                    { \s -> Begin}
  "}"                                    { \s -> End}
  ";"                                    { \s -> SemiColon}
  int                                    { \s -> Type s}
  =                                      { \s -> Assign}
 "("				                             { \s -> BeginParenthesis}
  ")"				                             { \s -> EndParenthesis}
  if                                     { \s -> If}
  else                                   { \s -> Else}
  print                                  { \s -> Print}
  >                                      { \s -> Greater}
  "<"                                    { \s -> Less}
  ">="                                   { \s -> GreaterOrEqual}
  "<="                                   { \s -> LessOrEqual}
  "=="                                   { \s -> Equal}
  "!="                                   { \s -> Diff}
  "+"                                    { \s -> Sum}
  $digit+                                { \s -> Int (read s)} 
  $alpha [$alpha $digit \_ \']*          { \s -> Id s }
  \" $alpha [$alpha $digit ! \_ \']* \"  { \s -> String s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Program |
  Begin   |
  End     |
  BeginParenthesis |
  EndParenthesis |
  SemiColon |
  Assign    | 
  If  |
  Else |
  Print |
  Greater |
  GreaterOrEqual |
  Less|
  LessOrEqual |
  Equal |
  Diff |
  Sum |
  Type String |
  Id String |
  Int Int |
  String String
  deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
