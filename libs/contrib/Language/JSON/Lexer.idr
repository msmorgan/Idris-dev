module Language.JSON.Lexer

import Language.JSON.String
import Language.JSON.Tokens
import Text.Lexer
import Text.Token

%access private
%default total

numberLit : Lexer
numberLit
  = let sign  = is '-'
        whole = is '0' <|> range '1' '9' <+> many digit
        frac  = is '.' <+> digits
        exp   = like 'e' <+> opt (oneOf "+-") <+> digits in
        opt sign <+> whole <+> opt frac <+> opt exp

jsonTokenMap : TokenMap JSONToken
jsonTokenMap
  = [ (spaces, const JTIgnore)
    , (is ',', const JTComma)
    , (is ':', const JTColon)
    , (is '[', const $ JTSquare Open)
    , (is ']', const $ JTSquare Close)
    , (is '{', const $ JTCurly Open)
    , (is '}', const $ JTCurly Close)
    , (exact "null", const $ JTNull)
    , (exact "true", const $ JTBoolean True)
    , (exact "false", const $ JTBoolean False)
    , (numberLit, JTNumber . cast)
    , (permissiveStringLit, JTString . stringValue)
    ]

export
lexJSON : String -> Maybe (List JSONToken)
lexJSON str
  = case lex jsonTokenMap str of
         (tokens, _, _, "") => Just (map TokenData.tok tokens)
         _ => Nothing
