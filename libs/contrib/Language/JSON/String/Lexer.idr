module Language.JSON.String.Lexer

import Data.String.Extra
import Language.JSON.String.Tokens
import Text.Lexer
import Text.Token

%access private
%default total

export
esc : Lexer -> Lexer
esc = escape '\\'

export
quo : Lexer
quo = is '"'

unicodeEscape : Lexer
unicodeEscape = esc $ is 'u' <+> count (exactly 4) hexDigit

simpleEscape : Lexer
simpleEscape = esc $ oneOf "\"\\/bfnrt"

control : Lexer
control = range '\x0000' '\x001f'
      <|> range '\x007f' '\x009f'

legalChar : Lexer
legalChar = non (quo <|> is '\\' <|> control)

jsonStringTokenMap : TokenMap JSONStringToken
jsonStringTokenMap
  = [ (quo, const JSTQuote)
    , (unicodeEscape, JSTUnicodeEscape . unicodeEscapeValue)
    , (simpleEscape, JSTSimpleEscape . simpleEscapeValue)
    , (legalChar, JSTChar . charValue)
    ]

export
lexString : String -> Maybe (List JSONStringToken)
lexString x = case lex jsonStringTokenMap x of
                   (toks, _, _, "") => Just $ map TokenData.tok toks
                   _ => Nothing
