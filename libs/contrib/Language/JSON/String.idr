module Language.JSON.String

import Data.String.Extra
import Text.Lexer
import Text.Parser

%access private
%default total

public export
data JSONStringToken
  = JSTQuote
  | JSTSimpleEscape Char
  | JSTUnicodeEscape Char
  | JSTChar Char

charValue : String -> Char
charValue x = case index 0 x of
                   Nothing => '\NUL'
                   Just c  => c

simpleEscapeValue : String -> Char
simpleEscapeValue x
  = case index 1 x of
         Nothing => '\NUL'
         Just c => case c of
                        '"'  => '"'
                        '\\' => '\\'
                        '/'  => '/'
                        'b'  => '\b'
                        'f'  => '\f'
                        'n'  => '\n'
                        'r'  => '\r'
                        't'  => '\t'
                        _    => '\NUL'

unicodeEscapeValue : String -> Char
unicodeEscapeValue x = chr (cast $ "0x" ++ drop 2 x)

esc : Lexer -> Lexer
esc = escape '\\'

quo : Lexer
quo = is '"'

unicodeEscape : Lexer
unicodeEscape = esc $ is 'u' <+> count (exactly 4) hexDigit

simpleEscape : Lexer
simpleEscape = esc $ oneOf "\"\\/bfnrt"

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

stringChar : Grammar JSONStringToken True Char
stringChar = terminal $
  \t => case t of
             JSTChar c => Just c
             JSTUnicodeEscape c => Just c
             JSTSimpleEscape c => Just c
             _ => Nothing

quote : Grammar JSONStringToken True ()
quote = terminal $
  \t => case t of
             JSTQuote => Just ()
             _ => Nothing

string : Grammar JSONStringToken True String
string = do chars <- between quote quote $ many stringChar
            eof
            pure $ pack chars

export
parseString : List JSONStringToken -> Maybe String
parseString toks = case parse string toks of
                        Right (str, []) => Just str
                        _ => Nothing

export
permissiveStringLit : Lexer
permissiveStringLit
  = quo <+> manyUntil quo (esc any <|> any) <+> opt quo

export
stringValue : String -> Maybe String
stringValue x = parseString !(lexString x)
