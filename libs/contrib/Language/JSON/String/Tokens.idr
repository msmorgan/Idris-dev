module Language.JSON.String.Tokens

import Data.String.Extra
import Text.Token

%access export
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
