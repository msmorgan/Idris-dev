module Language.JSON.String.Parser

import Language.JSON.String.Tokens
import Text.Lexer
import Text.Parser
import Text.Token

%access private
%default total


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
