module Language.JSON.Parser

import Language.JSON.Data
import Language.JSON.Tokens
import Text.Parser
import Text.Token

%access private
%default total

comma : Grammar JSONToken True ()
comma = terminal $
  \t => case t of
             JTComma => Just ()
             _ => Nothing

colon : Grammar JSONToken True ()
colon = terminal $
  \t => case t of
             JTColon => Just ()
             _ => Nothing

curly : Bracket -> Grammar JSONToken True ()
curly b = terminal $
  \t => case t of
             JTCurly b' => if b' == b
                              then Just ()
                              else Nothing
             _ => Nothing

square : Bracket -> Grammar JSONToken True ()
square b = terminal $
  \t => case t of
             JTSquare b' => if b' == b
                               then Just ()
                               else Nothing
             _ => Nothing

rawString : Grammar JSONToken True String
rawString = terminal $
  \t => case t of
             JTString str => str
             _ => Nothing

string : Grammar JSONToken True JSON
string = pure $ JString !rawString

boolean : Grammar JSONToken True JSON
boolean = terminal $
  \t => case t of
             JTBoolean b => Just $ JBoolean b
             _ => Nothing

number : Grammar JSONToken True JSON
number = terminal $
  \t => case t of
             JTNumber n => Just $ JNumber n
             _ => Nothing


null : Grammar JSONToken True JSON
null = terminal $
  \t => case t of
             JTNull => Just JNull
             _ => Nothing

mutual
  object : Grammar JSONToken True JSON
  object = do curly Open
              commit
              props <- properties
              curly Close
              pure (JObject props)
    where
      properties : Grammar JSONToken False (List (String, JSON))
      properties = sepBy comma $ do key <- rawString
                                    colon
                                    value <- json
                                    pure (key, value)

  array : Grammar JSONToken True JSON
  array = do square Open
             commit
             vals <- values
             square Close
             pure (JArray vals)
    where
      values : Grammar JSONToken False (List JSON)
      values = sepBy comma json

  json : Grammar JSONToken True JSON
  json = object
     <|> array
     <|> string
     <|> boolean
     <|> number
     <|> null

export
parseJSON : List JSONToken -> Maybe JSON
parseJSON toks = case parse json $ filter (not . ignored) toks of
                      Right (j, []) => Just j
                      _ => Nothing
