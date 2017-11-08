module Language.JSON.Data

import Data.String.Extra

%access private
%default total

public export
data JSON
   = JNull
   | JBoolean Bool
   | JNumber Double
   | JString String
   | JArray (List JSON)
   | JObject (List (String, JSON))

%name JSON json

showChar : Char -> String
showChar c
  = case c of
         '\b' => "\\b"
         '\f' => "\\f"
         '\n' => "\\n"
         '\r' => "\\r"
         '\t' => "\\t"
         '\\' => "\\\\"
         '"'  => "\\\""
         c => if isControl c || c >= '\127'
                 then "\\u" ++ b16ToHexString (fromInteger (cast (ord c)))
                 else singleton c

showString : String -> String
showString x = "\"" ++ concatMap showChar (unpack x) ++ "\""

mutual
  stringifyArray : List JSON -> String
  stringifyArray [] = ""
  stringifyArray (x :: []) = stringify x
  stringifyArray (x :: xs) = stringify x ++ "," ++ stringifyArray xs

  stringifyProp : (String, JSON) -> String
  stringifyProp (key, value) = show key ++ ":" ++ stringify value

  stringifyObject : List (String, JSON) -> String
  stringifyObject [] = ""
  stringifyObject (x :: []) = stringifyProp x
  stringifyObject (x :: xs) = stringifyProp x ++ "," ++ stringifyObject xs

  ||| Convert a JSON into its string representation. No whitespace is added.
  export
  stringify : JSON -> String
  stringify JNull = "null"
  stringify (JBoolean x) = if x then "true" else "false"
  stringify (JNumber x) = show x
  stringify (JString x) = showString x
  stringify (JArray xs) = "[" ++ stringifyArray xs ++ "]"
  stringify (JObject xs) = "{" ++ stringifyObject xs ++ "}"

export
Show JSON where
  show = stringify

mutual
  formatArray : (n, curr : Nat) -> List JSON -> String
  formatArray n curr [] = ""
  formatArray n curr (x :: []) = format' n curr x ++ "\n"
  formatArray n curr (x :: xs) = format' n curr x ++ ",\n"
                              ++ formatArray n curr xs

  formatObject : (n, curr : Nat) -> List (String, JSON) -> String
  formatObject n curr [] = ""
  formatObject n curr (x :: []) = formatProperty n curr x ++ "\n"
  formatObject n curr (x :: xs) = formatProperty n curr x ++ ",\n"
                               ++ formatObject n curr xs

  formatProperty : (n, curr : Nat) -> (String, JSON) -> String
  formatProperty n curr (key, value) = indent curr $
    show key ++ ": " ++ formatValue n curr value

  ||| Format without initial indentation.
  formatValue : (n, curr : Nat) -> JSON -> String
  formatValue n curr json
    = case json of
           JArray [] => "[]"
           JArray xs => "[\n" ++ formatArray n (n + curr) xs
                              ++ indent curr "]"
           JObject [] => "{}"
           JObject xs => "{\n" ++ formatObject n (n + curr) xs
                               ++ indent curr "}"
           x => stringify x

  ||| Format with initial indentation.
  format' : (n, curr : Nat) -> JSON -> String
  format' n curr json = indent curr $ formatValue n curr json

||| Format a JSON with a given amount of whitespace per indentation level.
export
format : Nat -> JSON -> String
format n json = format' n 0 json
