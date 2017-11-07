module Language.JSON.Data

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
