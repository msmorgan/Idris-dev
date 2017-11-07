module Language.JSON.Tokens

import Language.JSON.String
import Text.Token

%access public export
%default total

data Bracket = Open | Close

Eq Bracket where
  Open == Open = True
  Close == Close = True
  _ == _ = False

data JSONToken
  = JTBoolean Bool
  | JTNumber Double
  | JTString (Maybe String)
  | JTNull
  | JTIgnore
  | JTComma
  | JTColon
  | JTCurly Bracket
  | JTSquare Bracket

export
ignored : JSONToken -> Bool
ignored JTIgnore = True
ignored _ = False
