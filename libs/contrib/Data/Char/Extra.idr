module Data.Char.Extra

%access public export
%default total

||| Returns true if the character is an ISO 6429 control character.
isControl : Char -> Bool
isControl c = c >= '\x00' && c <= '\x1f' || -- C0
              c >= '\x7f' && c <= '\x9f'    -- delete, C1
