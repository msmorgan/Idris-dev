module Text.Token

import Interfaces.Convert

%access public export
%default total

||| A token of a particular kind and the text that was recognised.
record Token k where
  constructor Tok
  kind : k
  text : String

||| Get the value of a `Token k`. The resulting type depends upon
||| the kind of token.
value : Convert String k => (t : Token k) -> Target {from=String} (kind t)
value (Tok k x) = convert k x
