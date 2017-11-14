module Interfaces.Convert

%access public export
%default total

||| Discriminated conversion.
|||
||| Specifies conversion from an instance of an origin type into another
||| type, depending on some discriminator value.
|||
||| Like `cast`, this operation is potentially lossy. If a conversion may
||| fail, consider changing the definition of `Target` to handle a failure.
|||
||| ```idris example
||| data MyConversion = ToInt | ToString | ToChar
|||
||| Convert String MyConversion where
|||   Target ToInt = Int
|||   Target ToString = String
|||   Target ToChar = Maybe Char
|||
|||   convert ToInt x = cast x
|||   convert ToString x = x
|||   convert ToChar x = case unpack x of
|||                           (c :: _) => Just c
|||                           [] => Nothing
||| ```
interface Convert from discriminator where
  ||| The result of the conversion for this discriminator value.
  Target : discriminator -> Type

  ||| Perform the conversion.
  convert : (d : discriminator) -> (value : from) -> Target d
