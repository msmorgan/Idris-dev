module Text.Lexer.Horrible

import Text.Lexer

%access export
%default total

infix 0 !:, $:

||| Make a `TokenMap tok` entry for a `Lexer` that always results in the
||| same token value.
|||
||| ```idris example
||| data MyToken = MTComma | MTColon
|||
||| myMap : TokenMap MyToken
||| myMap = [ is ',' !: MTComma
|||         , is ':' !: MTColon
|||         ]
||| ```
(!:) : Lexer -> tok -> (Lexer, String -> tok)
(!:) l t = (l, const t)

||| A `Literal` represents a `Lexer` and a way to produce a value of
||| type `a` based on the text it recognises.
public export
Literal : (a : Type) -> Type
Literal a = (Lexer, String -> a)

||| Create a `TokenMap tok` entry from a `Literal a` and a function
||| that can create a `tok` from an `a`.
|||
||| ```idris example
||| data MyToken = MTInt Int | MTBool Bool
|||
||| bool : Literal Bool
||| bool = (exact "True" <|> exact "False", (== "True"))
|||
||| myMap : TokenMap MyToken
||| myMap = [ (intLit, cast) $: MTInt
|||         , bool           $: MTBool
|||         ]
||| ```
($:) : Literal a -> (a -> tok) -> (Lexer, String -> tok)
($:) (l, mkVal) mkTok = (l, mkTok . mkVal)
