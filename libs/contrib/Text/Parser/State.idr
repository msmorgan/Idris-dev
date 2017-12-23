module Text.Parser.Stateful

import Control.Monad.State
import Control.ST
import Text.Parser

import public Control.Delayed

%access public export
%default total

get : StateT stateType (Grammar tok False) stateType
get = ST (\x => pure (x, x))

put : stateType -> StateT stateType (Grammar tok False) ()
put x = ST (\y => pure ((), x))

modify : (stateType -> stateType) -> StateT stateType (Grammar tok False) ()
modify f = ST (\x => pure ((), f x))

lemma : (a, stateType) -> Inf (a -> StateT stateType (Grammar tok c2) b) -> Grammar tok c2 (b, stateType)
lemma (v, st') k = let ST kv = k v in
                       kv st'

(>>=) : StateT stateType (Grammar tok c1) a ->
        inf c1 (a -> StateT stateType (Grammar tok c2) b) ->
        StateT stateType (Grammar tok (c1 || c2)) b
(>>=) {c1 = False} (ST f) k = ST (\st => do (v, st') <- f st
                                            let ST kv = k v
                                            kv st')
(>>=) {c1 = True} (ST f) k = ST (\st => do x <- f st
                                           lemma x k)

pure : a -> StateT stateType (Grammar tok False) a
pure x = ST (\st => pure (x, st))


map : (a -> b) -> StateT stateType (Grammar tok c) a ->
                  StateT stateType (Grammar tok c) b
map f (ST g) = ST (\st => map (mapFst f) (g st))
  where mapFst : (a -> x) -> (a, s) -> (x, s)
        mapFst fn (a, b) = (fn a, b)

-- (>>=) {c1 = True} {c2 = True} (ST f) k = ST (\st => (f st) >>= Delay (\x => ?whatNow3))
                                           -- kv st')


-- gets : (stateType -> a) -> StateT stateType (Grammar tok False) a
-- gets f = do s <- get
--             pure (f s)


GrammarTrans : Type -> Type -> Bool -> Type -> Type
GrammarTrans st tok consumes result = st -> Grammar tok consumes (result, st)

identity : Grammar tok True a -> GrammarTrans st tok True a
identity g state = map (\x => (x, state)) g

comma : Grammar Char True ()
comma = terminal (\c => if c == ',' then Just () else Nothing)

commaCounter : GrammarTrans Nat Char True ()
commaCounter n = do comma
                    commaCounter (S n) <|> pure ((), S n)

countCommas : Grammar Char True Nat
countCommas = map (\(_, n) => n) (commaCounter Z)

