module Types where

import Control.Monad.State.Lazy

-- Logical variables, used as unknowns
newtype LVar = LVar Int
  deriving (Eq, Show)

data LValue = LValueInt Int
            | LValueVar LVar
            deriving (Eq, Show)

var :: Int -> LVar
var = LVar

isVar :: LValue -> Bool
isVar (LValueVar _) = True
isVar _             = False

type KanrenTerm = (LVar, LValue)

type KanrenSubstitutions = [KanrenTerm]

-- Program state
data KanrenState = KanrenState { nextVar       :: Int
                               , substitutions :: KanrenSubstitutions
                               } deriving Show

searchSubstitutions :: KanrenSubstitutions -> LVar -> Maybe LValue
searchSubstitutions [] _ = Nothing
searchSubstitutions (sub:subs) u = if u == fst sub
                                   then Just (snd sub)
                                   else searchSubstitutions subs u

addSubstitution :: KanrenSubstitutions -> LVar -> LValue -> KanrenSubstitutions
addSubstitution subs x v = (x,v) : subs

type KanrenProgram = State KanrenState KanrenSubstitutions

emptyState :: KanrenState
emptyState = KanrenState { nextVar = 0
                         , substitutions = []
                         }

runKanren :: KanrenProgram a -> (a, KanrenState)
runKanren p = runState p emptyState

fresh :: KanrenProgram LVar
fresh = do
  s <- get
  let k = nextVar s
  put s { nextVar = k + 1 }
  return $ LVar k

walk :: LVar -> KanrenProgram
walk u = do
  s <- get
  case searchSubstitutions (substitutions s) u of
    Nothing            -> return $ LValueVar u
    Just (LValueVar v) -> walk v
    Just (LValueInt k) -> return $ LValueInt k

extendState :: LVar -> LValue -> KanrenProgram
extendState x v = do
  s <- get
  put $ s { substitutions = addSubstitution (substitutions s) x v}

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x:y:(interleave xs ys)

newtype Goal a b = Goal (KanrenState -> KanrenState)
