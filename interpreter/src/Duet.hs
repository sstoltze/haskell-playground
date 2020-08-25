{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}

module Duet where

import           Control.Applicative
import           Control.Monad.Operational
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer  as MW
import           Control.Lens
import           Data.Char (isAlpha)
import           Data.Maybe (fromJust)
import           Data.Functor.Sum
import qualified Control.Monad.Trans.Accum as A
import qualified Data.List.PointedList as P
import qualified Data.Map              as M

class (Monad m, Monoid w) => MonadAccum w m | m -> w where
  add  :: w -> m ()
  look :: m w

instance (Monoid w, Monad m) => MonadAccum w (A.AccumT w m) where
    add = A.add
    look = A.look

instance MonadAccum w m => MonadAccum w (MaybeT m) where
    add = lift . add
    look = lift look

instance MonadAccum w m => MonadAccum w (StateT s m) where
    add = lift . add
    look = lift look

instance (Monoid v, MonadAccum w m) => MonadAccum w (MW.WriterT v m) where
    add = lift . add
    look = lift look

type Addr = Either Char Int

data Op = OSnd Addr
        | ORcv Char
        | OJgz Addr Addr
        | OBin (Int -> Int -> Int) Char Addr

-- Memory access
data Mem :: * -> * where
  MGet  :: Char -> Mem Int
  MSet  :: Char -> Int -> Mem ()
  MJump :: Int  -> Mem ()
  MPeek :: Mem Op

-- Communication
data Com :: * -> * where
  CSnd :: Int -> Com ()
  CRcv :: Int -> Com Int

-- Program
type Duet = Program (Sum Mem Com)

parseOp :: String -> Op
parseOp inp = case words inp of
                "snd" : x         : _ -> OSnd (addr x)
                "rcv" : (x:_)     : _ -> ORcv x
                "add" : (x:_) : y : _ -> OBin (+)        x (addr y)
                "mul" : (x:_) : y : _ -> OBin (*)        x (addr y)
                "mod" : (x:_) : y : _ -> OBin mod        x (addr y)
                "set" : (x:_) : y : _ -> OBin (const id) x (addr y)
                "jgz" : x     : y : _ -> OJgz (addr x) (addr y)
                _                     -> error "Bad parse"
  where
    addr :: String -> Addr
    addr [c] | isAlpha c = Left c
    addr str = Right (read str)

parseProgram :: String -> P.PointedList Op
parseProgram = fromJust . P.fromList . map parseOp . lines

testProgram :: String
testProgram = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"

dGet :: Char -> Duet Int
dGet = singleton . InL . MGet

dSet :: Char -> Int -> Duet ()
dSet r = singleton . InL . MSet r

dJump :: Int -> Duet ()
dJump = singleton . InL . MJump

dPeek :: Duet Op
dPeek = singleton (InL MPeek)

dSnd :: Int -> Duet ()
dSnd = singleton . InR . CSnd

dRcv :: Int -> Duet Int
dRcv = singleton . InR . CRcv

stepProg :: Duet ()
stepProg = dPeek >>= \case
  OSnd x -> do
    dSnd =<< addrVal x
    dJump 1
  OBin f x y -> do
    yVal <- addrVal y
    xVal <- dGet x
    dSet x $ f xVal yVal
    dJump 1
  ORcv x -> do
    y <- dRcv =<< dGet x
    dSet x y
    dJump 1
  OJgz x y -> do
    xVal <- addrVal x
    dJump =<< if xVal > 0
              then addrVal y
              else return 1
  where
    addrVal (Left  r) = dGet r
    addrVal (Right x) = return x

data ProgState = PS { _psOps :: P.PointedList Op
                    , _psRegs :: M.Map Char Int

                    }
makeClassy ''ProgState

-- psRegs . at c . non 0 :: HasProgState s => Lens' s Int
-- 'at c' er en Lens der tager et map og giver en Maybe værdi for keyen c
-- 'non 0' oversætter Nothing til 0 og Just x til x.
-- Så 'at c . non 0' er en lens der tager et map :: M.Map Char Int og giver en Int
interpMem :: (MonadState s m, MonadFail m, HasProgState s) => Mem a -> m a
interpMem = \case
  MGet c   -> use (psRegs . at c . non 0)
  MSet c x -> psRegs . at c . non 0 .= x
  MJump n  -> do
    Just t' <- P.moveN n <$> use psOps
    psOps .= t'
  MPeek    -> use (psOps . P.focus)

-- Part A:
interpComA :: (MonadAccum (MW.Last Int) m, MW.MonadWriter (MW.First Int) m) => Com a -> m a
interpComA = \case
  CSnd x -> add (MW.Last (Just x))
  CRcv x -> do
    unless (x == 0) $ do
      MW.Last lastSent <- look
      MW.tell (MW.First lastSent)
    return x

-- Part B:
data Thread = T { _tState :: ProgState
                , _tBuffer :: [Int]
                }
makeClassy ''Thread

instance HasProgState Thread where
  progState = tState

interpComB :: (MW.MonadWriter [Int] m, MonadFail m, MonadState Thread m) => Com a -> m a
interpComB = \case
  CSnd x -> MW.tell [x]
  CRcv _ -> do
    x:xs <- use tBuffer
    tBuffer .= xs
    return x

(>|<) :: (f a -> r) -> (g a -> r) -> (Sum f g a -> r)
f >|< g = \case
  InL x -> f x
  InR x -> g x

stepA :: MaybeT (StateT ProgState (MW.WriterT (MW.First Int) (A.Accum (MW.Last Int)))) ()
stepA = interpretWithMonad (interpMem >|< interpComA) stepProg

partA :: P.PointedList Op -> Maybe Int
partA ops = MW.getFirst
          . flip A.evalAccum mempty
          . MW.execWriterT
          . flip runStateT (PS ops M.empty)
          . runMaybeT
          $ many stepA

stepB :: MaybeT (State (Thread, Thread)) Int
stepB = do
  outA <- MW.execWriterT . zoom _1 $ many $ interpretWithMonad (interpMem >|< interpComB) stepProg
  outB <- MW.execWriterT . zoom _2 $ many $ interpretWithMonad (interpMem >|< interpComB) stepProg
  _1 . tBuffer .= outB
  _2 . tBuffer .= outA
  guard . not $ null outA && null outB
  return $ length outB -- What we need to output for the answer

partB :: P.PointedList Op -> Int
partB ops = maybe (error "`many` cannot fail") sum
          . flip evalState s0
          . runMaybeT
          $ many stepB
  where
    s0 = ( T (PS ops (M.singleton 'p' 0)) []
         , T (PS ops (M.singleton 'p' 1)) []
         )

main :: IO ()
main = do
    print $ partA (parseProgram testProg)
    print $ partB (parseProgram testProg)

testProg :: String
testProg = unlines
  ["set i 31"
  , "set a 1"
  , "mul p 17"
  , "jgz p p"
  , "mul a 2"
  , "add i -1"
  , "jgz i -2"
  , "add a -1"
  , "set i 127"
  , "set p 826"
  , "mul p 8505"
  , "mod p a"
  , "mul p 129749"
  , "add p 12345"
  , "mod p a"
  , "set b p"
  , "mod b 10000"
  , "snd b"
  , "add i -1"
  , "jgz i -9"
  , "jgz a 3"
  , "rcv b"
  , "jgz b -1"
  , "set f 0"
  , "set i 126"
  , "rcv a"
  , "rcv b"
  , "set p a"
  , "mul p -1"
  , "add p b"
  , "jgz p 4"
  , "snd a"
  , "set a b"
  , "jgz 1 3"
  , "snd b"
  , "set f 1"
  , "add i -1"
  , "jgz i -11"
  , "snd a"
  , "jgz f -16"
  , "jgz a -19"
  ]
