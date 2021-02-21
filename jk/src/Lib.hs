{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( readBuffered
    , run
    ) where

import qualified System.IO as IO
import Data.Text.IO
import qualified Data.Text as T

readBuffered :: FilePath -> (IO.Handle -> IO r) -> IO r
readBuffered fileName f =
  IO.withFile fileName IO.ReadMode (\h -> IO.hSetBuffering h (IO.BlockBuffering Nothing) >> f h)

file :: FilePath
file = "allflows.csv"

run :: (IO.Handle -> IO r) -> IO r
run = readBuffered file

-- hReadLines :: Handle -> IO String
-- hReadLines h = do
--   done <- hIsEOF h
--   if done
--     then return ""
--     else do
--   l <- hGetLine h
--   ls <- hReadLines h
--   return $ l ++ "\n" ++ ls

-- Probably useless
hDoLines :: (T.Text -> a) -> (a -> b -> b) -> b -> IO.Handle -> IO b
hDoLines f g z h = do
  done <- IO.hIsEOF h
  if done
    then return z
    else do
    l <- hGetLine h
    ls <- hDoLines f g z h
    return $ g (f l) ls

hReadLines :: IO.Handle -> IO T.Text
hReadLines = hDoLines id (\a b -> T.append (T.append a "\n") b) ""

output :: IO T.Text
output = run hReadLines

type Path = (Int, Int)

--parseLine :: String -> Path
parseLine = drop 1 . takeWhile (/= ',')
