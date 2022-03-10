{-# LANGUAGE CPP #-}

module Util where

import           Control.Concurrent
import           Data.Functor

#ifndef mingw32_HOST_OS
import           System.Posix.Signals
#endif

oneSecond :: Int
oneSecond = 1000000

installKillHandler :: [ThreadId] -> IO ()

#ifdef mingw32_HOST_OS
installKillHandler = return . const ()
#else
installKillHandler threadIds = void $ installHandler keyboardSignal (Catch (mapM_ killThread threadIds)) Nothing
#endif
