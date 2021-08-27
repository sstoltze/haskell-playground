module Types.Statsd (HandlerCounter(..)) where

import qualified System.Metrics.Counter as Counter

data HandlerCounter = HandlerCounter { requestCounter :: Counter.Counter
                                     , successCounter :: Counter.Counter
                                     , failureCounter :: Counter.Counter
                                     }
