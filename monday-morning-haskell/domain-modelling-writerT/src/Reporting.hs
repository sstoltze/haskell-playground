{-# LANGUAGE FlexibleContexts #-}

module Reporting where

import Data.Monoid (getSum)
import Data.Foldable (fold)

-- Stuff for WriterT
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer (listen, runWriterT, tell)

import qualified Database as DB
import Project

data Report = Report
  { budgetProfit :: Money
  , netProfit :: Money
  , difference :: Money
  } deriving (Show, Eq)

-- Required by Monoid
instance Semigroup Report where
  (Report b1 n1 d1) <> (Report b2 n2 d2) = Report (b1+b2) (n1+n2) (d1+d2)

-- mappend is defined from the Semigroup instance
instance Monoid Report where
  mempty = Report 0 0 0

calculateReport :: Budget -> [Transaction] -> Report
calculateReport budget transactions =
  Report
  { budgetProfit = budgetProfit'
  , netProfit = netProfit'
  , difference = netProfit' - budgetProfit'
  }
  where
    budgetProfit' = budgetIncome budget - budgetExpenditure budget
    netProfit' = getSum (foldMap asProfit transactions)
    asProfit (Sale m) = pure m
    asProfit (Purchase m) = pure (negate m)

-- calculateProjectReport :: Project ProjectId -> IO Report
-- calculateProjectReport = calc
--   where
--     calc (Project _ p) =
--       calculateReport <$> DB.getBudget p <*> DB.getTransactions p
--     calc (ProjectGroup _ projects) = foldMap calc projects

calculateProjectReports :: Project g ProjectId -> IO (Project Report Report)
calculateProjectReports project = fst <$> runWriterT (calc project)
  where
    calc (Project name p) = do
      report <- liftIO (calculateReport <$> DB.getBudget p <*> DB.getTransactions p)
      tell report -- Told things are accumulated with mappend, starting with mempty, leading to a single returned value
      pure (Project name report)
    calc (ProjectGroup name _ projects) = do
      (projects', report) <- listen (mapM calc projects) -- Use listen to extract the combined report of sub-projects
      pure (ProjectGroup name report projects')

-- accumulateProjectReport :: Project Report -> Report
-- accumulateProjectReport = fold
