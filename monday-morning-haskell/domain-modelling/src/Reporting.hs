{-# LANGUAGE FlexibleContexts #-}

module Reporting where

import Data.Monoid (getSum)
import Data.Foldable (fold)
import Data.Generics.Fixplate.Base (Attr)
import Data.Generics.Fixplate.Attributes (synthetiseM)

import qualified Database as DB
import Project

data Report = Report
  { budgetProfit :: Money
  , netProfit :: Money
  , difference :: Money
  } deriving (Show, Eq)

type ProjectReport = Attr ProjectF Report

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

calculateProjectReports :: Project -> IO ProjectReport
calculateProjectReports = synthetiseM calc
  where
    calc (Project p _) = calculateReport <$> DB.getBudget p <*> DB.getTransactions p
    calc (ProjectGroup _ reports) = pure (fold reports)

-- accumulateProjectReport :: Project Report -> Report
-- accumulateProjectReport = fold
