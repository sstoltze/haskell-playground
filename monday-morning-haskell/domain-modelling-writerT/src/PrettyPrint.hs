{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import Data.Tree
import Data.Decimal (roundTo)
import Text.Printf
import Project
import Reporting

prettyMoney :: Money -> String
prettyMoney (Money d) = sign ++ show (roundTo 2 d)
  where
    sign = if d > 0
             then "+"
             else "" -- Decimal already adds - if negative

asTree :: (g -> String) -> (a -> String) -> Project g a -> Tree String
asTree prettyGroup prettyValue project =
  case project of
    Project name x -> Node (printf "%s: %s" name (prettyValue x)) []
    ProjectGroup name x projects ->
      Node (printf "%s: %s" name (prettyGroup x)) (map (asTree prettyGroup prettyValue) projects)

prettyProject :: (g -> String) -> (a -> String) -> Project g a -> String
prettyProject prettyGroup prettyValue = drawTree . asTree prettyGroup prettyValue

prettyReport :: Report -> String
prettyReport r =
  printf
    "Budget: %s, Net: %s, difference: %s"
    (prettyMoney (budgetProfit r))
    (prettyMoney (netProfit r))
    (prettyMoney (difference r))
