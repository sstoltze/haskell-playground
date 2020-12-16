{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo where

import Project
import Reporting
import PrettyPrint
import Data.Generics.Fixplate.Draw

someProject :: Project
someProject = projectGroup "Sweden" [stockholm, gothenburg, malmo]
  where
    stockholm  = project 1 "Stockholm"
    gothenburg = project 2 "Gothenburg"
    malmo      = projectGroup "Malmö" [city, limhamn]
    city       = project 3 "Malmö City"
    limhamn    = project 4 "Limhamn"

test = putStrlN ""

-- reports :: IO Project
-- reports = calculateProjectReports someProject
-- >>> calculateProjectReports someProject >>= putStrLn . prettyProject prettyReport prettyReport

-- ppProject :: IO ()
-- ppProject = reports >>= putStrLn . prettyProject prettyReport prettyReport
