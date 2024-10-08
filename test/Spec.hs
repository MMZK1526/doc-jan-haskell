{-# LANGUAGE BlockArguments #-}

import qualified Year2014.Exam as Y2014
import qualified Year2015.Exam as Y2015
import qualified Year2016.Exam as Y2016
import qualified Year2017.DC as Y2017
import qualified Year2018.CP as Y2018
import qualified Year2019.SOL as Y2019
import qualified Year2020.Tries as Y2020
import qualified Year2021.Alloc as Y2021
import qualified Year2022.SC as Y2022
import qualified Year2023.Solver as Y2023

main :: IO ()
main = do
  putStrLn "Year 2014 Tests: "
  Y2014.tester
  putStrLn "Year 2014 Tests Done!"
  putStrLn "Year 2015 Tests: "
  Y2015.tester
  putStrLn "Year 2015 Tests Done!"
  putStrLn "Year 2016 Tests: "
  Y2016.tester
  putStrLn "Year 2016 Tests Done!"
  putStrLn "Year 2017 Tests: "
  Y2017.tester
  putStrLn "Year 2017 Tests Done!"
  putStrLn "Year 2018 Tests: "
  Y2018.tester
  putStrLn "Year 2018 Tests Done!"
  putStrLn "Year 2019 Tests: "
  Y2019.tester
  putStrLn "Year 2019 Tests Done!"
  putStrLn "Year 2020 Tests: "
  Y2020.tester
  putStrLn "Year 2020 Tests Done!"
  putStrLn "Year 2021 Tests: "
  Y2021.tester
  putStrLn "Year 2021 Tests Done!"
  putStrLn "Year 2022 Tests: "
  Y2022.tester
  putStrLn "Year 2022 Tests Done!"
  putStrLn "Year 2023 Tests: "
  Y2023.tester
  putStrLn "Year 2023 Tests Done!"
  putStrLn "All Tests Done!"
