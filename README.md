# IC DoC Haskell January Test Solutions

## Introduction

This repo contains my solutions to the past-year Imperial College Department of Computing Haskell January Tests. The questions are written by Professor Tony Field and can be found [here](http://wp.doc.ic.ac.uk/ajf/haskell-tests/).

Currently, I have implemented the solutions for the tests since 2014. I will gradually (no guarantee on when since I'm overloaded by life, but never forgetting this) add the solutions for the rest.

These solutions are solely based on my personal responses and by no mean serve as the official solutions to the tests. While it is welcomed to discuss and share the solutions (**don't forget to refer Tony's work!**), I am not responsible for any potential damages (*is that even possible?*) caused by using these solutions.

As a direct consequence of the above, my solutions may use techniques not covered in the course, and may not be the most efficient ones. If you spot any mistakes in my solutions, please feel free to open an issue 😃

I have preserved all the original comments in the test. New comments are marked with `-- >` at the beginning of each line.

## How to Use

Since it is a not-mini compilation of Haskell code, I use cabal to manage the project.

### Tests Before 2024

Run `cabal repl` in the root directory, it will open up a GHCi REPL that already loads the files for the tests. You can then load the modules for each year according to the following table to explore and test the functions. Note that simply running `ghci` and load the modules manually may not work because of the dependencies.

Otherwise if you have not installed `cabal`, you can just copy the solution of any given year into a standalone Haskell file (remember to remove the `Year20XX.` prefix in the imports as well as the `tester` function). It should compile.

If there's any issue or doubt on running the solutions, you are more than welcomed to drop an issue or contact me directly, and I will do my best to assist.

| Year | Module Name                                                                                |
| ---- | ------------------------------------------------------------------------------------------ |
| 2023 | `:m Test Year2022.Clues Year2022.Examples Year2022.Solve Year2022.Types Year2022.WordData` |
| 2022 | `:m Test Year2022.Examples Year2022.SC Year2022.Types`                                     |
| 2021 | `:m Test Year2021.Alloc Year2021.Examples Year2021.Types`                                  |
| 2020 | `:m Test Year2020.Examples Year2020.HashFunctions Year2020.Tries Year2020.Types`           |
| 2019 | `:m Test Year2019.SOL Year2019.TestData Year2019.Types`                                    |
| 2018 | `:m Test Year2018.CP`                                                                      |
| 2017 | `:m Test Year2017.DC`                                                                      |
| 2016 | `:m Test Year2016.Exam`                                                                    |
| 2015 | `:m Test Year2015.Exam`                                                                    |
| 2014 | `:m Test Year2014.Exam`                                                                    |

### Tests Since 2024

The tests since 2024 are presented in a different format. They are now in the form of a cabal package, and each test has its own cabal package (written by Jamie Willis). I have copied the original packages into the repo under folders "Year20XX". To interact with them, run `cabal repl` either in the root directory or in the folder of the test you want to explore. Then you can load the modules and test the functions as usual.

## Test Suite

Run `cabal test all` to run the test suites. The test suites before 2024 are made by myself, while later Haskell tests have their own test suites [^1].

[^1]: While the older tests do have test cases, they are not provided in the template files.
