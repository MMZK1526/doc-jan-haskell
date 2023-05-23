# IC DoC Haskell January Test Solutions

## Introduction

This repo contains my solutions to the past-year Imperial College Department of Computing Haskell January Tests. The questions are written by Professor Tony Field and can be found [here](http://wp.doc.ic.ac.uk/ajf/haskell-tests/).

Currently, I have implemented the solutions only for the tests since 2021. I will add the solutions for the rest later on.

These solutions are solely based on my personal responses and by no mean serve as the official solutions to the tests. While it is welcomed to discuss and share the solutions (**don't forget to refer Tony's work!**), I am not responsible for any potential damages (*is that even possible?*) caused by using these solutions.

As a direct consequence of the above, my solutions may use techniques not covered in the course, and may not be the most efficient ones. If you spot any mistakes in my solutions, please feel free to open an issue 😃

I have preserved all the original comments in the test. New comments are marked with `-- >` at the beginning of each line.

## How to Use

Since it is a not-mini compilation of Haskell code, I use cabal to manage the project.

Run `cabal repl` in the root directory, it will open up a GHCi REPL that already loads the files for
the tests. You can then load the modules for each year according to the following table to explore and test the functions. Note that simply running `ghci` and load the modules manually may not work because of the dependencies.

| Year | Module Name |
| ---- | ----------- |
| 2022 | `:m Test Year2022.Examples Year2022.SC Year2022.Types` |
| 2021 | `:m Test Year2021.Alloc Year2021.Examples Year2021.Types` |

Apart from the functions as per the specifications, in each year's test (right now only 2022), I also introduced a `tester` function that runs a series of tests on the functions.
