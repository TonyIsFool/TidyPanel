## Resubmission (v0.1.2)

This is a resubmission addressing feedback from CRAN reviewer Konstanze Lauseker:

* Added academic references to the Description field of DESCRIPTION:
  Wickham (2014) <doi:10.18637/jss.v059.i10> and
  Wickham & Bryan (2023) <https://readxl.tidyverse.org>

* Added executable code blocks to TidyPanel_introduction.Rmd using
  tempfile() so all examples run without external files.

* Replaced all \dontrun{} with executable toy examples that run in < 5
  seconds using tempfile() and in-memory data frames. No \dontrun{}
  remains in the package.

## Test environments
* local Windows 11 install, R 4.6.0

## R CMD check results

0 errors | 0 warnings | 0 notes
