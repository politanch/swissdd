## swissdd 1.1.3

New submission.

## Reason

swissdd was removed from CRAN in April 2021 due to a policy violation. I truly apologize for any inconvenience and thank the CRAN Team for their efforts. I have now fundamentaly improved the packages functions to ensure that they fail gracefully without leading to check errors when the API request yields an error or the resources are broken. Currently, the cantonal votes resource is corrupt (the data provider has been informed). Nonetheless the checks are successful, which would not have been the case before the latest fixes. 

## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## Local check

Package built by

``` r
library("devtools")
build()
```
Status: OK

## win-builder

``` r
devtools::check_win_devel()
```
Status: OK

Status: 1 NOTE
R Under development (unstable) (2021-06-18 r80528)

## Rhub checks

``` r
check_rhub()
```

Results:

1.  Debian Linux, R-devel, GCC ASAN/UBSAN:  Success
2.  Windows Server 2008 R2 SP1, R-devel, 32/64 bit: PREPERROR - related to the sf package
3.  Ubuntu Linux 20.04.1 LTS, R-release, GCC: Success
4.  Fedora Linux, R-devel, clang, gfortran: Success, with three (benign)
    warnings about CPU time for examples

