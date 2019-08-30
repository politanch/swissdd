## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
* The tests that have caused the check warnings/errors when the needed internet resources were not available have been removed
* Informative warnings have been added that notify the package user when the opendata.swiss DCAT API does not respond or the data can not be retrieved
