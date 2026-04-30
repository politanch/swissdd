# Changelog

## swissdd 1.1.6

- minor fixes for bugs in
  [`get_geodata()`](https://politanch.github.io/swissdd/reference/get_geodata.md)

## swissdd 1.1.5

- fix for `get_geodata(geolevel="canton")` added

## swissdd 1.1.4.999

- fix for `get_geodata(geolevel="zh_counting_districts")` and additional
  checks to ensure functions fail gracefully in case of availability and
  connection problems.

## swissdd 1.1.3

CRAN release: 2021-06-29

- fix for `get_geodata() / plot_nationalvotes()`

- gentle failure of the swissdd-function in case of errors in the data
  source / problems with the API

- bug fixes for get_geodata() / plot_nationalvotes()

## swissdd 1.1.1

CRAN release: 2021-02-15

- minor modification to the ‘plot_cantonalvotes()’-function to ensure
  that the function works even when there are no results yet for an
  upcoming vote.

## swissdd 1.1.0

CRAN release: 2021-02-03

- major update : new functions to retrieve geodata of voting districts
  and plot vote results maps have been added by [David
  Zumbach](https://github.com/zumbov2) (Thanks a thousand David!)

## swissdd 1.0.4

CRAN release: 2020-10-25

- the `canton_json_to_dfr` has been adapted to work with the altered
  data structure of the json files provided via opendata.swiss
  containing the cantonal vote results.

## swissdd 1.0.3

CRAN release: 2020-05-15

- broken examples fixed

## swissdd 1.0.2

CRAN release: 2020-01-19

- Functions renamed (breaking change): to retrieve real time or archive
  results data via opendata.swiss, the function `get_nationalvotes` must
  be used. `get_swissvotes` is turned into the function that allows to
  get Swissvotes-Data.

- New function that allows to retrieve Swissvotes-Data added
  (`get_swissvotes`)

- option to retrieve counting district level data with the
  [`get_nationalvotes()`](https://politanch.github.io/swissdd/reference/get_nationalvotes.md)-
  and
  [`get_cantonalvotes()`](https://politanch.github.io/swissdd/reference/get_cantonalvotes.md)-functions
  added (affects Zurich and Winterthur only)

- functions adapted to new tidyr API, the package now depends on tidyr
  \>= 1.0.0.

## swissdd 1.0.1

CRAN release: 2019-08-30

- Added parent level ids to district and municipality level results
  (canton / district)
- New function that calculates correlations between votes added:
  [`similar_votes()`](https://politanch.github.io/swissdd/reference/similar_votes.md)
- Better error handling with fallback if votedate information provided
  on opendata.swiss is corrupt
- Improved speed of data-retrieval. Comparison:

`system.time(get_swissvotes(to_date="1983-12-04"))`

user system elapsed

2.492 0.160 47.024

*comparison with swissdd version 1.0.0*

user system elapsed

3.120 0.276 64.693
