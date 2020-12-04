# swissdd 1.0.4

* the `canton_json_to_dfr` has been adapted to work with the altered data structure of the json files provided via opendata.swiss containing the cantonal vote results.

# swissdd 1.0.3

* broken examples fixed

# swissdd 1.0.2

* Functions renamed (_*breaking change*_): to retrieve real time or archive results data via opendata.swiss, the function `get_nationalvotes` must be used. `get_swissvotes` is turned into the function that allows to get Swissvotes-Data.

* New function that allows to retrieve Swissvotes-Data added (`get_swissvotes`)

* option to retrieve counting district level data with the `get_nationalvotes()`- and `get_cantonalvotes()`-functions added (affects Zurich and Winterthur only)

* functions adapted to new tidyr API, the package now depends on tidyr >= 1.0.0.

# swissdd 1.0.1

* Added parent level ids to district and municipality level results (canton / district)
* New function that calculates correlations between votes added: `similar_votes()`
* Better error handling with fallback if votedate information provided on opendata.swiss is corrupt
* Improved speed of data-retrieval. Comparison:

`system.time(get_swissvotes(to_date="1983-12-04"))` 

user  system elapsed 

2.492   0.160  47.024 

*comparison with swissdd version 1.0.0*

user  system elapsed

3.120   0.276  64.693
