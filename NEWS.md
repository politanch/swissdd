# swissdd (development version)

# swissdd 1.0.1.9000

* option to retrieve counting district level data (Winterthur & Zürich) added

* Added parent level ids to district and municipality level results (canton / district)
* New function that calculates correlations between votes added: `similar_votes()`
* Better error handling with fallback if votedate information provided on opendata.swiss is corrupt
* Improved speed of data-retrieval. Comparison:

`system.time(get_swissvotes(to_date="1983-12-04")))` 

__swissdd 1.0.0__

user  system elapsed

3.120   0.276  64.693

__swissdd 1.0.1__

user  system elapsed 

2.492   0.160  47.024 
  
>>>>>>> master
