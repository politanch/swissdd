# swissdd 1.0.1

* Added parent level ids to district and municipality level results (canton / district)
* Better error handling with fallback if votedate information provided on opendata.swiss is corrupt
* Improved speed of data-retrieval. Comparison:

`system.time(get_swissvotes(to_date="1983-12-04")))` 

__swissdd 1.0.1__

user  system elapsed

3.120   0.276  64.693

__swissdd 1.0.1__

user  system elapsed 

2.492   0.160  47.024 
  
