# Get cantonal results and counting status in real time or for selected dates or a time range in the past

`get_cantonalvotes` is one of the two main functions of swissvote
package. It allows to retrieve the results and the counting status for
national ballots.

## Usage

``` r
get_cantonalvotes(
  geolevel = "municipality",
  votedates = NULL,
  from_date = NULL,
  to_date = NULL
)
```

## Arguments

- geolevel:

  geographical level for which the results should be loaded. Options:
  "canton", "district", "municipality" or "zh_counting_districts".

- votedates:

  dates of the ballots to be selected. Default: most recent ballot
  available. Format: "YYYY-MM-DD".

- from_date:

  starting point in time from which vote results should be retrieved.
  Format: "YYYY-MM-DD".

- to_date:

  end point in time to which vote results should be retrieved. Format:
  "YYYY-MM-DD".

## Value

a tibble containing the results

## Details

get_cantonalvotes - retrieve vote results for cantonal ballots at
district- or municipality level for selected dates or a given date
range.

## Examples

``` r

# Select by range
results <- get_cantonalvotes(
   geolevel = "district", 
   from_date = "2019-01-01", 
   to_date = "2019-12-31"
   )

# Select specific votedate(s)
get_cantonalvotes(votedates = "2019-02-10")
#> # A tibble: 2,222 × 19
#>        id canton_name mun_id mun_name     geoLevelParentnummer gebietAusgezaehlt
#>     <int> <chr>       <chr>  <chr>        <chr>                <lgl>            
#>  1 104812 ZH          1      Aeugst am A… 101                  TRUE             
#>  2 104812 ZH          2      Affoltern a… 101                  TRUE             
#>  3 104812 ZH          3      Bonstetten   101                  TRUE             
#>  4 104812 ZH          4      Hausen am A… 101                  TRUE             
#>  5 104812 ZH          5      Hedingen     101                  TRUE             
#>  6 104812 ZH          6      Kappel am A… 101                  TRUE             
#>  7 104812 ZH          7      Knonau       101                  TRUE             
#>  8 104812 ZH          8      Maschwanden  101                  TRUE             
#>  9 104812 ZH          9      Mettmenstet… 101                  TRUE             
#> 10 104812 ZH          10     Obfelden     101                  TRUE             
#> # ℹ 2,212 more rows
#> # ℹ 13 more variables: jaStimmenInProzent <dbl>, jaStimmenAbsolut <int>,
#> #   neinStimmenAbsolut <int>, stimmbeteiligungInProzent <dbl>,
#> #   eingelegteStimmzettel <int>, anzahlStimmberechtigte <int>,
#> #   gueltigeStimmen <int>, de <chr>, en <chr>, fr <chr>, it <chr>, rm <chr>,
#> #   votedate <date>

# get the results at counting district level
# yields the same result as the municipality level, with the 
# exception of Winterthur and Zurich,
# where detailed counting district results are returned instead.

get_cantonalvotes(votedate = "2019-09-22", geolevel = "zh_counting_districts")
#> # A tibble: 33 × 19
#>        id canton_name mun_id mun_name   geoLevelParentnummer gebietAusgezaehlt
#>     <int> <chr>       <chr>  <chr>      <chr>                <lgl>            
#>  1 104897 OW          1401   Alpnach    600                  TRUE             
#>  2 104897 OW          1402   Engelberg  600                  TRUE             
#>  3 104897 OW          1403   Giswil     600                  TRUE             
#>  4 104897 OW          1404   Kerns      600                  TRUE             
#>  5 104897 OW          1405   Lungern    600                  TRUE             
#>  6 104897 OW          1406   Sachseln   600                  TRUE             
#>  7 104897 OW          1407   Sarnen     600                  TRUE             
#>  8 104898 SH          2901   Gächlingen 1401                 TRUE             
#>  9 104898 SH          2903   Löhningen  1401                 TRUE             
#> 10 104898 SH          2904   Neunkirch  1401                 TRUE             
#> # ℹ 23 more rows
#> # ℹ 13 more variables: jaStimmenInProzent <dbl>, jaStimmenAbsolut <int>,
#> #   neinStimmenAbsolut <int>, stimmbeteiligungInProzent <dbl>,
#> #   eingelegteStimmzettel <int>, anzahlStimmberechtigte <int>,
#> #   gueltigeStimmen <int>, de <chr>, en <chr>, fr <chr>, it <chr>, rm <chr>,
#> #   votedate <date>
```
