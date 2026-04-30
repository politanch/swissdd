# Transform a opendata.swiss cantonal results json into a tibble

`canton_json_to_dfr` tranforms a single results json for a selected
cantonal votedate into a tibble.

## Usage

``` r
canton_json_to_dfr(
  votedate = NULL,
  geolevel = "municipality",
  dataurl = NULL,
  index = NULL,
  call_res
)
```

## Arguments

- votedate:

  date of the ballot. Default: most recent ballot available.

- geolevel:

  geographical level for which the results should be loaded. Options:
  "canton", "district" or "municipality".

- dataurl:

  list of datasets / metadata for the given dataset and its resources OR
  url of the dcat dataset on opendata.swiss

- index:

  selection by index of the resource (last published = 1).

- call_res:

  result of a previous call to the base API. Optional argument.

## Value

a tibble containing the results

## Examples

``` r

# Get and transform the json for the most recent vote
results <- canton_json_to_dfr()

# Get and transform the json for a single votedate at counting district level
canton_json_to_dfr(votedate = "2020-02-09", geolevel = "zh_counting_districts")
#> # A tibble: 2,775 × 19
#>        id canton_name mun_id mun_name     geoLevelParentnummer gebietAusgezaehlt
#>     <int> <chr>       <chr>  <chr>        <chr>                <lgl>            
#>  1 104947 ZH          1      Aeugst am A… 101                  TRUE             
#>  2 104947 ZH          2      Affoltern a… 101                  TRUE             
#>  3 104947 ZH          3      Bonstetten   101                  TRUE             
#>  4 104947 ZH          4      Hausen am A… 101                  TRUE             
#>  5 104947 ZH          5      Hedingen     101                  TRUE             
#>  6 104947 ZH          6      Kappel am A… 101                  TRUE             
#>  7 104947 ZH          7      Knonau       101                  TRUE             
#>  8 104947 ZH          8      Maschwanden  101                  TRUE             
#>  9 104947 ZH          9      Mettmenstet… 101                  TRUE             
#> 10 104947 ZH          10     Obfelden     101                  TRUE             
#> # ℹ 2,765 more rows
#> # ℹ 13 more variables: jaStimmenInProzent <dbl>, jaStimmenAbsolut <int>,
#> #   neinStimmenAbsolut <int>, stimmbeteiligungInProzent <dbl>,
#> #   eingelegteStimmzettel <int>, anzahlStimmberechtigte <int>,
#> #   gueltigeStimmen <int>, de <chr>, en <chr>, fr <chr>, it <chr>, rm <chr>,
#> #   votedate <date>
```
