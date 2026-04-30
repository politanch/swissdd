# Transform an opendata.swiss national results json into a tibble

`swiss_json_to_dfr` transforms the json containing the results of a
selected federal votedate into a tibble.

## Usage

``` r
swiss_json_to_dfr(
  votedate = NULL,
  geolevel = "municipality",
  dataurl = NULL,
  index = NULL,
  language = "DE",
  call_res
)
```

## Arguments

- votedate:

  date of the ballot. Default: most recent ballot available. To select
  multiple ballots use the 'get_swissvotes'-function. Format = YYYYMMDD

- geolevel:

  geographical level for which the results should be loaded. Options:
  "national", "canton", "district" or "municipality".

- dataurl:

  url of the dataset on opendata.swiss

- index:

  selection by index of the resource (last published = 1).

- language:

  defines the language of the vote title. Options: "DE" for German, "FR"
  for French, "IT" for Italian or "RM" for Romansh.

- call_res:

  result of a previous call to the base API. Optional argument.

## Value

a tibble containing the results

## Examples

``` r

# Transform the json of the most recent vote
results <- swiss_json_to_dfr()

# Transform the json of a selected votedate
swiss_json_to_dfr(votedate = "2019-02-10")
#> # A tibble: 2,116 × 16
#>    name            id canton_id canton_name mun_id mun_name geoLevelParentnummer
#>    <chr>        <int> <chr>     <chr>       <chr>  <chr>    <chr>               
#>  1 Volksinitia…  6260 1         Zürich      1      Aeugst … 101                 
#>  2 Volksinitia…  6260 1         Zürich      2      Affolte… 101                 
#>  3 Volksinitia…  6260 1         Zürich      3      Bonstet… 101                 
#>  4 Volksinitia…  6260 1         Zürich      4      Hausen … 101                 
#>  5 Volksinitia…  6260 1         Zürich      5      Hedingen 101                 
#>  6 Volksinitia…  6260 1         Zürich      6      Kappel … 101                 
#>  7 Volksinitia…  6260 1         Zürich      7      Knonau   101                 
#>  8 Volksinitia…  6260 1         Zürich      8      Maschwa… 101                 
#>  9 Volksinitia…  6260 1         Zürich      9      Mettmen… 101                 
#> 10 Volksinitia…  6260 1         Zürich      10     Obfelden 101                 
#> # ℹ 2,106 more rows
#> # ℹ 9 more variables: gebietAusgezaehlt <lgl>, jaStimmenInProzent <dbl>,
#> #   jaStimmenAbsolut <int>, neinStimmenAbsolut <int>,
#> #   stimmbeteiligungInProzent <dbl>, eingelegteStimmzettel <int>,
#> #   anzahlStimmberechtigte <int>, gueltigeStimmen <int>, votedate <date>
```
