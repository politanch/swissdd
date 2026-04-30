# Get national results and counting status in real time or for selected dates or a time range in the past

`get_nationalvotes` is one of the two main functions of swissvote
package. It allows to retrieve the results and the counting status for
national ballots.

## Usage

``` r
get_nationalvotes(
  geolevel = "municipality",
  votedates = NULL,
  from_date = NULL,
  to_date = NULL,
  language = "DE"
)
```

## Arguments

- geolevel:

  geographical level for which the results should be loaded. Options:
  "national", "canton", "district", "municipality" or
  "zh_counting_districts".#' @param votedates dates of the ballots to be
  selected. Default: most recent ballot available. Format: "YYYY-MM-DD".

- votedates:

  dates of the ballots to be selected. Default: most recent ballot
  available. Format: "YYYY-MM-DD".

- from_date:

  starting point in time from which vote results should be retrieved.
  Format: "YYYY-MM-DD".

- to_date:

  end point in time to which vote results should be retrieved. Format:
  "YYYY-MM-DD".

- language:

  defines the language of the vote title. Options: "DE" for German, "FR"
  for French, "IT" for Italian or "RM" for Romansh.

## Value

a tibble containing the results

## Details

get_nationalvotes - retrieve vote results for national ballots at
district- or municipality level for selected dates or a given date
range.

## Examples

``` r
# \donttest{
# Selection by range
results <- get_nationalvotes(
    geolevel = "district", 
    from_date = "2018-01-01",
    to_date = "2018-12-31"
    )

# Selection by end date only
get_nationalvotes(to_date = "1983-12-04")
#> # A tibble: 23,029 × 16
#>    name            id canton_id canton_name mun_id mun_name geoLevelParentnummer
#>    <chr>        <int> <chr>     <chr>       <chr>  <chr>    <chr>               
#>  1 Bundesbesch…  3140 1         Zürich      1      Aeugst … 101                 
#>  2 Bundesbesch…  3140 1         Zürich      2      Affolte… 101                 
#>  3 Bundesbesch…  3140 1         Zürich      3      Bonstet… 101                 
#>  4 Bundesbesch…  3140 1         Zürich      4      Hausen … 101                 
#>  5 Bundesbesch…  3140 1         Zürich      5      Hedingen 101                 
#>  6 Bundesbesch…  3140 1         Zürich      6      Kappel … 101                 
#>  7 Bundesbesch…  3140 1         Zürich      7      Knonau   101                 
#>  8 Bundesbesch…  3140 1         Zürich      8      Maschwa… 101                 
#>  9 Bundesbesch…  3140 1         Zürich      9      Mettmen… 101                 
#> 10 Bundesbesch…  3140 1         Zürich      10     Obfelden 101                 
#> # ℹ 23,019 more rows
#> # ℹ 9 more variables: gebietAusgezaehlt <lgl>, jaStimmenInProzent <dbl>,
#> #   jaStimmenAbsolut <int>, neinStimmenAbsolut <int>,
#> #   stimmbeteiligungInProzent <dbl>, eingelegteStimmzettel <int>,
#> #   anzahlStimmberechtigte <int>, gueltigeStimmen <int>, votedate <date>
 # }
# Selection of a specific vote date
get_nationalvotes(votedates = "2014-02-09")
#> # A tibble: 6,342 × 16
#>    name            id canton_id canton_name mun_id mun_name geoLevelParentnummer
#>    <chr>        <int> <chr>     <chr>       <chr>  <chr>    <chr>               
#>  1 Bundesbesch…  5780 1         Zürich      1      Aeugst … 101                 
#>  2 Bundesbesch…  5780 1         Zürich      2      Affolte… 101                 
#>  3 Bundesbesch…  5780 1         Zürich      3      Bonstet… 101                 
#>  4 Bundesbesch…  5780 1         Zürich      4      Hausen … 101                 
#>  5 Bundesbesch…  5780 1         Zürich      5      Hedingen 101                 
#>  6 Bundesbesch…  5780 1         Zürich      6      Kappel … 101                 
#>  7 Bundesbesch…  5780 1         Zürich      7      Knonau   101                 
#>  8 Bundesbesch…  5780 1         Zürich      8      Maschwa… 101                 
#>  9 Bundesbesch…  5780 1         Zürich      9      Mettmen… 101                 
#> 10 Bundesbesch…  5780 1         Zürich      10     Obfelden 101                 
#> # ℹ 6,332 more rows
#> # ℹ 9 more variables: gebietAusgezaehlt <lgl>, jaStimmenInProzent <dbl>,
#> #   jaStimmenAbsolut <int>, neinStimmenAbsolut <int>,
#> #   stimmbeteiligungInProzent <dbl>, eingelegteStimmzettel <int>,
#> #   anzahlStimmberechtigte <int>, gueltigeStimmen <int>, votedate <date>
```
