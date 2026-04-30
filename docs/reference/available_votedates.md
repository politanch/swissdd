# Get a vector of available vote dates via \`get_nationalvotes\` and \`get_cantonalvotes“

`available_votedates` is a utility function to get the available
votedates.

## Usage

``` r
available_votedates(geolevel = "national", call_res)
```

## Arguments

- geolevel:

  geographical level for which available votedates should be displayed.
  options "national" or "canton".

- call_res:

  result of a previous call to the base API. Optional argument.

## Value

a vector of votedates (Format: YYYY-MM-DD)

## Details

available_votedates - get available votedates of federal and cantonal
popular votes

## Examples

``` r

# Get vector of all available dates
federal_votedates <- available_votedates()
cantonal_votedates <- available_votedates(geolevel = "canton")
```
