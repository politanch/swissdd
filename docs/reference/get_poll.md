# Download poll data collected after a national vote by gfs.bern and the political science departements of the universities of Berne, Zurich, and Geneva.

`get_poll` downloads exit poll data Please cite data.

## Usage

``` r
get_poll(bfsnr = NULL, codebook = F)
```

## Arguments

- bfsnr:

  number of identification of the vote, by default = NULL. Polls
  available after September 2020 (from voteid 6360 onwards). Bfsnr
  corresponds to anr in swissvotes data and has to be four digits
  (available through get_swissvotes).

- codebook:

  by default = FALSE. If TRUE navigates your browser to the codebook if
  available.

## Value

a tibble containing the results

## Details

get_poll - retrieve poll data on votes. The unit of analysis are
individuals.

## Examples

``` r
results <- get_poll(bfsnr=6360, codebook=FALSE)
#> Data is downloaded from swissvotes.
```
