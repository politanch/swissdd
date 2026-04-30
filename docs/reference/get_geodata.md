# Get Swiss Geodata

`get_geodata` retrieves the latest geodata provided by the Federal
Statistical Office in connection with federal votes.

## Usage

``` r
get_geodata(geolevel = "municipality", latest = T, verbose = F, call_res)
```

## Arguments

- geolevel:

  geographical level. Options: "national", "canton", "district",
  "municipality", "zh_counting_districts" or "lakes".

- latest:

  if `TRUE`, the latest data is retrieved. If `FALSE`, geo data from the
  beginning of the year is retrieved. The API does not support finer
  distinctions. For more detailed information on the exact status of the
  data, please use `verbode = TRUE`.

- verbose:

  if `TRUE`, the date from which the data originates is displayed.

- call_res:

  result of a previous call to the geodata API. Optional argument.

## Value

a simple feature collection of the desired spatial units with
corresponding IDs.

## Examples

``` r

# Get latest geodata at municipal level
get_geodata()
#> Simple feature collection with 2105 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 2485202 ymin: 1075286 xmax: 2834036 ymax: 1295872
#> CRS:           NA
#> First 10 features:
#>    mun_id                       geometry
#> 1       1 MULTIPOLYGON (((2678553 123...
#> 2       2 MULTIPOLYGON (((2673840 123...
#> 3       3 MULTIPOLYGON (((2676438 124...
#> 4       4 MULTIPOLYGON (((2680614 123...
#> 5       5 MULTIPOLYGON (((2675417 124...
#> 6       6 MULTIPOLYGON (((2679234 123...
#> 7       7 MULTIPOLYGON (((2676331 123...
#> 8       8 MULTIPOLYGON (((2675434 123...
#> 9       9 MULTIPOLYGON (((2676169 123...
#> 10     10 MULTIPOLYGON (((2672854 123...

# Get latest geodata at cantonal level
get_geodata(geolevel = "canton")
#> Simple feature collection with 26 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 2485202 ymin: 1075286 xmax: 2834036 ymax: 1295872
#> CRS:           NA
#> First 10 features:
#>    canton_id                       geometry
#> 1          1 MULTIPOLYGON (((2695058 122...
#> 2          2 MULTIPOLYGON (((2575614 119...
#> 3          3 MULTIPOLYGON (((2648177 119...
#> 4          4 MULTIPOLYGON (((2679718 115...
#> 5          5 MULTIPOLYGON (((2714001 119...
#> 6          6 MULTIPOLYGON (((2672531 118...
#> 7          7 MULTIPOLYGON (((2678714 118...
#> 8          8 MULTIPOLYGON (((2718732 122...
#> 9          9 MULTIPOLYGON (((2673840 122...
#> 10        10 MULTIPOLYGON (((2583105 119...
```
