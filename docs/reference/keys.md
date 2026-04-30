# Merge data

Merge keys that allow to easily combine aggregate data from swissvotes
with post-ballot surveys (VOX).

## Usage

``` r
data(keys)
```

## Format

An object of class `data.frame` with 297 rows and 3 columns.

## Source

Walder, Maxime (\[Twitter\](https://twitter.com/WalderMaxime))

## Examples

``` r
data(keys)
bsnr <- keys$bfsnr
voxnr <- keys$projetx
```
