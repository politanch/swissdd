# Obtain similarities a vote result shares with other votes

`similar_votes` allows to obtain correlations of specified vote with
other votes.

## Usage

``` r
similar_votes(
  federalvotes = NULL,
  id = NULL,
  corr = TRUE,
  from = NULL,
  to = NULL
)
```

## Arguments

- federalvotes:

  tibble or data.frame that is returned by 'get_swissvotes'.

- id:

  identification number of the vote, needs four digits. Vote 626
  (Zersiedelungsinitiative) needs 6260.

- corr:

  set to TRUE by default. If FALSE return the variance-covariance
  matrix.

- from:

  lower limit of correlations.

- to:

  upper limit of correlations.

## Value

a tibble containing the results

## Examples

``` r
 # \donttest{
 
 fedvotes <- get_nationalvotes(geolevel = "canton",from_date = "2010-03-07",to_date="2019-02-10")
 
 #Find correlating votes for the 'Zersiedelungsinitiative', 2019-02-10
 results <- similar_votes(fedvotes, id=6260)
 

#Zersiedelungsinitiative, 2019-02-10, filter stronger correlations (>0.5)
 results <- similar_votes(fedvotes, id=6260, from = 0.5)

# }
```
