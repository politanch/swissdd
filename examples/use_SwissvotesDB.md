Example: Plot Vote Shares
================
Last Updated: 25, November, 2019 at 22:23

### How to Use Swissvotes Data

First, you have to make sure that your machine is connected to the
internet.

``` r
# installation from CRAN (stable)
# install.packages("swissdd")
# install.packages("dplyr")

# installation from github (ongoing updates)
# devtools::install_github("politanch/swissdd")

Sys.setlocale('LC_ALL','C')
```

    ## [1] "C/C/C/C/C/en_US.UTF-8"

``` r
library(swissdd)
library(dplyr)
```

By default, function extracts only the database provided by
<https://swissvotes.ch>. However, you can specify that you want the
codebook as well (or just the codebook for that matter). Additionally,
there is a specification to save the citation. If you work with
Swissvotes data, we recommend to save the citation.

If you don’t specify anything the function will be executed with the
following specifications.

``` r
#default
swissvotesDB <- get_swissvotes(DB=T, savecitation=F, codebook=F)
```

If you use the parameter `codebook=T` the function will download the
data and direct your browser automatically to the codebook. If you’re
only interested in the codebook (you accessed the data already, for
example), set the parameter `DB` to `FALSE`.

Since we loaded the data already, there is no need to download it again.

``` r
get_swissvotes(DB=F, codebook=T)
```

For illustration, we are interested in the upcoming vote on affordable
housing (date of vote 9th of February, 2020). The variable
`titel_kurz_d` or `titel_kurz_f` contains the title of a vote (shorter
version than the official title `titel_off_d` or `titel_off_f`). Please
note that the letter `d` indicates the German title, while the letter
`f` gives you the French title. Since we don’t know too much about the
initiative we can read about it as well. First, we find the position in
the data and then use the hyperlink provided to read about it.

``` r
#get position in dataset
pos_ah <- grep("bezahlbare Wohnungen", swissvotesDB$titel_kurz_d)

#extract url that directs you to more information about the vote
mei_url <- swissvotesDB$anneepolitique[pos_ah]

#mei_url
#"https://anneepolitique.swiss/prozesse/56996-volksinitiative-mehr-bezahlbare-wohnungen"
```

``` r
#access url
browseURL(mei_url)
```
