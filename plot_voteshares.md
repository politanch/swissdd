Example: Plot Vote Shares
================

### plot voteshares on the cantonal level

``` r
# installation from CRAN (stable)
# install.packages("swissdd")
# install.packages("dplyr")
# install.packages("RSwissMaps")

# installation from github (ongoing updates)
# devtools::install_github("politanch/swissdd")

library(swissdd)
library(dplyr)
library(RSwissMaps)

#get data from API for the 2014 Swiss immigration initiative (mei)
mei_nat <- get_swissvotes(votedates="2014-02-09", geolevel = "canton")%>%
  dplyr::filter(id == 5800)%>%
  dplyr::select(canton_id, jaStimmenInProzent)%>%
  mutate(canton_id=as.numeric(canton_id))

#plot
can.plot(mei_nat$canton_id, mei_nat$jaStimmenInProzent, 2016,
         title = "2014 Swiss immigration initiative ",
         subtitle = "Swiss Cantons, 2016", 
         caption = "Data:swissdd/FSO\nPlot:RSwissMaps")
```

![](plot_voteshares_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### plot voteshares on the municipal level

``` r
#get data from API for the 2014 Swiss immigration initiative (mei)
mei_mun <- get_swissvotes(votedates="2014-02-09", geolevel = "municipality")%>%
  dplyr::filter(id == 5800)%>%
  dplyr::select(mun_id, jaStimmenInProzent)%>%
  mutate(mun_id=as.numeric(mun_id))

mun.plot(mei_mun$mun_id, mei_mun$jaStimmenInProzent, 2016,
         color_continuous = c("#c7e9c0", "#006d2c"),
         boundaries_size = 0.2,
         title = "2014 Swiss immigration initiative ",
         subtitle = "Swiss Municipalities, 2016", 
         caption = "Data:swissdd/FSO\nPlot:RSwissMaps")
```

![](plot_voteshares_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
