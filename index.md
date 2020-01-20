# swissdd

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/swissdd)](https://cran.r-project.org/package=swissdd)
[![Build Status](https://travis-ci.org/politanch/swissdd.svg?branch=master)](https://travis-ci.org/politanch/swissdd/)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.en.html)
[![cranlogs](https://cranlogs.r-pkg.org/badges/grand-total/swissdd)](http://cran.rstudio.com/web/packages/swissdd/index.html)

## the swiss direct democracy R package
<p align="left">
<img src="swissdd_sticker.png" alt="" width="100"/>
</p>

`swissdd` builds upon the real time data service for federal and cantonal votes provided by the __Federal Statistical Office__ via [opendata.swiss](https://opendata.swiss/de/). It brings the results of popular votes, aggregated at the geographical level of choice, straight into R. Available levels are 

* national
* cantons
* districts
* municipalities

The package wraps the real time data on vote Sundays. As soon as the ballot close (from 12:00 on), the datastream is continuosly updated, until the data for all municipalities is complete and the final results are available. Additionally, it allows to access the archive and to retrieve the *harmonized* results of national votes since 1981.

Additionally, with the newest version available via CRAN it is now possible to retrieve data from the [swissvotes-database](https://swissvotes.ch/page/home), the most  comprehensive database on swiss popular votes. Due to the integration of this additional data source there is a __breaking change__: 

1. The `get_swissvotes`-function has been modified in its use. It now allows to retrieve the data from the [swissvotes-database](https://swissvotes.ch/page/home).

2. Real-time and archive-data from [opendata.swiss](https://opendata.swiss/de/) can now instead be loaded with the 'new' `get_nationalvotes`-function.

```
# installation from CRAN (stable)
install.packages("swissdd")

# installation from github (ongoing updates)
devtools::install_github("politanch/swissdd")

#realtimedata on vote-sundays or the data of the last votations
federalvotes <- get_nationalvotes(geolevel = "district")

#retrieve data for many votes from the archive, either by selecting indiviual dates...
federalvotes <- get_nationalvotes(votedates=c("2019-02-10","1984-09-23"), geolevel = "district")

#... or defining a range.
federalvotes <- get_nationalvotes(from_date="2017-01-01",to_date="2018-01-01", geolevel = "district")

# the results of cantonal votes are also available (2019-)

cantonalvotes <- get_cantonalvotes(votedates="2019-02-10", geolevel = "municipality")
```


The webservice of the federal statistical office is documented on opendata.swiss (language settings can be found at the bottom of the page):

<img src="https://handbook.opendata.swiss/images/terms_by-ask.svg" alt="" width="12%"/>

https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen

https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen

## More data on Swiss politics 
- [DigDemLab](https://digdemlab.io/)  
- [swissparl](https://github.com/zumbov2/swissparl)
- [swissvotes](https://swissvotes.ch/)
