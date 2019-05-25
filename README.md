[![Build Status](https://travis-ci.org/politanch/swissdd.svg?branch=master)](https://travis-ci.org/politanch/swissdd/)

# swissdd

## the swiss direct democracy R package
<p align="center">
<img src="https://raw.githubusercontent.com/politanch/swissdd/dev/swissdd_sticker.png" alt="" width="100"/>
</p>

`swissdd` builds upon real time data service for federal and cantonal votes provided by the __Federal Statistical Office__ via [opendata.swiss](https://opendata.swiss/de/). It brings the results of popular votes, aggregated at the geographical level of choice, straight into R. Available levels are 

* national
* cantons
* districts
* municipalities

The package wraps the real time data on vote Sundays. As soon as the ballot close (from 12:00 on), the datastream is continuosly updated, until the data for all municipalities is complete and the final results are available. Additionally, it allows to access the archive and to retrieve the *harmonized* results of national votes since 1981.

```
devtools::install_github("politanch/swissdd")

#realtimedata on vote-sundays or the data of the last votations
federalvotes <- get_swissvotes_stream(geolevel = "district")

#retrieve data for many votes from the archive, either by selecting indiviual dates...
federalvotes <- get_swissvotes(votedates=c("2019-02-10","1984-09-23"), geolevel = "district")

#... or defining a range.
federalvotes <- get_swissvotes(from_date="2017-01-01",to_date="2018-01-01", geolevel = "district")

# the results of cantonal votes are also available (2019-)

cantonalvotes <- get_cantonalvotes(votedates="2019-02-10", geolevel = "municipality")

```

The webservice is documented on opendata.swiss (language settings can be found at the bottom of the page):

<img src="https://handbook.opendata.swiss/images/terms_by-ask.svg" alt="" width="12%"/>

https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen

https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen

## More data on Swiss politics 
- [DigDemLab](https://digdemlab.io/)  
- [swissparl](https://github.com/zumbov2/swissparl)

