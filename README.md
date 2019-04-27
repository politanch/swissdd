# swissdd

## the swiss direct democracy R package

`swissdd` builds upon the swiss real time data service for federal and cantonal votes. It brings the results of popular votes, aggregated at the geographical level of choice, straight into R (national, cantons, districts and municipalities).

The package wraps the real time data on vote sundays. On vote sundays from 12:00 on, the datastream is continuosly updated, until the data for all municipalities is complete and the final results are available. Additionally, it allows to access the archive national votes since 1983.

```
devtools::install_github("politanch/swissdd")

#realtimedata on vote-sundays or the data of the last votations
federalvotes <- get_swissvotes_stream(geolevel = "district")

#retrieve data for many votes, either by selecting indiviual dates...
federalvotes <- get_swissvotes(votedates=c("2019-10-02","1984-09-23"), geolevel = "district")

#... or defining a range.
federalvotes <- get_swissvotes(from_date="2017-01-01",to_date="2018-01-01", geolevel = "district")

# cantonal results are also already available, however the functions to get the cantonal results will be slightly modified shortly.

cantonalvotes <- get_cantonalvotes_stream(votedate="20191002", geolevel = "municipality")

```

The webservice is documented on opendata.swiss:

https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen

https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen

