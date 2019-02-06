# swissdd

## the swiss direct democracy R package

´swissdd´ builds upon the swiss real time data service for federal and cantonal votes. It brings the results of popular votes, aggregated at the federal level of choice, straight into R (national, cantons, districts and municipalities).

By now, the package wraps the real time data on vote sundays. On vote sundays from 12:00 on, the datastream is continuosly updated, until the data for all municipalities is complete and the final results are available. We plan to integrate the data for past votes into the package as soon as it will be made available via the same web service.

So far, the package provides two functions (one of federal and the other for cantonal votes).

´
devtools::install_github(politanch/swissdd)

federalvotes <- get_swissvotes(votedate="20191002", geolevel = "district")

cantonalvotes <- get_cantonalvotes(votedate="20191002", geolevel = "municipality")

´

The webservice is documented on opendata.swiss.

https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen

https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen

