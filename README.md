[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/swissdd)](https://cran.r-project.org/package=swissdd)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.en.html)
[![cranlogs](https://cranlogs.r-pkg.org/badges/grand-total/swissdd)](http://cran.rstudio.com/web/packages/swissdd/index.html)

# swissdd

## the swiss direct democracy R package
<p align="left">
<img src="https://raw.githubusercontent.com/politanch/swissdd/master/pkgdown/swissdd_sticker.png" alt="" width="100"/>


### Installation
To install the CRAN version use `install.packages("swissdd")` or `devtools::install_github("swissdd")` for the development version.


`swissdd` builds upon real time data service for federal and cantonal votes provided by the __Federal Statistical Office__ via [opendata.swiss](https://opendata.swiss/de/). It brings the results of popular votes, aggregated at the geographical level of choice, straight into R. Available levels are 

* national
* cantons
* districts
* municipalities

The package wraps the real time data on vote Sundays. As soon as the ballot close (from 12:00 on), the datastream is continuosly updated, until the data for all municipalities is complete and the final results are available. Additionally, it allows to access the archive and to retrieve the *harmonized* results of national votes since 1981. Thanks to a major contribution of [David Zumbach](https://github.com/zumbov2) the latest version contains new functions to retrieve geodata of administrative boundaries and plot vote results maps.

It also allows to retrieve data from the [swissvotes-database](https://swissvotes.ch/page/home), the most  comprehensive database on swiss popular votes.

The documentation is available here: https://politanch.github.io/swissdd/

The webservice of the FSO is documented on opendata.swiss (language settings can be found at the bottom of the page):

https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen

https://opendata.swiss/de/dataset/echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen

## More data on Swiss politics 
- [DigDemLab](https://digdemlab.io/)  
- [swissparl](https://github.com/zumbov2/swissparl)
- [swissvotes](https://swissvotes.ch/)
