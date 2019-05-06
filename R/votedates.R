#' Get a vector of available vote dates
#'
#' \code{available_votedates} is a utility function to get the available votedates.
#'
#'   get_swissvotes - retrieve real time vote results for national ballots at district- or municipality level.
#'
#' @param geolevel geographical level for which available votedates should be displayed. options "national" or "cantonal"
#' @importFrom jsonlite fromJSON
#' @export
#' @rdname available_votedates
#' @details placeholder
#' @return a vector of votedates (Format: YYYY-MM-DD)
#' @examples
#'  \donttest{
#' 
#' # Get vector of all available dates
#' federal_votedates <- available_votedates()
#' 
#' cantonal_votedates <- available_votedates(geolevel="cantonal)
#'
#' }
#'


available_votedates <- function(geolevel="national"){
  
  # datum aus coverage attribut auslesen -> urls$result$resources$coverage
  
  if(!geolevel %in% c("national","cantonal")) stop("geolevel must be set to either 'national' or 'cantonal'")

  #add option for cantonal votedates
  if(geolevel=="national"){
    
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")
  
  # dates <- substr(urls$result$resources$name$de,21,30)
  
  dates <- urls$result$resources$coverage
 
  }
  
  
  if(geolevel=="cantonal"){
    
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen")
    
    
  # dates <- substr(urls$result$resources$name$de,21,30)
  dates <- urls$result$resources$coverage
    
  }
  
  # as.Date(dates,format="%d.%m.%Y")
  
  as.Date(dates)
  
# dates <-substring(urls$result$resources$download_url, regexpr("[0-9]{8}",urls$result$resources$download_url),regexpr("[0-9]{8}",urls$result$resources$download_url)+7)


}