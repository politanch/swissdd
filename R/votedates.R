#' Get a vector of available vote dates via `get_nationalvotes` and `get_cantonalvotes``
#'
#' \code{available_votedates} is a utility function to get the available votedates.
#'
#'  available_votedates - get available votedates of federal and cantonal popular votes
#'
#' @param geolevel geographical level for which available votedates should be displayed. options "national" or "canton"
#' @importFrom jsonlite fromJSON
#' @export
#' @rdname available_votedates
#' @return a vector of votedates (Format: YYYY-MM-DD)
#' @examples
#' 
#' # Get vector of all available dates
#' federal_votedates <- available_votedates()
#' 
#' cantonal_votedates <- available_votedates(geolevel="canton")
#'
#'


available_votedates <- function(geolevel="national"){
  
  # datum aus coverage attribut auslesen -> urls$result$resources$coverage
  
  if(!geolevel %in% c("national","canton")) stop("geolevel must be set to either 'national' or 'canton'")

  #add option for cantonal votedates
  if(geolevel=="national"){
    
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")
  
  # dates <- substr(urls$result$resources$name$de,21,30)
  
 
  }
  
  
  if(geolevel=="canton"){
    
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen")
    
    
  # dates <- substr(urls$result$resources$name$de,21,30)
    
  }
  
  dates <- urls$result$resources$coverage
  

  
# Convert to Date Format - fallback if coverage attribute contains no information: get date from resource title 
tryCatch(as.Date(dates), error=function(e) as.Date(substr(urls$result$resources$name$de,21,30),format="%d.%m.%Y"))
  
  
  # as.Date(dates,format="%d.%m.%Y")
  
  # as.Date(dates)
  
}