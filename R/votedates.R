#' Get a vector of available vote dates
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
#'  \donttest{
#' 
#' # Get vector of all available dates
#' federal_votedates <- available_votedates()
#' 
#' cantonal_votedates <- available_votedates(geolevel="canton")
#'
#' }
#'


available_votedates <- function(geolevel="national"){
  
  if(!geolevel %in% c("national","canton")) stop("geolevel must be set to either 'national' or 'canton'")

  #national votedates
  if(geolevel=="national"){
    
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")
  
  # get votedates from dcat coverage attribute
  
  dates <- urls$result$resources$coverage
 
  }
  
  #cantonal votedates
  if(geolevel=="canton"){
    
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen")
    
  dates <- urls$result$resources$coverage
    
  }
  
  as.Date(dates)
  
}