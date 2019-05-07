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
#' all <- available_votedates()
#' 
#'
#'
#' }
#'


available_votedates <- function(geolevel="national", dataOwner=T){
  
  if(!geolevel %in% c("national","cantonal")) stop("geolevel must be set to either 'national' or 'cantonal'")

  #add option for cantonal votedates
  if(geolevel=="national"){
    
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")
  
  dates <- substr(urls$result$resources$name$de,21,30)
 
  }
  
  
  if(geolevel=="cantonal"){
    
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen")
    
    
  dates <- substr(urls$result$resources$name$de,21,30)
    
  }
  
  as.Date(dates,format="%d.%m.%Y")
  
  #if(dataOwner==T) message("Data provided by the Federal Statistical Office of Switzerland.")
# dates <-substring(urls$result$resources$download_url, regexpr("[0-9]{8}",urls$result$resources$download_url),regexpr("[0-9]{8}",urls$result$resources$download_url)+7)


}