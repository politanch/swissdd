#' Get a vector of available vote dates
#'
#' \code{available_votedates} is a utility function to get the available votedates.
#'
#'   get_swissvotes - retrieve real time vote results for national ballots at district- or municipality level.
#'
#' @param votedate date of the ballot. Default: most recent ballot available. Format = YYYYMMDD
#' @param geolevel geographical level for which the results should be loaded. options "district" or "municipality"
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


available_votedates <- function(list){

  
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")
  
  dates <- substr(urls$result$resources$name$de,21,30)

  
  as.Date(dates,format="%d.%m.%Y")
  
# dates <-substring(urls$result$resources$download_url, regexpr("[0-9]{8}",urls$result$resources$download_url),regexpr("[0-9]{8}",urls$result$resources$download_url)+7)


}



