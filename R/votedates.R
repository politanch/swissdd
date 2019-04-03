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
#' @return a vector of votedates (Format: YYYYMMDD)
#' @examples
#'  \donttest{
#' 
#' # Get data for four votedates 
#' a <- available_votedates()
#' votedata <- map_dfr(a[1:4],~get_swissvotes(votedate = .x))
#'
#'
#' }
#'


available_votedates <- function(list){


urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")

dates <-substring(urls$result$resources$download_url, regexpr("[0-9]{8}",urls$result$resources$download_url),regexpr("[0-9]{8}",urls$result$resources$download_url)+7)

as.numeric(dates)


}



