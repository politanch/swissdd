#' Get a vector of available vote dates via `get_nationalvotes` and `get_cantonalvotes``
#'
#' \code{available_votedates} is a utility function to get the available votedates.
#'
#' available_votedates - get available votedates of federal and cantonal popular votes
#'
#' @param geolevel geographical level for which available votedates should be displayed. options "national" or "canton".
#' @param call_res result of a previous call to the base API. Optional argument.
#' 
#' @importFrom lubridate ymd
#' 
#' @export
#' 
#' @return a vector of votedates (Format: YYYY-MM-DD)
#' 
#' @examples
#' 
#' # Get vector of all available dates
#' federal_votedates <- available_votedates()
#' cantonal_votedates <- available_votedates(geolevel = "canton")
#'
available_votedates <- function(geolevel = "national", call_res) {
  
  # Check input
  check_geolevel(geolevel, available_geolevels = c("national", "canton"))

  # Retrieve dates
  resources <- get_vote_urls(geolevel = geolevel, call_res = call_res)
  votedates <- lubridate::ymd(resources[["date"]])
  
  # Return
  return(votedates)
  
}