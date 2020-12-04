#' Get cantonal results and counting status in real time or for selected dates or a time range in the past
#'
#' \code{get_cantonalvotes} is one of the two main functions of swissvote package. It allows to retrieve the results and the counting status for national ballots.
#'
#' get_cantonalvotes - retrieve vote results for cantonal ballots at district- or municipality level for selected dates or a given date range.
#'
#' @param geolevel geographical level for which the results should be loaded. Options: "canton", "district", "municipality" or "zh_counting_districts".
#' @param votedates dates of the ballots to be selected. Default: most recent ballot available. Format: "YYYY-MM-DD".
#' @param from_date starting point in time from which vote results should be retrieved. Format: "YYYY-MM-DD".
#' @param to_date end point in time to which vote results should be retrieved. Format: "YYYY-MM-DD".
#' 
#' @importFrom purrr map_dfr
#' @importFrom lubridate ymd
#'  
#' @export
#' 
#' @return a tibble containing the results
#' 
#' @examples
#' 
#' # Select by range
#' results <- get_cantonalvotes(
#'    geolevel = "district", 
#'    from_date = "2019-01-01", 
#'    to_date = "2019-12-31"
#'    )
#' 
#' # Select specific votedate(s)
#' get_cantonalvotes(votedates = "2019-02-10")
#'
#' # get the results at counting district level
#' # yields the same result as the municipality level, with the 
#' # exception of Winterthur and Zurich,
#' # where detailed counting district results are returned instead.
#' 
#' get_cantonalvotes(votedate = "2019-09-22", geolevel = "zh_counting_districts")
#'
get_cantonalvotes <- function(geolevel = "municipality", votedates = NULL, from_date = NULL, to_date = NULL) {
  
  # Check inputs
  check_geolevel(geolevel, available_geolevels = c("canton", "district", "municipality", "zh_counting_districts"))
  if (!is.null(votedates) & (!is.null(from_date) | !is.null(to_date))) stop("Please select the vote dates either with 'votedates' OR via a range ('from_date' / 'to_date').")
  
  # Parse dates
  if (!is.null(votedates)) votedates <- lubridate::ymd(votedates)
  if (!is.null(from_date)) from_date <- lubridate::ymd(from_date)
  if (!is.null(to_date)) to_date <- lubridate::ymd(to_date)
  
  # Call base api
  call_res <- call_api_base(geolevel = "canton")
  available_dates <- available_votedates(geolevel = "canton", call_res)
  
  # Define vote dates to be fetched
  if (is.null(votedates) & is.null(from_date) & is.null(to_date)) votedates <- max(available_dates)
  if (!is.null(from_date) | !is.null(to_date)) {
    
    if (is.null(from_date)) from_date <- lubridate::ymd(min(available_dates))
    if (is.null(to_date)) to_date <- lubridate::ymd(max(available_dates))
    votedates <- available_dates[available_dates >= from_date & available_dates <= to_date]
    
  }
  
  # Check votedates
  check_votedates(votedates, available_dates)
  
  # Iterate over dates and create dataframe
  votedata <- purrr::map_dfr(
    votedates, 
    canton_json_to_dfr,
    geolevel = geolevel,
    call_res = call_res
    )
  
  # Return
  return(votedata)
  
}