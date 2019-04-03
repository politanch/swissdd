#' Get national results and counting status
#'
#' \code{get_swissvotes_range} allows to select a date range to retrieve all the results of the votes that have taken place during the selected period.
#'
#'   get_swissvotes_range - retrieve vote results for national ballots at district- or municipality level for a given date range.
#'
#' @param votedate date of the ballot. Default: most recent ballot available. Format = YYYYMMDD
#' @param geolevel geographical level for which the results should be loaded. options "district" or "municipality"
#' @importFrom purrr map_dfr
#' @importFrom purrr map_chr
#' @importFrom purrr map
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @importFrom rvest html
#' @importFrom rvest html_node
#' @importFrom rvest html_attr
#' @export
#' @rdname get_swissvotes_range
#' @details placeholder
#' @return a tibble containing the results
#' @examples
#'  \donttest{
#' results <-get_swissvotes_range(geolevel="district",from_date = 20180101,to_date=20181231)
#'
#'glimpse(results)
#'
#'
#' }
#'

get_swissvotes_range <- function(geolevel = "municipality",from_date=NULL,to_date=NULL){

  #retrieve available dates
dates <- swissdd::available_votedates()

 #filter range
if(!is.null(from_date)) dates <- dates[dates>=from_date]

if(!is.null(to_date)) dates <- dates[dates<=to_date]

 #iterate over dates and create dataframe - add votedate column?
votedata <- purrr::map_dfr(dates, ~swissdd::get_swissvotes(votedate = .x,geolevel=geolevel))

votedata

}



