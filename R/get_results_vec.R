#' Get national results and counting status
#'
#' \code{get_swissvotes_wrap} is one of the two main functions of swissvote package. It allows to retrieve the results and the counting status for national ballots.
#'
#'   get_swissvotes_wrap - retrieve real time vote results for national ballots at district- or municipality level.
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
#' @rdname get_swissvotes_wrap
#' @details placeholder
#' @return a tibble containing the results
#' @examples
#'  \donttest{
#' results <- get_swissvotes(votedate="20191002", geolevel = "district")
#'
#'glimpse(results)
#'
#'
#' }
#'

get_swissvotes <- function(votedate=NULL,geolevel="municipality",from_date=NULL,to_date=NULL){

  
a <- available_votedates()

votedata <- map_dfr(a[1:4],~get_swissvotes(votedate = .x))


}