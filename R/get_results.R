#' Get national results and counting status for selected dates or a given period
#'
#' \code{get_swissvotes} is one of the two main functions of swissvote package. It allows to retrieve the results and the counting status for national ballots.
#'
#'   get_swissvotes - retrieve vote results for national ballots at district- or municipality level for selected dates or a given date range.
#'
#' @param votedates dates of the ballots to be selected. Default: most recent ballot available. Format = "YYYY-MM-DD"
#' @param geolevel geographical level for which the results should be loaded. options:  options: "national", "canton", "district" or "municipality"
#' @param from_date starting point in time from which vote-results should be retrived. Format = "YYYY-MM-DD"
#' @param to_date end point in time to which vote-results should be retrived. Format = "YYYY-MM-DD"
#' @importFrom purrr map_dfr
#' @importFrom purrr map_chr
#' @importFrom purrr map
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @export
#' @rdname get_swissvotes
#' @return a tibble containing the results
#' @examples
#'  \donttest{
# results <-get_swissvotes(geolevel="district",from_date = "2018-01-01",to_date="2018-12-31")
#' 
#' # Selection by enddate only
#'  get_swissvotes(to_date="1983-12-04")
#'  
#' 
#'  # Selection of a specific votedate
#'  get_swissvotes(votedates="2014-02-09")
#'
#'glimpse(results)
#'
#'
#' }
#'

get_swissvotes <- function(geolevel = "municipality",votedates=NULL,from_date=NULL,to_date=NULL){
  
  # when either range or dates are selected -> default value 
  if(is.null(from_date) &  is.null(to_date) & is.null(votedates)) {
    
    #retrieve available dates
    dates <- max(swissdd::available_votedates())
    
  }
  
  #Warning - either select by range OR specified dates
  if(!is.null(votedates) & !is.null(from_date) | !is.null(votedates) & !is.null(to_date)) warning("please choose selected dates with the 'votedates' argument OR define a range via 'from_date' / 'to_date'), not both simultaneously")
  
  
  # When range parameters haven been passed or "all" votes should be loaded
  if(!is.null(from_date) |  !is.null(to_date)) {
    
    #retrieve available dates
  dates <- swissdd::available_votedates()
    
  }
  
  
  # for selection by votedates
  if(is.null(from_date) &  is.null(to_date) & !is.null(votedates)) {
    
   # stop if votedate is not available
    if (sum(is.na(match(as.Date(votedates), available_votedates())))>0) stop("one or more votedates not found, please call available_votedates() to check which dates are available. Also check if the format is correct (YYYY-MM-DD).")
    
    dates <- votedates
    
  }

 #filter range
  if(!is.null(from_date)) dates <- dates[dates>=from_date]
  
  if(!is.null(to_date)) dates <- dates[dates<=to_date]

 #iterate over dates and create dataframe
votedata <- purrr::map_dfr(dates, ~get_swissvotes_stream(votedate = .x,geolevel=geolevel) %>% dplyr::mutate(votedate=.x))

votedata

}


#' Get cantonal results and counting status for selected dates or a given period
#'
#' \code{get_cantonalsvotes} is one of the two main functions of swissvote package. It allows to retrieve the results and the counting status for national ballots.
#'
#'   get_cantonalvotes - retrieve vote results for cantonal ballots at district- or municipality level for selected dates or a given date range.
#'
#' @param votedates dates of the ballots to be selected. Default: most recent ballot available. Format = "YYYY-MM-DD"
#' @param geolevel geographical level for which the results should be loaded.  options: "canton", "district" or "municipality"
#' @param from_date starting point in time from which vote-results should be retrived. Format = "YYYY-MM-DD"
#' @param to_date end point in time to which vote-results should be retrived. Format = "YYYY-MM-DD"
#' @importFrom purrr map_dfr
#' @importFrom purrr map_chr
#' @importFrom purrr map
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @export
#' @rdname get_cantonalvotes
#' @details placeholder
#' @return a tibble containing the results
#' @examples
#'  \donttest{
#' results <-get_cantonalvotes(geolevel="district",from_date = 20180101,to_date=20181231)
#' 
#'  get_cantonalvotes(to_date="1983-12-04")
#'  
#'  OR
#'  
#'  get_cantonalvotes(votedates="2019-02-10")
#'
#'glimpse(results)
#'
#'
#' }
#'

get_cantonalvotes <- function(geolevel = "municipality",votedates=NULL,from_date=NULL,to_date=NULL){
  
  
  # when either range or dates are selected -> defaul value = max date
  if(is.null(from_date) &  is.null(to_date)&is.null(votedates)) {
    
    #retrieve available dates
    dates <- max(swissdd::available_votedates(geolevel="canton"))
    
  }

  
  #Warning - nur daten auswählen oder votedates
  if(!is.null(votedates) & !is.null(from_date) | !is.null(votedates) & !is.null(to_date)) warning("please choose selected dates with the 'votedates' argument OR define a range via 'from_date' / 'to_date'), not both simultaneously")
  
  
  # When range parameters haven been passed or "all" votes should be loaded
  if(!is.null(from_date) |  !is.null(to_date)) {
    
    #retrieve available dates
    dates <- swissdd::available_votedates(geolevel="canton")
    
  }
  
  # available_votedates()
  
  # when keine range ausgewählt wurde
  if(is.null(from_date) &  is.null(to_date) & !is.null(votedates)) {
    
    dates <- votedates
    
  }
  
  #filter range
  if(!is.null(from_date)) dates <- dates[dates>=from_date]
  
  if(!is.null(to_date)) dates <- dates[dates<=to_date]
  
  #iterate over dates and create dataframe - add votedate column?
  votedata <- purrr::map_dfr(dates, ~get_cantonalvotes_stream(votedate = .x,geolevel=geolevel) %>% dplyr::mutate(votedate=.x))
  
  votedata
  
}
