#' Get national results and counting status in real time or for selected dates or a time range in the past
#'
#' \code{get_nationalvotes} is one of the two main functions of swissvote package. It allows to retrieve the results and the counting status for national ballots.
#'
#'   get_nationalvotes - retrieve vote results for national ballots at district- or municipality level for selected dates or a given date range.
#'
#' @param votedates dates of the ballots to be selected. Default: most recent ballot available. Format = "YYYY-MM-DD"
#' @param geolevel geographical level for which the results should be loaded. options:  options: "national", "canton", "district", "municipality" or "zh_counting_districts"
#' @param from_date starting point in time from which vote-results should be retrived. Format = "YYYY-MM-DD"
#' @param to_date end point in time to which vote-results should be retrived. Format = "YYYY-MM-DD"
#' @importFrom purrr map_dfr
#' @importFrom purrr map_chr
#' @importFrom purrr map
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @export
#' @rdname get_nationalvotes
#' @return a tibble containing the results
#' @examples
#' \donttest{
# results <-get_nationalvotes(geolevel="district",from_date = "2018-01-01",to_date="2018-12-31")
#' 
#' # Selection by enddate only
#'  get_nationalvotes(to_date="1983-12-04")
#'  
#' 
#'  # Selection of a specific votedate
#'   get_nationalvotes(votedates="2014-02-09")
#'
#' }
#'


get_nationalvotes <- function(geolevel = "municipality",votedates=NULL,from_date=NULL,to_date=NULL){
  
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")
  # 
  # #Message if opendata.swiss API does not respond properly
  if(!is.list(urls)) {message("The Opendata.swiss DCAT Power API does not respond. Do you have internet-connection and an open proxy?")}

  
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")
  # 
  # #Message if opendata.swiss API does not respond properly
  if(!is.list(urls)) {message("The Opendata.swiss DCAT Power API does not respond. Do you have internet-connection and an open proxy?")}

  
  # when either range or dates are selected -> defaul value 
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
votedata <- purrr::map_dfr(dates, ~swiss_json_to_dfr(votedate = .x, geolevel=geolevel,dataurl=urls) %>% dplyr::mutate(votedate=.x))

votedata

}


#' Get national results and counting status in real time or for selected dates or a time range in the past
#'
#' \code{get_cantonalsvotes} is one of the two main functions of swissvote package. It allows to retrieve the results and the counting status for national ballots.
#'
#'   get_cantonalvotes - retrieve vote results for cantonal ballots at district- or municipality level for selected dates or a given date range.
#'
#' @param votedates dates of the ballots to be selected. Default: most recent ballot available. Format = "YYYY-MM-DD"
#' @param geolevel geographical level for which the results should be loaded.  options: "canton", "district","municipality" or "zh_counting_districts"
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
#' @return a tibble containing the results
#' @examples
#' # select by range
#' results <- get_cantonalvotes(geolevel="district",from_date = "2019-01-01",to_date="2019-12-31")
#' 
#' # select specific votedate(s)
#'  
#'  get_cantonalvotes(votedates="2019-02-10")
#'
#'
#' # get the results at counting district level
#' # yields the same result as the municipality level, with the 
#' # exception of Winterthur and Zurich,
#' # where detailed counting district results are returned instead.
#' 
#' get_cantonalvotes(votedate="2019-09-22",geolevel = "zh_counting_districts")
#'
#'
#' 
#'

get_cantonalvotes <- function(geolevel = "municipality",votedates=NULL,from_date=NULL,to_date=NULL){
  
  # anpassen
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen")
  
  #Message if opendata.swiss API does not respond properly
  if(!is.list(urls)) {message("The Opendata.swiss DCAT Power API does not respond. Do you have internet-connection and an open proxy?")}
  
  
  # when either range or dates are selected -> default value = max date
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
  votedata <- purrr::map_dfr(dates, ~canton_json_to_dfr(votedate = .x,geolevel=geolevel,urls) %>% dplyr::mutate(votedate=.x))
  
  votedata
  
}
