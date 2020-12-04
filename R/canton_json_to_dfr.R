#' Transform a opendata.swiss cantonal results json into a tibble
#'
#' \code{canton_json_to_dfr} tranforms a single results json for a selected cantonal votedate into a tibble.
#'
#' @param votedate date of the ballot. Default: most recent ballot available.
#' @param geolevel geographical level for which the results should be loaded. Options: "canton", "district" or "municipality".
#' @param dataurl list of datasets / metadata for the given dataset and its resources OR url of the dcat dataset on opendata.swiss
#' @param index selection by index of the resource (last published = 1).
#' @param call_res result of a previous call to the base API. Optional argument.
#' 
#' @importFrom httr GET http_error content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr "%>%" filter bind_rows rename left_join
#' @importFrom tidyr unnest unpack
#' @importFrom lubridate ymd
#' 
#' @return a tibble containing the results
#' 
#' @export
#' 
#' @examples
#' 
#' # Get and transform the json for the most recent vote
#' results <- canton_json_to_dfr()
#' 
#' # Get and transform the json for a single votedate at counting district level
#' canton_json_to_dfr(votedate = "2020-02-09", geolevel = "zh_counting_districts")
#'
canton_json_to_dfr <- function(votedate = NULL, geolevel = "municipality", dataurl = NULL, index = NULL, call_res) {
  
  # Check inputs
  check_geolevel(geolevel, available_geolevels = c("canton", "district", "municipality", "zh_counting_districts"))

  # Get urls
  if (is.null(dataurl)) {
    
    # Call API if required
    if (missing(call_res)) call_res <- call_api_base(geolevel = "canton")
    
    # Handle votedate
    available_dates <- available_votedates(geolevel = "canton", call_res)
    if (is.null(votedate)) votedate <- max(available_dates)
    votedate <- lubridate::ymd(votedate)
    check_votedate(votedate, available_dates)
    
    # Get URL
    urls <- get_vote_urls(geolevel = "canton", call_res = call_res)
    dataurl <- urls[urls[["date"]] == votedate,][["download_url"]]
    
    }
  
  # Index
  if (!is.null(index)) dataurl <- dataurl[index]
  if (length(dataurl) > 1) stop("This is not a vectorised function. Only one URL can be queried at a time.")
  
  # Fetch, check and extract vote data
  res <- httr::GET(dataurl)
  check_api_call(res)
  res_data <- suppressWarnings(jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8")))
  
  # Simplification
  data_cantons <- res_data[["kantone"]]
  
  # Geolevel specific extraction
  if (geolevel == "canton") {
    
    ktdata2 <- tibble::tibble(
      canton_name = data_cantons[["geoLevelname"]],
      id = purrr::map(data_cantons[["vorlagen"]], 1),
      resultat = purrr::map(data_cantons[["vorlagen"]], "resultat")
      ) %>% 
      tidyr::unnest(c(id, resultat))
    
  }
  if (!(geolevel == "canton")) {
    
    # Switch
    switch(
      geolevel,
      municipality = {geoindex <- "gemeinden"},
      zh_counting_districts = {geoindex <- "gemeinden"},
      district = {geoindex <- "bezirke"}
      )
    
    ## tibble with data
    ktdata <- tibble::tibble(
      id = purrr::map(data_cantons[["vorlagen"]], 1),
      canton_name = data_cantons[["geoLevelname"]],
      res = purrr::map(data_cantons[["vorlagen"]], geoindex)
      ) %>%  
      tidyr::unnest(c(id, res)) %>% 
      tidyr::unnest(res) %>% 
      tidyr::unpack(resultat)
    
    # Zaehlkreisdaten einlesen (nur falls vorhanden)
    if (geolevel == "zh_counting_districts" & is.list(data_cantons$vorlagen[[1]]$zaehlkreise)) {
      
      zaehlkreise <- tibble::tibble(
        id = purrr::map(data_cantons[["vorlagen"]], 1),
        canton_name = data_cantons[["geoLevelname"]],
        res = purrr::map(data_cantons[["vorlagen"]], "zaehlkreise")
        ) %>% 
        tidyr::unnest(c(id, res)) %>% 
        tidyr::unnest(res) %>% 
        tidyr::unpack(resultat)
      
    }
    
  }
  if (geolevel == "district") ktdata2 <- ktdata %>% dplyr::rename(district_id = geoLevelnummer, district_name = geoLevelname)
  if (geolevel %in% c("municipality","zh_counting_districts")) ktdata2 <- ktdata %>% dplyr::rename(mun_id = geoLevelnummer, mun_name = geoLevelname)
  if (geolevel == "zh_counting_districts" & is.list(data_cantons$vorlagen[[1]]$zaehlkreise)) {
    
    # remove winterthur and zurich as single municipalities
    ktdata2 <-  ktdata2 %>% 
      dplyr::filter(!mun_id %in% c(261,230)) %>% 
      dplyr::bind_rows(zaehlkreise %>% dplyr::rename(mun_id = geoLevelnummer, mun_name = geoLevelname))
    
  }
  
  # vote names in all languages
  canton_vote_names <- tibble::tibble(
    id = purrr::map(data_cantons[["vorlagen"]], 1),
    yes = purrr::map(c(1:length(data_cantons[["vorlagen"]])), ~data_cantons[["vorlagen"]][[.x]]$vorlagenTitel)
    ) %>%
    # unnest lists with ids and the vote-names
    tidyr::unnest(c(id, yes)) %>%
    # unnest list with language versions
    tidyr::unnest(yes) %>%
    #spread to wide to join descriptions to data
    tidyr::spread(langKey, text)
  
  # join vote names to result
  ktdata3 <- ktdata2 %>% dplyr::left_join(canton_vote_names, by = "id")
  
  # Add votedate
  if (is.null(votedate)) {
    
    urls <- get_vote_urls(geolevel = "canton", call_res = call_res)
    votedate <- urls[urls[["download_url"]] == dataurl,][["date"]]
    
  }
  ktdata3$votedate <- lubridate::ymd(votedate)
  
  # Return
  return(ktdata3)
  
  }