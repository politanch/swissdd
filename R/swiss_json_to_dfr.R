#' Transform an opendata.swiss national results json into a tibble
#'
#' \code{swiss_json_to_dfr} transforms the json containing the results of a selected federal votedate into a tibble.
#'
#' @param votedate date of the ballot. Default: most recent ballot available. To select multiple ballots use the 'get_swissvotes'-function. Format = YYYYMMDD
#' @param geolevel geographical level for which the results should be loaded. Options: "national", "canton", "district" or "municipality".
#' @param dataurl url of the dataset on opendata.swiss
#' @param index selection by index of the resource (last published = 1).
#' @param language defines the language of the vote title. Options: "DE" for German, "FR" for French, "IT" for Italian or "RM" for Romansh.
#' @param call_res result of a previous call to the base API. Optional argument.
#' 
#' @importFrom httr GET http_error content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom purrr map_chr map
#' @importFrom dplyr "%>%" bind_cols rename filter bind_rows case_when
#' @importFrom tidyr unnest unpack
#' @importFrom lubridate ymd
#' 
#' @return a tibble containing the results
#' 
#' @export
#' 
#' @examples
#' 
#' # Transform the json of the most recent vote
#' results <- swiss_json_to_dfr()
#'
#' # Transform the json of a selected votedate
#' swiss_json_to_dfr(votedate = "2019-02-10")
#'
swiss_json_to_dfr <- function(votedate = NULL, geolevel = "municipality", dataurl = NULL, index = NULL, language = "DE", call_res) {
  
  # Check inputs
  check_geolevel(geolevel, available_geolevels = c("national", "canton", "district", "municipality", "zh_counting_districts"))
  check_language(language, available_languages = c("DE", "FR", "IT", "RM"))
  
  # Get URL if required
  if (is.null(dataurl)) {
    
    # Call API if required
    if (missing(call_res)) call_res <- call_api_base(geolevel = "national")
    
    # Handle votedate
    available_dates <- available_votedates(geolevel = "national", call_res)
    if (is.null(votedate)) votedate <- max(available_dates)
    votedate <- lubridate::ymd(votedate)
    check_votedate(votedate, available_dates)
    
    # Fail gracefully when available votedates cannot be parsed.
    if(length(available_dates)<1){
      
      message("Available Votedates cannot be parsed. There might be a technical issue with the opendata.swiss API.")
      
      return(invisible(NULL))
      
    }

    # Get URL
    urls <- get_vote_urls(geolevel = "national", call_res = call_res)
    dataurl <- urls[urls[["date"]] == votedate,][["download_url"]]
    
    }
  
  # Index
  if (!is.null(index)) dataurl <- dataurl[index]
  if (length(dataurl) > 1) stop("This is not a vectorised function. Only one URL can be queried at a time.")
  
  # Fetch, check and extract vote data
  res <- httr::GET(dataurl)
  res_data <- check_api_call(res)
  # res_data <- suppressWarnings(jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8")))
  
  if(!is.null(res_data)){
  
  # Simplify data
  data_national <- res_data[["schweiz"]][["vorlagen"]]
  data_cantons <- res_data[["schweiz"]][["vorlagen"]][["kantone"]]
  
  # Language index
  lang_ind <- dplyr::case_when(
    language == "DE" ~ 1,
    language == "FR" ~ 2,
    language == "IT" ~ 3,
    language == "RM" ~ 4
    )
  
  # Geolevel specific extraction
  if (geolevel == "national") {
    
    findata <- tibble::tibble(
      id = data_national[["vorlagenId"]],
      name = purrr::map_chr(data_national[["vorlagenTitel"]], c(2, lang_ind))
      ) %>%
      dplyr::bind_cols(data_national[["resultat"]])
    
    }
  if (geolevel == "canton") {
    
    findata <- tibble::tibble(
      canton_id = purrr::map(data_cantons, 1),
      canton_name = purrr::map(data_cantons, 2),
      name = purrr::map_chr(data_national[["vorlagenTitel"]], c(2, lang_ind)),
      id = data_national[["vorlagenId"]],
      res = purrr::map(data_cantons, 3)
      ) %>% 
      tidyr::unnest(c(canton_id, canton_name, res))
    
  }
  if (geolevel == "district") {
    
    findata <- tibble::tibble(
      name = purrr::map_chr(data_national[["vorlagenTitel"]], c(2, lang_ind)),
      id = data_national[["vorlagenId"]],
      canton_id = purrr::map(data_cantons, 1),
      canton_name = purrr::map(data_cantons, 2),
      res = purrr::map(res_data[["schweiz"]][["vorlagen"]][["kantone"]], "bezirke")
      ) %>%
      tidyr::unnest(c(res, canton_id, canton_name)) %>% 
      tidyr::unnest(res) %>% 
      dplyr::rename(
        district_id = geoLevelnummer, 
        district_name = geoLevelname
      ) %>%
      tidyr::unpack(resultat) 
    
  }
  if (geolevel == "zh_counting_districts" & is.list(data_cantons[[1]]$zaehlkreise)) {
    
    zaehlkreise <- tibble::tibble(
      name = purrr::map_chr(data_national[["vorlagenTitel"]], c(2, lang_ind)),
      id = data_national[["vorlagenId"]],
      canton_id = "1",
      canton_name = data_cantons[[1]][["geoLevelname"]][[1]],
      res = purrr::map(data_cantons, "zaehlkreise")
      ) %>% 
      tidyr::unnest(res) %>% 
      tidyr::unnest(res) %>% 
      tidyr::unpack(resultat) %>% 
      dplyr::rename(
        mun_id = geoLevelnummer, 
        mun_name = geoLevelname
      )
    
  }  
  if (geolevel %in% c("municipality", "zh_counting_districts")){   
    
    findata <- tibble::tibble(
      name = purrr::map_chr(data_national[["vorlagenTitel"]], c(2, lang_ind)),
      id = data_national[["vorlagenId"]],
      canton_id = purrr::map(data_cantons, 1),
      canton_name = purrr::map(data_cantons, 2),
      res = purrr::map(res_data[["schweiz"]][["vorlagen"]][["kantone"]], "gemeinden")
      ) %>%
      tidyr::unnest(c(res, canton_id, canton_name)) %>% 
      tidyr::unnest(res) %>% 
      tidyr::unpack(resultat) %>% 
      dplyr::rename(
        mun_id = geoLevelnummer, 
        mun_name = geoLevelname
        )
    
    # Add results for counting districts 
    if(geolevel == "zh_counting_districts" & is.list(data_cantons[[1]]$zaehlkreise)){
      
      findata <- findata %>%
        dplyr::filter(!mun_id %in% c(261, 230)) %>%
        dplyr::bind_rows(zaehlkreise)
      
      }
    
    
  }
  
  # Add votedate
  if (is.null(votedate)) {
    
    urls <- get_vote_urls(geolevel = "national", call_res = call_res)
    votedate <- urls[urls[["download_url"]] == dataurl,][["date"]]
    
  }
  findata$votedate <- lubridate::ymd(votedate)
  
  # Return
  return(findata)
  
  }
  
}