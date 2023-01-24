#' Utility function to extract district, municipality or counting district level data from the result-Json
#'
#' @param data_cantons data slot of the json 
#' @param geovar label of the slot in the json for the geographical level for which the results should be loaded. Options: "national", "canton", "district", "municipality" or "zh_counting_districts". 
#'
#' @return tibble
#' @noRd

extract_election_data <- function(data_cantons, geovar) {
  
  dataset <- tibble::tibble(
    id = purrr::map(data_cantons[["vorlagen"]], 1),
    canton_name = data_cantons[["geoLevelname"]],
    res = purrr::map(data_cantons[["vorlagen"]], geovar)) %>% 
    tidyr::unnest(c(id, res)) %>% 
    tidyr::unnest(res) %>% 
    tidyr::unpack(resultat) %>% 
    tidyr::unnest(listen)
  
  dataset
  
}

#' Get Cantonal Election Results
#'
#' @param geolevel geographical level for which the results should be loaded. Options: "national", "canton", "district", "municipality" or "zh_counting_districts". 
#'
#' @return tibble
#' @export
#'
#' @examples
#' # get cantonal level results
#' get_cantonalelection()
#' 
#' # get cantonal level results
#' get_cantonalelection(geolevel="district")


get_cantonalelection <- function(geolevel="district"){
  
  
  check_geolevel(geolevel=geolevel, 
                 available_geolevels = c("canton","district","municipality","zh_counting_districts"))
  
  dataurl <- "https://app-prod-static-voteinfo.s3.eu-central-1.amazonaws.com/v1/ogd/wahlen_resultate_2023_02_12.json"
  
  resource <- httr::GET(dataurl)
  
  res_data <-  suppressWarnings(jsonlite::fromJSON(httr::content(resource, as = "text", encoding = "UTF-8")))
  
  # Simplification
  datajson <- res_data[["kantone"]]
  
  # Geolevel specific extraction
  if (geolevel == "canton"){
    
    # extract results
    kt_results <- tibble::tibble(
      canton_name = datajson[["geoLevelname"]],
      id = purrr::map(datajson[["vorlagen"]], 1),
      resultat = purrr::map(datajson[["vorlagen"]], "resultat")) %>% 
      tidyr::unnest(id, resultat) %>% 
      tidyr::unnest(listen)
    
    #extract timeseries
    zeitreihen <-  kt_results$zeitreihen %>%  
      tidyr::unnest(listen) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(hinweisZeitreihen=unique(.$metadaten$hinweisZeitreihen)) %>% 
      dplyr::select(-1)
    
    #join with results
    dataset <-kt_results %>% 
      dplyr::left_join(zeitreihen, by=c("listeNummer","listeCode"))
    
    
  } 
  
  if (geolevel == "district"){
    
    dataset <- extract_election_data(datajson, geovar="wahlkreise")
    
  } 
  
  if (geolevel == "municipality") {
    
    dataset <- extract_election_data(datajson, geovar="gemeinden")
    
  } 
  
  if (geolevel == "zh_counting_districts") {
    
    gemeinde_data <- extract_election_data(datajson, geovar="gemeinden") %>% 
      dplyr::filter(!geoLevelnummer %in% c(261,230))
    
    zaehlkreis_data <- extract_election_data(datajson, geovar="zaehlkreise") 
    
    dataset <- gemeinde_data %>% 
      dplyr::bind_rows(zaehlkreis_data)
    
    
  }
  
  return(dataset)
  
}



