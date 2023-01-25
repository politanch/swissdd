#' Get Cantonal Election Results
#' 
#' \code{get_cantonalvotes} Retrieves cantonal election results
#'
#' @param geolevel geographical level for which the results should be loaded. Options: "national", "canton", "district", "municipality" or "zh_counting_districts". 
#'
#' @importFrom httr GET http_error content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr "%>%" filter bind_rows rename left_join
#' @importFrom tidyr unnest unpack
#' 
#' @return tibble
#' 
#' @export
#'
#' @examples
#' # get cantonal level results
#' get_cantonalelection()
#' 
#' # get cantonal level results
#' get_cantonalelection(geolevel="district")


get_cantonalelections <- function(geolevel="canton"){
  
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
  
  if (!(geolevel == "canton")) {
    
  switch(
      geolevel,
      municipality = {geoindex <- "gemeinden"},
      zh_counting_districts = {geoindex <- "gemeinden"},
      district = {geoindex <- "wahlkreise"}
    )
    
    
  dataset <- tibble::tibble(
      id = purrr::map(datajson[["vorlagen"]], 1),
      canton_name = datajson[["geoLevelname"]],
      res = purrr::map(datajson[["vorlagen"]], geoindex)) %>% 
      tidyr::unnest(c(id,res)) %>% 
      tidyr::unnest(res) %>% 
      tidyr::unpack(resultat) %>% 
      tidyr::unnest(listen)
    
  
 if (geolevel == "zh_counting_districts") {

    
zaehlkreis_data <- tibble::tibble(
  id = purrr::map(datajson[["vorlagen"]], 1),
  canton_name = datajson[["geoLevelname"]],
  res = purrr::map(datajson[["vorlagen"]], "zaehlkreise")) %>% 
  tidyr::unnest(c(id, res)) %>% 
  tidyr::unnest(res) %>% 
  tidyr::unpack(resultat) %>% 
  tidyr::unnest(listen)

    
dataset <-dataset %>% 
      dplyr::bind_rows(zaehlkreis_data)
    
    
      }
  
  }
  
return(dataset)
  
}



