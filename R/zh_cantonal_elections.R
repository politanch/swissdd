#' Title
#'
#' @param geolevel 
#'
#' @return
#' @export
#'
#' @examples


get_cantonalelection <- function(geolevel=""){

# 1. results canton
resource <- "https://app-prod-static-voteinfo.s3.eu-central-1.amazonaws.com/v1/ogd/wahlen_resultate_2023_02_12.json"

# Simplification
res_data <-  suppressWarnings(jsonlite::fromJSON(httr::content(resource, as = "text", encoding = "UTF-8")))

# Simplification
data_cantons <- res_data[["kantone"]]

# Geolevel specific extraction
if (geolevel == "canton") {
  
  # extract results
  kt_results <- tibble::tibble(
    canton_name = data_cantons[["geoLevelname"]],
    id = purrr::map(data_cantons[["vorlagen"]], 1),
    resultat = purrr::map(data_cantons[["vorlagen"]], "resultat")) %>% 
    tidyr::unnest(id, resultat) %>% 
    tidyr::unnest(listen)
  
  #extract timeseries
zeitreihen <-  kt_results$zeitreihen %>%  
  tidyr::unnest(listen) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(hinweisZeitreihen=unique(.$metadaten$hinweisZeitreihen)) %>% 
  dplyr::select(-1)
  
  #join with results
kt_results_time <- kt_results %>% 
  dplyr::left_join(zeitreihen, by=c("listeNummer","listeCode"))
  
}

# function for wahlkreis / municipal data or zahlkreis data

extract_data <- function(data_cantons, geolevel) {
  
  tibble::tibble(
    id = purrr::map(data_cantons[["vorlagen"]], 1),
    canton_name = data_cantons[["geoLevelname"]],
    res = purrr::map(data_cantons[["vorlagen"]], geolevel)) %>% 
    tidyr::unnest(c(id, res)) %>% 
    tidyr::unnest(res) %>% 
    tidyr::unpack(resultat) %>% 
    tidyr::unnest(listen)
  
  
}

if (!(geolevel == "wahlkreise")) {
  ## tibble with data
  # extract results
extract_data(data_cantons, "wahlkreise")
 
}
  
if (geolevel == "gemeinden") {
  
extract_data(data_cantons, "gemeinden")

  
}

  # Zaehlkreisdaten einlesen (nur falls vorhanden)
if (geolevel == "zaehlkreise") {
  
gemeinde_data <- extract_data(data_cantons, "gemeinden") %>% 
  dplyr::filter(!geoLevelnummer %in% c(261,230))

zaehlkreis_data <- extract_data(data_cantons, "zaehlkreise") 

gemeinde_data %>% dplyr::bind_rows(zaehlkreis_data)

    
  }
  
}


  
