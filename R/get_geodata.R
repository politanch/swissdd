#' Get Swiss Geodata
#'
#' \code{get_geodata} retrieves the latest geodata provided by the Federal Statistical Office in connection with federal votes.
#'
#' @param geolevel geographical level. Options: "national", "canton", "district", "municipality", "zh_counting_districts" or "lakes".
#' @param latest if \code{TRUE}, the latest data is retrieved. If \code{FALSE}, geo data from the beginning of the year is retrieved. 
#'    The API does not support finer distinctions. For more detailed information on the exact status of the data, please use 
#'    \code{verbode = TRUE}.
#' @param verbose if \code{TRUE}, the date from which the data originates is displayed.
#' @param call_res result of a previous call to the geodata API. Optional argument.
#' 
#' @return a simple feature collection of the desired spatial units with corresponding IDs.
#' 
#' @importFrom httr content
#' @importFrom sf st_read st_layers
#' @importFrom dplyr rename mutate select
#' @importFrom stringr str_detect
#' 
#' @examples
#' \dontrun{
#' # Get latest geodata at municipal level
#' get_geodata()
#' 
#' # Get latest geodata at cantonal level
#' get_geodata(geolevel = "canton")
#' }
#' 
#' @export
get_geodata <- function(geolevel = "municipality", latest = T, verbose = F, call_res) {
  
  # Check input
  check_geolevel(geolevel, available_geolevels = c("national", "canton", "district", "municipality", "zh_counting_districts", "lakes"))
  
  # Call geodata api
  if (missing(call_res)) call_res <- call_api_geodata()
  
  # Check if there is an error
  if (httr::http_error(call_res)){
    
    message("The API does not respond properly. Do you have an internet connection and an open proxy?")
    
    return(invisible(NULL))
    
  }
  
  
  # check_api_call_geo(call_res)
  
  # FIX in order that function fails gracefully
  
  # if(!is.null(call_res)){
  
  cnt <- httr::content(call_res)
  # }  # Check
  
  # Extract coverage dates and download URLs from all resources.
  # The format tag is unreliable (empty for most resources), so we use all resources
  # and select by coverage date rather than issued date.
  all_res_list   <- cnt[["result"]][["resources"]]
  coverage_dates <- as.Date(unlist(purrr::map(all_res_list, "coverage")))
  download_urls  <- unlist(purrr::map(all_res_list, "download_url"))
  
  if (any(is.na(coverage_dates)) || length(download_urls) == 0) {
    message("Resource metadata cannot be parsed properly. There might be an issue with the opendata.swiss-metadata API.")
    return(invisible(NULL))
  }
  
  if (latest) {
    # Pick the resource with the most recent boundary state
    best_idx <- which(coverage_dates == max(coverage_dates, na.rm = TRUE))[1]
  } else {
    # Pick the resource whose boundary state is closest to Jan 1 of the current year
    jan1 <- as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"))
    best_idx <- which(coverage_dates == jan1)[1]
    if (is.na(best_idx)) {
      message("No boundary state found for January 1 of this year. Using the latest instead.")
      best_idx <- which(coverage_dates == max(coverage_dates, na.rm = TRUE))[1]
    }
  }
  
  gdUrl  <- download_urls[best_idx]
  gdInfo <- unlist(all_res_list[[best_idx]][["title"]])
  
  if (verbose) cat(paste0(gdInfo[!gdInfo == ""], collapse = "\n"), "\n\n")
  
  # Get layer names; return NULL silently if the resource is unavailable
  gdLayers <- suppressWarnings(tryCatch(
    sf::st_layers(gdUrl)[1][["name"]],
    error = function(e) NULL
  ))
  
  if (is.null(gdLayers)) { return(invisible(NULL)) }
  
  # Load geodata and join votes
  if (geolevel == "municipality") {
    
    # Load
  gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "voge_")], quiet = T)
    
    # Mutate if variable vogenr exists
    if ("vogeId" %in% names(gd)) {
      
      gd <- gd %>% 
        dplyr::mutate(id = vogeId) %>% 
        dplyr::select(-vogeId) 
      
    }
    
    # Adjust variable mun_id
    gd <- gd %>% 
      dplyr::rename(mun_id = id) %>% 
      dplyr::mutate(mun_id = as.character(mun_id)) %>% 
      dplyr::select(mun_id, geometry)
    
  }
  if (geolevel == "district") {
    
    # Load
    gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "bezk_")], quiet = T)
    
    # Mutate if variable vogenr exists
    if ("bezkId" %in% names(gd)) {
      
      gd <- gd %>% 
        dplyr::mutate(id = bezkId) %>% 
        dplyr::select(-bezkId) 
      
    }
    
    # Adjust variable district_id
    gd <- gd %>%
      dplyr::rename(district_id = id) %>%
      dplyr::mutate(district_id = as.character(district_id)) %>% 
      dplyr::select(district_id, geometry)
    
  }
  if (geolevel == "canton") {
    
    # Load
    gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "kant_")], quiet = T) %>%  
      dplyr::rename(canton_id = kantId) %>% 
      # dplyr::rename(canton_name = name) %>% 
      dplyr::mutate(canton_id = as.character(canton_id)) %>% 
      dplyr::select(canton_id, geometry)
    
  }
  if (geolevel == "zh_counting_districts") {
    
    gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "zaehlkreise_")], quiet = T) %>% 
      dplyr::rename(mun_id = id) %>% 
      # dplyr::rename(mun_name = name) %>% 
      dplyr::mutate(mun_id = as.character(mun_id)) %>% 
      dplyr::select(mun_id, geometry)
    
  }
  if (geolevel == "lakes") {
    
    gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "seen_")], quiet = T) %>% 
      dplyr::select(id, geometry)
    
  }
  if (geolevel == "national") {
    
    gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "suis_")], quiet = T) %>% 
      dplyr::select(id, geometry)
    
  }
  
  # Return
  return(gd)

}

