#' Transform a opendata.swiss national results json into a tibble
#'
#' \code{swiss_json_to_dfr} Transforms the json containing the results of a selected federal votedate into a tibble.
#'
#' @param votedate date of the ballot. Default: most recent ballot available. To select multiple ballots use the 'get_swissvotes'-function. Format = YYYYMMDD
#' @param geolevel geographical level for which the results should be loaded. options "national", "canton", "district" or "municipality"
#' @param dataurl url of the dataset on opendata-swiss
#' @param index selection by index of the resource (last published = 1).
#' @importFrom purrr map_dfr
#' @importFrom purrr map_chr
#' @importFrom purrr map
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_cols 
#' @importFrom tidyr unnest
#' @importFrom tidyr unpack
#' @return a tibble containing the results
#' @export
#' @examples
#'  \donttest{
#'
#' # transform the json of the most recent vote
#' results <- swiss_json_to_dfr()
#'
#' glimpse(results)
#' 
#' # transform the json of a selected votedate
#' 
#' swiss_json_to_dfr(votedate = "2019-02-10")
#'
#'
#' }

swiss_json_to_dfr <- function(votedate=NULL,geolevel="municipality",dataurl=NULL,index=NULL){
  
  # if no urls-list is passed on to the function (allows to use function without wrapper)
  if(is.null(dataurl)) {urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")} else {urls <- dataurl}
  
  if (is.null(votedate)&is.null(index)) {
    selection <- 1
  }
  if (!is.null(index)&is.null(votedate)) {
    selection <- index
  }
  
  #index des Abstimmungssonntags
  if(!is.null(votedate)) selection <- match(as.Date(votedate),swissdd::available_votedates())
  
  # retrieve data - switch to httr , remove supressWarnings! ----
  
  # data <- suppressWarnings(jsonlite::fromJSON(paste0("https://www.bfs.admin.ch",damlink)))
  
  # get index of date that has been chosen and select position of url
  data <- suppressWarnings(jsonlite::fromJSON(urls$result$resources$download_url[selection]))
  
  # swiss results
  
  if(geolevel=="national"){
    
    findata <- tibble::tibble(
      id = data$schweiz$vorlagen$vorlagenId,
      name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1))) %>%
      dplyr::bind_cols(data$schweiz$vorlagen$resultat)
    
  }
  
  
  #cantonal results
  
  if(geolevel=="canton"){
    
    findata <- tibble::tibble(
      canton_id = purrr::map(data$schweiz$vorlagen$kantone, 1),
      canton_name = purrr::map(data$schweiz$vorlagen$kantone, 2),
      name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1)),
      id = data$schweiz$vorlagen$vorlagenId,
      res = purrr::map(data$schweiz$vorlagen$kantone,3)
    ) %>% tidyr::unnest(c(canton_id,canton_name,res))
    
  }
  
  
  #district results
  
  if(geolevel == "district"){
    
    
    #reduce to tibble
    datas <- tibble::tibble(
      name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1)),
      id = data$schweiz$vorlagen$vorlagenId,
      canton_id = purrr::map(data$schweiz$vorlagen$kantone, 1),
      canton_name = purrr::map(data$schweiz$vorlagen$kantone, 2),
      res = purrr::map(data$schweiz$vorlagen$kantone,4)
    )
    
    findata <- datas %>%
      # unnest list columns
      tidyr::unnest(c(res,canton_id,canton_name)) %>% 
      tidyr::unnest(res) %>% 
      dplyr::rename(district_id=geoLevelnummer,district_name=geoLevelname) %>% 
      # unpack dataframe column containing the results
      tidyr::unpack(resultat) 
    
  }
  
  # parse data for counting districts level, if available
  
  if(geolevel=="zh_counting_districts" & is.list(data$schweiz$vorlagen$kantone[[1]]$zaehlkreise)){
    
    zaehlkreise <-  tibble::tibble(
      name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1)),
      id = data$schweiz$vorlagen$vorlagenId,
      canton_id = "1",
      canton_name = "Zuerich",
      res = purrr::map(data$schweiz$vorlagen$kantone,6)
    ) %>% 
      tidyr::unnest(res) %>% 
      tidyr::unnest(res) %>% 
      tidyr::unpack(resultat) %>% 
      dplyr::rename(mun_id=geoLevelnummer,mun_name=geoLevelname)
    
  }  
  
  #municipal results
  
  if(geolevel %in% c("municipality","zh_counting_districts")){   
    
    
    #reduce to tibble
    datas <-  tibble::tibble(
      name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1)),
      id = data$schweiz$vorlagen$vorlagenId,
      canton_id = purrr::map(data$schweiz$vorlagen$kantone, 1),
      canton_name = purrr::map(data$schweiz$vorlagen$kantone, 2),
      res = purrr::map(data$schweiz$vorlagen$kantone,5)
    )
    
    findata  <- datas %>%
      tidyr::unnest(c(canton_id,canton_name,res)) %>% 
      tidyr::unnest(res) %>% 
      # unpack dataframe column
      tidyr::unpack(resultat) %>% 
      dplyr::rename(mun_id=geoLevelnummer,
                    mun_name=geoLevelname)
    
    
    # add results for counting districts 
    if(geolevel=="zh_counting_districts"& is.list(data$schweiz$vorlagen$kantone[[1]]$zaehlkreise)){
      
      
      #remove winterthur and zurich as entire municipalities, add counting district data
      findata <-  findata %>% 
        dplyr::filter(!(mun_id%in%c(261,230))) %>% 
        #add counting district level results instead
        dplyr::bind_rows(zaehlkreise)
      
    }
    
    
  }
  
  return(findata)
  
}


#' Transform a opendata.swiss cantonal results json into a tibble
#'
#' \code{canton_json_to_dfr} Tranforms a single results json for a selected cantonal votedate into a tibble.
#'
#' @param votedate date of the ballot. Default: most recent ballot available.
#' @param geolevel geographical level for which the results should be loaded. options."canton","district" or "municipality"
#' @param dataurl list of datasets / metadata for the given dataset and its resources OR url of the dcat dataset on opendata.swiss
#' @param index selection by index of the resource (last published = 1).
#' @importFrom purrr map_dfr
#' @importFrom purrr map_chr
#' @importFrom purrr map
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @importFrom tidyr unpack
#' @rdname canton_json_to_dfr
#' @details placeholder
#' @return a tibble containing the results
#' @export
#' @examples
#'  \donttest{
#'
#'#most recent vote
#' results <- canton_json_to_dfr()
#'
#' glimpse(results)
#' 
#' # transform the json for a single votedate at counting district level
#' canton_json_to_dfr(votedate="2019-09-01",geolevel = "zh_counting_districts")
#'
#'
#' }
#'

canton_json_to_dfr <- function(votedate=NULL,geolevel="municipality",dataurl=NULL,index=NULL){
  
  # if no urls-list is passed on to the function (allows to use the function without wrapper)
  if(is.null(dataurl)) {urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen")} else {urls <- dataurl}
  
  if(is.null(votedate)) {selection <- 1}
  
  if (is.null(votedate)&is.null(index)) {
    selection <- 1
  }
  if (!is.null(index)&is.null(votedate)) {
    selection <- index
  }
  
  #index des Abstimmungssonntags
  if(!is.null(votedate)) selection <- match(as.Date(votedate),swissdd::available_votedates(geolevel="canton"))
  
  # retrieve data - switch to httr !------------
  
  data <- suppressWarnings(jsonlite::fromJSON(urls$result$resources$download_url[selection]))
  
  if(geolevel=="canton"){
    
    #gesamtkanton
    ktdata2 <-tibble::tibble(
      canton_name = data$kantone$geoLevelname,
      id=purrr::map(data$kantone$vorlagen,1),
      resultat=purrr::map(data$kantone$vorlagen,"resultat")
    ) %>% tidyr::unnest(c(id,resultat))
    
  }
  
  if(!(geolevel=="canton")){
    ## switch geolevel---
    switch(geolevel,
           municipality={geoindex<-9} ,
           zh_counting_districts={geoindex<-9} ,
           district={geoindex<-8})
    
    ## tibble with data
    
    ktdata <-tibble::tibble(
      id = purrr::map(data$kantone$vorlagen,1),
      kanton = data$kantone$geoLevelname,
      res = purrr::map(data$kantone$vorlagen,c(geoindex))
    ) %>%  tidyr::unnest(c(id,res)) %>% 
      tidyr::unnest(res) %>% 
      tidyr::unpack(resultat)
    
    # Zaehlkreisdaten einlesen (nur falls vorhanden)
    
    if((geolevel=="zh_counting_districts" & is.list(data$kantone$vorlagen[[1]]$zaehlkreise))){
      
      zaehlkreise <-tibble::tibble(
        id = purrr::map(data$kantone$vorlagen,1),
        kanton = data$kantone$geoLevelname,
        res = purrr::map(data$kantone$vorlagen,10))%>%  
        tidyr::unnest(c(id,res)) %>% 
        tidyr::unnest(res) %>% 
        tidyr::unpack(resultat)
      
    }
    
    
  }
  
  
  if(geolevel=="district"){
    
    ktdata2 <- ktdata %>% 
      rename(district_id=geoLevelnummer,district_name=geoLevelname)
  }
  
  
  if(geolevel %in% c("municipality","zh_counting_districts")){
    
    ktdata2 <- ktdata %>% 
      rename(mun_id=geoLevelnummer,mun_name=geoLevelname)
    
  }
  
  # extract data for counting districts
  
  if(geolevel=="zh_counting_districts" & is.list(data$kantone$vorlagen[[1]]$zaehlkreise)){
    
    #remove winterthur and zurich as single municipalities
    ktdata2 <-  ktdata2 %>% 
      dplyr::filter(!(mun_id%in%c(261,230))) %>% 
      #add counting district level results instead
      dplyr::bind_rows(zaehlkreise)
    
  }
  
  # vote names in all languages
  
  canton_vote_names  <-tibble::tibble(
    id = purrr::map(data$kantone$vorlagen,1),
    yes=purrr::map(c(1:length(data$kantone$vorlagen)),
                   ~data$kantone$vorlagen[[.x]]$vorlagenTitel)) %>%
    # unnest lists with ids and the vote-names
    tidyr::unnest(c(id,yes)) %>%
    # unnest list with language versions
    tidyr::unnest(yes) %>%
    #spread to wide to join descriptions to data
    tidyr::spread(langKey,text)
  
  
  # join vote names to result
  
  ktdata3 <-ktdata2 %>% dplyr::left_join(canton_vote_names, by=c("id"="id"))
  
  return(ktdata3)
  
}






