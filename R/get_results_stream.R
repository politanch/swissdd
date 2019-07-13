#' Get national results and counting status in real time
#'
#' \code{get_swissvote_stream} Allows to retrieve the results and the counting status for national ballots in realtime.
#'
#'   get_swissvote_stream - retrieve real time vote results for national ballots at district- or municipality level.
#'
#' @param votedate date of the ballot. Default: most recent ballot available. To select multiple ballots use the 'get_swissvotes'-function. Format = YYYYMMDD
#' @param geolevel geographical level for which the results should be loaded. options "national", "canton", "district" or "municipality"
#' @importFrom purrr map_dfr
#' @importFrom purrr map_chr
#' @importFrom purrr map
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_cols 
#' @importFrom tidyr unnest
#' @rdname get_swissvotes_stream
#' @export
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

get_swissvotes_stream <- function(votedate=NULL,geolevel="municipality",urls){

  if(is.null(votedate)) {selection <- 1}
  
  #index des Abstimmungssonntags
  if(!is.null(votedate)) selection <- match(as.Date(votedate),swissdd::available_votedates())

  # retrieve data - switch to httr , remove supressWarnings! ----
  
 # data <- suppressWarnings(jsonlite::fromJSON(paste0("https://www.bfs.admin.ch",damlink)))
  
  # get index of date that has been chosen and select position of url
 data <- suppressWarnings(jsonlite::fromJSON(urls$result$resources$download_url[selection]))

 # swiss results

  if(geolevel=="national"){

  data <- tibble::tibble(
    id = data$schweiz$vorlagen$vorlagenId,
    name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1))) %>%
    dplyr::bind_cols(data$schweiz$vorlagen$resultat)

  }



#cantonal results

  if(geolevel=="canton"){

   data <- tibble::tibble(
      ktid = purrr::map(data$schweiz$vorlagen$kantone, 1),
      kantonname = purrr::map(data$schweiz$vorlagen$kantone, 2),
      name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1)),
      id = data$schweiz$vorlagen$vorlagenId,
      res = purrr::map(data$schweiz$vorlagen$kantone,3)
    ) %>% tidyr::unnest(ktid,kantonname,res)

  }

 
 # switch(geolevel,
 #        municipality={geoindex<-5} ,
 #        district={geoindex<-4}
 # )
 
 
 #district results

  if(geolevel == "district"){

    
    #reduce to tibble
    datas <-data$schweiz$vorlagen$vorlagenId %>% {
      tibble::tibble(
        name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1)),
        id = data$schweiz$vorlagen$vorlagenId,
        canton_id = purrr::map(data$schweiz$vorlagen$kantone, 1),
        canton_name = purrr::map(data$schweiz$vorlagen$kantone, 2),
        res = purrr::map(data$schweiz$vorlagen$kantone,4)
      )
    }
    
    
    district_data  <- datas %>%
      tidyr::unnest(res,canton_id,canton_name)
    
    data <- district_data %>%
      dplyr::mutate(
        district_id=purrr::map(gemdata$res,1),
        district_name=purrr::map(gemdata$res,2),
        results=purrr::map(gemdata$res,4),
        results2=purrr::map(gemdata$res,"resultat")
      ) %>%
      tidyr::unnest(results2,disttict_id,district_name)
  }
    
#municipal results
 
 # add district id & label

 if(geolevel == "municipality"){   


      #reduce to tibble
      datas <-data$schweiz$vorlagen$vorlagenId %>% {
        tibble::tibble(
          name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1)),
          id = data$schweiz$vorlagen$vorlagenId,
          canton_id = purrr::map(data$schweiz$vorlagen$kantone, 1),
          canton_name = purrr::map(data$schweiz$vorlagen$kantone, 2),
          res = purrr::map(data$schweiz$vorlagen$kantone,5)
        )
      }


      gemdata  <- datas %>%
        tidyr::unnest(res,canton_id,canton_name)

     data <- gemdata %>%
       dplyr::mutate(
        mun_id=purrr::map(gemdata$res,1),
        mun_name=purrr::map(gemdata$res,2),
        results=purrr::map(gemdata$res,4),
        results2=purrr::map(gemdata$res,"resultat")
      ) %>%
        tidyr::unnest(results2,mun_id,mun_name)
  }

  return(data)

}


#' Get cantonal results and counting status in real time
#'
#' \code{get_cantonalvotes_stream} is one of the two main functions of swissvote package. It allows to retrieve the results and the counting status for national ballots on vote sundays.
#'
#'   get_cantonalvotes_stream - retrieve real time vote results for cantonal ballots at district- or municipality level.
#'
#' @param votedate date of the ballot. Default: most recent ballot available.
#' @param geolevel geographical level for which the results should be loaded. options."canton","district" or "municipality"
#' @importFrom purrr map_dfr
#' @importFrom purrr map_chr
#' @importFrom purrr map
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @export
#' @rdname get_cantonalvotes_stream
#' @details placeholder
#' @return a tibble containing the results
#' @examples
#'  \donttest{
#' results <- get_cantonalvotes_stream(votedate="20191002",geolevel = "municipality")
#'
#'glimpse(results)
#'
#'
#' }
#'

get_cantonalvotes_stream <- function(votedate=NULL,geolevel="municipality",urls){

  
    if(is.null(votedate)) {selection <- 1}
  
  #index des Abstimmungssonntags
  if(!is.null(votedate)) selection <- match(as.Date(votedate),swissdd::available_votedates(geolevel="canton"))
  
  # Hier stimmt link noch -> allenfalls anpassen, falls BFS auf DAM Link umstellt
  
  
  # retrieve data - switch to httr !------------

  # fix! two dates available
  data <- suppressWarnings(jsonlite::fromJSON(urls$result$resources$download_url[selection]))


  # data <- jsonlite::fromJSON("20181125_kant_Abstimmungsresultate_ogd.json")


  if(geolevel=="canton"){

      #gesamtkanton
      ktdata2 <-tibble::tibble(
        name = data$kantone$geoLevelname,
        id=purrr::map(data$kantone$vorlagen,1),
        resultat=purrr::map(data$kantone$vorlagen,"resultat")
      ) %>% tidyr::unnest(id,resultat)

  }

  if(!(geolevel=="canton")){
    ## switch geolevel---
        switch(geolevel,
               municipality={geoindex<-9} ,
               district={geoindex<-8})

    ## tibble with data

      ktdata <-tibble::tibble(
        id = purrr::map(data$kantone$vorlagen,1),
        kanton = data$kantone$geoLevelname,
        res = purrr::map(data$kantone$vorlagen,c(geoindex))
      ) %>%  tidyr::unnest(id,res)

    }

  if(geolevel=="district"){

    ktdata2 <- tibble::tibble(
      id=ktdata$id,
      kt=ktdata$kanton,
      geoid=purrr::map(ktdata$res,1),
      geoname=purrr::map(ktdata$res,2),
      resultat=purrr::map(ktdata$res,3)) %>%
      tidyr::unnest(resultat,geoid,geoname)
  }


  if(geolevel=="municipality"){

    ktdata2 <- tibble::tibble(
      id=ktdata$id,
      kt=ktdata$kanton,
      geoid=purrr::map(ktdata$res,1),
      geoname=purrr::map(ktdata$res,2),
      district_id=purrr::map(ktdata$res,3),
      resultat=purrr::map(ktdata$res,4)) %>%
      tidyr::unnest(resultat,geoid,geoname,district_id)

  }

  # -----


  # vote names in all languages

  canton_vote_names  <-tibble::tibble(
    id = purrr::map(data$kantone$vorlagen,1),
    yes=purrr::map(c(1:length(data$kantone$vorlagen)),
            ~data$kantone$vorlagen[[.x]]$vorlagenTitel)) %>%
    # unnest lists with ids and the vote-names
    tidyr::unnest(id,yes) %>%
    # unnest list with language versions
    tidyr::unnest(yes) %>%
    #spread to wide to join descriptions to data
    tidyr::spread(langKey,text)

  # join vote names to result

  ktdata3 <-ktdata2 %>% dplyr::left_join(canton_vote_names, by=c("id"="id"))

  return(ktdata3)

}




