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

get_swissvotes_stream <- function(votedate=NULL,geolevel="municipality"){

  # get urls of available distributions on opendata.swiss 

  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")

  #default when no votedate is specified : latest
  if(is.null(votedate)) {selection <- 1}
  
  #get index of the selected votedate
  if(!is.null(votedate)) selection <- match(as.Date(votedate),swissdd::available_votedates())
  
  # retrieve the data for the selected votedate
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

  # apply switch according to the chosen geographical level

  if(geolevel %in% c("district","municipality")){

      switch(geolevel,
             municipality={geoindex<-5} ,
             district={geoindex<-4}
      )

      #reduce to tibble
      datas <-data$schweiz$vorlagen$vorlagenId %>% {
        tibble::tibble(
          name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1)),
          id = data$schweiz$vorlagen$vorlagenId,
          res = purrr::map(data$schweiz$vorlagen$kantone,geoindex)
        )
      }

      # unnest columns
      gemdata  <- datas %>%
        tidyr::unnest(res)

      #compose final dataframe
     data <- gemdata %>%
       dplyr::mutate(
        geoLevelnummer=purrr::map(gemdata$res,1),
        geoLevelname=purrr::map(gemdata$res,2),
        results=purrr::map(gemdata$res,4),
        results2=purrr::map(gemdata$res,"resultat")
      ) %>%
        tidyr::unnest(results2,geoLevelnummer,geoLevelname)
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

get_cantonalvotes_stream <- function(votedate=NULL,geolevel="municipality"){

  # get urls of available distributions on opendata.swiss 
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen")

  #default when no votedate is specified : latest
    if(is.null(votedate)) {selection <- 1}
  
  #index of the selected votedate
  if(!is.null(votedate)) selection <- match(as.Date(votedate),swissdd::available_votedates(geolevel="canton"))
  

  data <- suppressWarnings(jsonlite::fromJSON(urls$result$resources$download_url[selection]))



  if(geolevel=="canton"){

      #cantonal results
      ktdata2 <-tibble::tibble(
        name = data$kantone$geoLevelname,
        id=purrr::map(data$kantone$vorlagen,1),
        resultat=purrr::map(data$kantone$vorlagen,"resultat")
      ) %>% tidyr::unnest(id,resultat)

  }

  if(!(geolevel=="canton")){
    ## switch geolevel
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




