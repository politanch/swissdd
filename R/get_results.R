#' Get national results and counting status
#'
#' \code{get_swissvotes} is one of the two main functions of swissvote package. It allows to retrieve the results and the counting status for national ballots.
#'
#'   get_swissvotes - retrieve real time vote results for national ballots at district- or municipality level.
#'
#' @param votedate date of the ballot. Default: most recent ballot available.
#' @param geolevel geographical level for which the results should be loaded. options "district" or "municipality"
#' @importFrom purrr map_dfr
#' @importFrom purrr map_chr
#' @importFrom purrr map
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @rdname get_swissvotes
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

get_swissvotes <- function(votedate=NULL,geolevel="municipality"){

  # get urls of distributions (change link when dataset is live) - make separate function for this -------------------

  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")


  #dates <- as.Date(substr(urls$result$resources$issued,1,10))
  dates <- as.Date(substr(urls$result$temporals$start_date, 1, 10))

  # set newest votedate as default
  if (is.null(votedate)){
    votedate <- gsub("-","",max(dates))
  }
  else {
    votedate<- gsub("-","",votedate)
  }


  # retrieve data - switch to httr , remove supressWarnings! ----

 data <- suppressWarnings(jsonlite::fromJSON(urls$result$resources$download_url))

 # swiss results

  if(geolevel=="national"){

  data <- tibble::tibble(
    id = data$schweiz$vorlagen$vorlagenId,
    name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1))) %>%
    bind_cols(data$schweiz$vorlagen$resultat)

  }



  #kantonsresultate

  if(geolevel=="canton"){

   data <- tibble::tibble(
      ktid = purrr::map(data$schweiz$vorlagen$kantone, 1),
      kantonname = purrr::map(data$schweiz$vorlagen$kantone, 2),
      name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1)),
      id = data$schweiz$vorlagen$vorlagenId,
      res = purrr::map(data$schweiz$vorlagen$kantone,3)
    ) %>% tidyr::unnest(ktid,kantonname,res)

  }

  #------------------

  if(geolevel %in% c("district","municipality")){

      switch(geolevel,
             municipality={geoindex<-5} ,
             district={geoindex<-4}
      )

      #reduce to tibble
      datas <-data$schweiz$vorlagen$vorlagenId %>% {
        tibble(
          name = purrr::map_chr(data$schweiz$vorlagen$vorlagenTitel,c(2,1)),
          id = data$schweiz$vorlagen$vorlagenId,
          res = purrr::map(data$schweiz$vorlagen$kantone,geoindex)
        )
      }


      gemdata  <- datas %>%
        tidyr::unnest(res)

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




#' Get cantonal results and counting status
#'
#' \code{get_cantonalvotes} is one of the two main functions of swissvote package. It allows to retrieve the results and the counting status for national ballots.
#'
#'   get_cantonalvotes - retrieve real time vote results for cantonal ballots at district- or municipality level.
#'
#' @param votedate date of the ballot. Default: most recent ballot available.
#' @param geolevel geographical level for which the results should be loaded. options."canton","district" or "municipality"
#' @importFrom purrr map_dfr
#' @importFrom purrr map_chr
#' @importFrom purrr map
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @rdname get_cantonalvotes
#' @details placeholder
#' @return a tibble containing the results
#' @examples
#'  \donttest{
#' results <- get_cantonalvotes(votedate="20191002",geolevel = "municipality")
#'
#'glimpse(results)
#'
#'
#' }
#'

get_cantonalvotes <- function(votedate=NULL,geolevel="municipality"){

  # anpassen
  urls <- jsonlite::fromJSON("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen")

  #dates <- as.Date(substr(urls$result$resources$issued,1,10))
  dates <- as.Date(substr(urls$result$temporals$start_date, 1, 10))


  # to do - set newest votedate as default
  if (is.null(votedate)){
    votedate <- gsub("-","",max(dates))
  }
  else {
    votedate<- gsub("-","",votedate)
  }


  # retrieve data - switch to httr !------------

  data <- suppressWarnings(jsonlite::fromJSON(urls$result$resources$download_url))


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

    ktdata2 <- tibble(
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

  canton_vote_names  <-tibble(
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
# listviewer::jsonedit(data)
