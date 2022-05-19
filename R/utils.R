#' @importFrom httr http_error
#'
#' @noRd
check_api_call <- function(call_res) {
  
  if (httr::http_error(call_res)){
    
    message("The API does not respond properly. Do you have an internet connection and an open proxy?")
    
    return(invisible(NULL))
    
  } else {
    
    suppressWarnings(jsonlite::fromJSON(httr::content(call_res, as = "text", encoding = "UTF-8")))
    
  }
  
}

#' @importFrom httr http_error
#'
#' @noRd
check_api_call_geo <- function(call_res) {
  
  if (!httr::http_error(call_res)){
    
    message("The API does not respond properly. Do you have an internet connection and an open proxy?")
    
    return(invisible(NULL))
    
  } 
  
}


#' @noRd
check_geolevel <- function(geolevel, available_geolevels) {
  
  if (!geolevel %in% available_geolevels) stop("Please select valid 'geolevel'.")
  
}

#' @noRd
check_votedates <- function(votedates, available_votedates) {
  
  if (sum(!votedates %in% available_votedates) > 0) message("One or more 'votedates' not found, please call available_votedates() to check which dates are available.")
  
}

#' @noRd
check_votedate <- function(votedate, available_votedates) {
  
  if (length(votedate) > 1) message("This is not a vectorised function. Only one 'votedate' can be queried at a time.")
  if (!votedate %in% available_votedates) message("Please select valid 'votedate'.")
  
}

#' @noRd
check_language <- function(language, available_languages) {
  
  if (!language %in% available_languages) stop("Please select valid 'language'.")
  
}

#' @noRd
check_measure <- function(measure, available_measures) {
  
  if (!measure %in% available_measures) stop("Please select valid 'measure'.")
  
}

#' @noRd
check_theme <- function(theme, available_themes) {
  
  if (!theme %in% available_themes) stop("Please select valid 'theme'.")
  
}

#' @importFrom httr GET
#'
#' @noRd
call_api_base <- function(geolevel = "national") {
  
  # Call
  if (geolevel == "national") res <- httr::GET("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-eidgenoessischen-abstimmungsvorlagen")
  if (geolevel == "canton") res <- httr::GET("https://opendata.swiss/api/3/action/package_show?id=echtzeitdaten-am-abstimmungstag-zu-kantonalen-abstimmungsvorlagen")
  
  # Check
  check_api_call(res)
  
  # Return
  return(res)
  
}

#' @importFrom httr GET http_error
#'
#' @noRd
call_api_geodata <- function(){
  
  # Call
  res <- httr::GET("https://opendata.swiss/api/3/action/package_show?id=geodaten-zu-den-eidgenoessischen-abstimmungsvorlagen")
  
  # Return
  return(res)
  
  
}

#' @importFrom httr content 
#' @importFrom tibble tibble 
#' @importFrom purrr map 
#'
#' @noRd
get_vote_urls <- function(geolevel = "national", call_res) {
  
  # Call API if required
  if (missing(call_res)) call_res <- call_api_base(geolevel = geolevel)
  
  # Extract content
  cnt <- httr::content(call_res)
  resources <- cnt[["result"]][["resources"]]
  # get jsons only
  
  jsons <- which(unlist(purrr::map(resources, "format")) %in% c("JSON","GeoJSON"))
  
  resources <- resources[jsons]
  
  # wrap tibble function into possibly - if a tibble cannot be created, an empty one is returned
  posstibble = purrr::possibly(.f = tibble, otherwise = tibble())
  
  # Extract URLs
  urls <- posstibble(
    date =  unlist(purrr::map(resources, "coverage")),
    pub_date =  unlist(purrr::map(resources, "issued")),
    download_url = unlist(purrr::map(resources, "download_url"))
  )
  
  # check if votedates and resource-URLs can be parsed 
  if(is.null(urls$date)) message("Votedates cannot be parsed properly. There might be an issue with the opendata.swiss-metadata API.")
  
  if(is.null(urls$download_url)) message("The download URLs cannot be parsed properly. There might be an issue with the opendata.swiss-metadata API.")
  
  
  # Return
  return(urls)
  
}

#' @importFrom dplyr mutate case_when
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual guide_legend 
#'     unit labs theme element_rect element_blank guide_colourbar
#'     element_text scale_fill_viridis_c ggtitle 
#'
#' @noRd
plot_map_national <- function(dt, lakes, legend_title, language, theme) {
  
  # Legend title
  if (legend_title == "jaStimmenInProzent") {
    
    legend_title <- dplyr::case_when(
      language == "DE" ~ "Ja in %",
      language == "FR" ~ "Oui en %",
      language == "IT" ~ "Quota del si",
      language == "RM" ~ "Pertschient vuschs affirmativas"
    )
    
    stimmbeteiligung <- round(100 * sum(dt[["eingelegteStimmzettel"]], na.rm = T) / sum(dt[["anzahlStimmberechtigte"]], na.rm = T), 1)
    
    caption_text <- dplyr::case_when(
      language == "DE" ~ paste0("Stimmbeteiligung: ", stimmbeteiligung, " %"),
      language == "FR" ~ paste0("Participation au vote: ", stimmbeteiligung, " %"),
      language == "IT" ~ paste0("Partecipazione al voto: ", stimmbeteiligung, " %"),
      language == "RM" ~ paste0("Participaziun a la votaziun: ", stimmbeteiligung, " %")
    )
  }
  if (legend_title == "stimmbeteiligungInProzent") {
    
    legend_title <- dplyr::case_when(
      language == "DE" ~ "Stimmbeteiligung in %",
      language == "FR" ~ "Participation au vote en %",
      language == "IT" ~ "Quota di partecipazione al voto",
      language == "RM" ~ "Participaziun a la votaziun"
    )
    
    ja_anteil <- round(100 * sum(dt[["jaStimmenAbsolut"]], na.rm = T) / sum(dt[["gueltigeStimmen"]], na.rm = T), 1)
    
    caption_text <- dplyr::case_when(
      language == "DE" ~ paste0("Ja: ", ja_anteil, " %"),
      language == "FR" ~ paste0("Oui: ", ja_anteil, " %"),
      language == "IT" ~ paste0("Si: ", ja_anteil, " %"),
      language == "RM" ~ paste0("Gea: ", ja_anteil, " %")
    )
  }
  
  # Base plot
  if (theme == "srf") {
    
    # Prepare data
    dt <- dt %>% 
      dplyr::mutate(
        measure = factor(dplyr::case_when(
          measure < 35 ~ "",
          measure >= 35 & measure < 40 ~ "35", 
          measure >= 40 & measure < 45 ~ "40",
          measure >= 45 & measure < 50 ~ "45",
          measure >= 50 & measure < 55 ~ "50",
          measure >= 55 & measure < 60 ~ "55",
          measure >= 60 & measure < 65 ~ "60",
          measure >= 65 ~ "65"
        ), levels = c("", "35", "40", "45", "50", "55", "60", "65")
        )
      )
    
    # Plot
    p1 <- ggplot2::ggplot(dt) +
      ggplot2::geom_sf(ggplot2::aes(fill = measure), color = "white") +
      ggplot2::scale_fill_manual(
        values = c(
          "#8d0613", "#c91022", "#f1434a", "#ff9193",
          "#91cdff", "#42a2f1", "#1a7bc5", "#105182"
        ),
        drop = F,
        name = legend_title,
        guide = ggplot2::guide_legend(
          direction = "horizontal",
          keyheight = ggplot2::unit(2, units = "mm"),
          keywidth = ggplot2::unit(c(25, rep(7, 6), 25), units = "mm"),
          title.position = "top",
          title.hjust = 0,
          label.hjust = 1,
          nrow = 1,
          byrow = T,
          reverse = T,
          label.position = "bottom",
        )
      ) +
      ggplot2::labs(
        title = unique(dt[["name"]]),
        caption = caption_text
      ) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.justification = "right",
        legend.text = ggplot2::element_text(color = "#6b6960"),
        plot.title = ggplot2::element_text(face = "bold"),
        plot.caption = ggplot2::element_text(hjust = 0, colour = "#6b6960")
      )
    
  } else {
    
    p1 <- ggplot2::ggplot(dt) +
      ggplot2::geom_sf(ggplot2::aes(fill = measure), color = "white") +
      ggplot2::scale_fill_viridis_c(
        option = theme, 
        direction = -1, 
        name = legend_title,
        guide = ggplot2::guide_colourbar(
          title.position = "top", 
          barwidth = ggplot2::unit(70, units = "mm"),
          title.hjust = 0.5,
          label.hjust = 0.5,
          ticks = FALSE
        )
      ) +
      ggplot2::ggtitle(unique(dt[["name"]])) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        legend.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "bottom"
      )
    
  }
  
  # Add lakes
  if (!is.null(lakes)) {
    
    if (theme == "srf") p1 <- p1 + ggplot2::geom_sf(data = lakes, fill = "white", color = "white")
    if (!theme == "srf") p1 <- p1 + ggplot2::geom_sf(data = lakes, fill = "#ceefff", color = "#4889c5")
    
  }
  
  # Display
  p1
  
}

#' @importFrom dplyr mutate case_when
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual guide_legend 
#'     unit labs theme element_rect element_blank guide_colourbar
#'     element_text scale_fill_viridis_c ggtitle 
#'
#' @noRd
plot_map_cantonal <- function(dt, legend_title, language, theme) {
  
  # Legend title
  if (legend_title == "jaStimmenInProzent") {
    
    legend_title <- dplyr::case_when(
      language == "DE" ~ "Ja in %",
      language == "FR" ~ "Oui en %",
      language == "IT" ~ "Quota del si",
      language == "RM" ~ "Pertschient vuschs affirmativas"
    )
    
    stimmbeteiligung <- round(100 * sum(dt[["eingelegteStimmzettel"]], na.rm = T) / sum(dt[["anzahlStimmberechtigte"]], na.rm = T), 1)
    
    caption_text <- dplyr::case_when(
      language == "DE" ~ paste0("Stimmbeteiligung: ", stimmbeteiligung, " %"),
      language == "FR" ~ paste0("Participation au vote: ", stimmbeteiligung, " %"),
      language == "IT" ~ paste0("Partecipazione al voto: ", stimmbeteiligung, " %"),
      language == "RM" ~ paste0("Participaziun a la votaziun: ", stimmbeteiligung, " %")
    )
  }
  if (legend_title == "stimmbeteiligungInProzent") {
    
    legend_title <- dplyr::case_when(
      language == "DE" ~ "Stimmbeteiligung in %",
      language == "FR" ~ "Participation au vote en %",
      language == "IT" ~ "Quota di partecipazione al voto",
      language == "RM" ~ "Participaziun a la votaziun"
    )
    
    ja_anteil <- round(100 * sum(dt[["jaStimmenAbsolut"]], na.rm = T) / sum(dt[["gueltigeStimmen"]], na.rm = T), 1)
    
    caption_text <- dplyr::case_when(
      language == "DE" ~ paste0("Ja: ", ja_anteil, " %"),
      language == "FR" ~ paste0("Oui: ", ja_anteil, " %"),
      language == "IT" ~ paste0("Si: ", ja_anteil, " %"),
      language == "RM" ~ paste0("Gea: ", ja_anteil, " %")
    )
  }
  
  # Plot title
  plot_title <- dplyr::case_when(
    language == "DE" ~ unique(dt[["de"]]),
    language == "FR" ~ unique(dt[["fr"]]),
    language == "IT" ~ unique(dt[["it"]]),
    language == "RM" ~ unique(dt[["rm"]])
  )
  
  # Remove NA
  plot_title <- plot_title[!is.na(plot_title)]
  
  # Save title if empty in chosen language
  if (plot_title %in% c("", " ")) {
    
    titles <- unique(c(dt[["de"]], dt[["fr"]], dt[["it"]], dt[["rm"]]))
    titles <- titles[!titles %in% c("", " ")]
    if (length(titles) > 0) plot_title <- titles[1]
    
  }
  
  # Add canton abbreviation to title
  plot_title <- paste0(plot_title, " (", unique(dt[["canton_name"]]), ")")
  
  # Base plot
  if (theme == "srf") {
    
    # Prepare data
    dt <- dt %>% 
      dplyr::mutate(
        measure = factor(dplyr::case_when(
          measure < 35 ~ "",
          measure >= 35 & measure < 40 ~ "35", 
          measure >= 40 & measure < 45 ~ "40",
          measure >= 45 & measure < 50 ~ "45",
          measure >= 50 & measure < 55 ~ "50",
          measure >= 55 & measure < 60 ~ "55",
          measure >= 60 & measure < 65 ~ "60",
          measure >= 65 ~ "65"
        ), levels = c("", "35", "40", "45", "50", "55", "60", "65")
        )
      )
    
    # Plot
    ggplot2::ggplot(dt) +
      ggplot2::geom_sf(ggplot2::aes(fill = measure), color = "white") +
      ggplot2::scale_fill_manual(
        values = c(
          "#8d0613", "#c91022", "#f1434a", "#ff9193",
          "#91cdff", "#42a2f1", "#1a7bc5", "#105182"
        ),
        drop = F,
        name = legend_title,
        guide = ggplot2::guide_legend(
          direction = "horizontal",
          keyheight = ggplot2::unit(2, units = "mm"),
          keywidth = ggplot2::unit(c(25, rep(7, 6), 25), units = "mm"),
          title.position = "top",
          title.hjust = 0,
          label.hjust = 1,
          nrow = 1,
          byrow = T,
          reverse = T,
          label.position = "bottom",
        )
      ) +
      ggplot2::labs(
        title = plot_title,
        caption = caption_text
      ) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.justification = "right",
        legend.text = ggplot2::element_text(color = "#6b6960"),
        plot.title = ggplot2::element_text(face = "bold"),
        plot.caption = ggplot2::element_text(hjust = 0, colour = "#6b6960")
      )
    
  } else {
    
    ggplot2::ggplot(dt) +
      ggplot2::geom_sf(ggplot2::aes(fill = measure), color = "white") +
      ggplot2::scale_fill_viridis_c(
        option = theme, 
        direction = -1, 
        name = legend_title,
        guide = ggplot2::guide_colourbar(
          title.position = "top", 
          barwidth = ggplot2::unit(70, units = "mm"),
          title.hjust = 0.5,
          label.hjust = 0.5,
          ticks = FALSE
        )
      ) +
      ggplot2::ggtitle(plot_title) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        legend.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
      )
    
  }
  
}