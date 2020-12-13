#' Download additional data collected by annee politique suisse (the complete SwissVotes-Database)
#'
#' \code{get_swissvotes} downloads additional data collected by annee politique suisse. It allows for completely downloading their database. Please cite data.
#'
#' get_swissvotes - retrieve data on votes. The unit of analysis are votes.
#'
#' @param DB get database
#' @param savecitation by default = FALSE. Saves the citation within a .txt file in the working directory if TRUE.
#' @param codebook by default = FALSE. If TRUE navigates your browser to the codebook.
#' 
#' @export
#' 
#' @return a tibble containing the results
#' 
#' @examples
#'
# results <- get_swissvotes(DB=TRUE, savecitation=FALSE, codebook=FALSE)
#' 
#' # See codebook only
#' get_swissvotes(codebook=FALSE)
#' 
get_swissvotes <- function(DB = T, savecitation = F, codebook = F) {

  if (DB) {
    
    swissvotesDB <- utils::read.csv(
      "https://swissvotes.ch/page/dataset/swissvotes_dataset.csv",
      sep=";", 
      stringsAsFactors = F
      )
    
    }
  if (codebook) utils::browseURL("https://swissvotes.ch/page/dataset/codebook-de.pdf")

  if(DB) {

    message("To cite swissvotes data in publications, please use:\n
            Linder, Wolf, Christian Bolliger und Yvan Rielle (2010): 
            Verzeichnisse der Literatur, der Quellen und der Abkuerzungen fuer die Kurzbeschreibungen zu den Abstimmungen 1848-2007. 
            In: Linder, Wolf, Christian Bolliger und Yvan Rielle (Hg.): Handbuch der eidgenoessischen Volksabstimmungen 1848-2007. Bern: Haupt. S. 713-729.\n
            
            A BibTeX entry for LaTeX users is

            @Book{,
    author = {Linder, Wolf and Bolliger, Christian and Rielle, Yvan},
            title = {Handbuch der eidgenoessischen Volksabstimmungen 1848-2007},
            publisher = {Haupt, Bern},
            year = {2010},
            url = {https://swissvotes.ch},
  }")

    if(savecitation) {
      hb <- "@Book{,
        author = {Linder, Wolf and Bolliger, Christian and Rielle, Yvan},
        title = {Handbuch der eidgenoessischen Volksabstimmungen 1848-2007},
        publisher = {Haupt, Bern},
        year = {2010},
        url = {https://swissvotes.ch},
      }"


      utils::write.table(hb, "swissvotes.txt", col.names=F, row.names=F, quote=F)

    }

    #change id of ballot according swissdd codebook.

    swissvotesDB <- swissvotesDB %>%
      dplyr::mutate(anr = as.numeric(anr)*10)

    return(swissvotesDB)

  }

}

#identify non ASCII chars 
# tools::showNonASCII("@Book{,
#         author = {Linder, Wolf and Bolliger, Christian and Rielle, Yvan},
#         title = {Handbuch der eidgenoöessischen Volksabstimmungen 1848-2007},
#         publisher = {Haupt, Bern},
#         year = {2010},
#         url = {https://swissvotes.ch},
#       }")
