
#' downloads additional data collected by annee politique suisse. It allows to downloading their complete database.
#'
#' \code{get_swissvotes} downloads additional data collected by annee politique suisse. It allows for completely downloading their database. Please cite data.
#'
#'   get_swissvotes - retrieve data on votes. The unit of analysis are votes.
#'
#' @param DB get database
#' @param savecitation by default = FALSE. Saves the citation within a .txt file in the working directory if TRUE.
#' @param codebook by default = FALSE. If TRUE navigates your browser to the codebook.
#' @export
#' @rdname get_swissvotes
#' @return a tibble containing the results
#' @examples
#'  \donttest{
# results <-get_swissvotes(DB=T, savecitation=F, codebook=F)
#' 
#' # See codebook only
#'  get_swissvotes(codebook=T)
#'  
#' 
#'
#'glimpse(results)
#'
#'
#' }
#'
#'

get_swissvotes <- function(DB=T, savecitation=F, codebook=F){

  if(DB){
    swissvotesDB <- read.csv("https://swissvotes.ch/storage/7a811d141834e316aaae398bc298fcf46946777def27830fce919f67a28d48ee",
                             sep=";", stringsAsFactors = F)
  }


  if(codebook) browseURL("https://swissvotes.ch/storage/3ea287c640534b5ba9c483a6a8a4aa7eee4adf40c9ef85ea68b12d3384cb9cac")


  if(DB) {

    message("To cite swissvotes data in publications, please use:\n
            Linder, Wolf, Christian Bolliger und Yvan Rielle (2010): Verzeichnisse der Literatur, der Quellen und der Abkürzungen für die Kurzbeschreibungen zu den Abstimmungen 1848–2007. In: Linder, Wolf, Christian Bolliger und Yvan Rielle (Hg.): Handbuch der eidgenössischen Volksabstimmungen 1848–2007. Bern: Haupt. S. 713–729.\n

            A BibTeX entry for LaTeX users is

            @Book{,
    author = {Linder, Wolf and Bolliger, Christian and Rielle, Yvan},
            title = {Handbuch der eidgenössischen Volksabstimmungen 1848–2007},
            publisher = {Haupt, Bern},
            year = {2010},
            url = {https://swissvotes.ch},
  }")

    if(savecitation) {
      hb <- "@Book{,
        author = {Linder, Wolf and Bolliger, Christian and Rielle, Yvan},
        title = {Handbuch der eidgenössischen Volksabstimmungen 1848–2007},
        publisher = {Haupt, Bern},
        year = {2010},
        url = {https://swissvotes.ch},
      }"


      write.table(hb, "swissvotes.txt", col.names=F, row.names=F, quote=F)

    }

    #change id of ballot according swissdd codebook.

    swissvotesDB <- swissvotesDB %>%
      mutate(anr = as.numeric(anr)*10)

    return(swissvotesDB)

  }

}


