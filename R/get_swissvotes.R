
#' Get national results and counting status for selected dates or a given period
#'
#' \code{get_swissvotes} downloads additional data collected by annee politique suisse. It allows for completely downloading their database. Please cite data.
#'
#'   get_apsdata - retrieve data on votes. The unit of analysis are votes.
#'
#' @param DB get database
#' @param savecitation by default = FALSE. Saves the citation within a .txt file in the working directory if TRUE.
#' @param codebook by default = FALSE. If TRUE navigates your browser to the codebook.
#' @export
#' @rdname get_apsdata
#' @return a tibble containing the results
#' @examples
#'  \donttest{
# results <-get_apsdata(DB=T, savecitation=F, codebook=F)
#' 
#' # See codebook only
#'  get_apsdata(codebook=T)
#'  
#' 
#'
#'glimpse(results)
#'
#'
#' }
#'
#'
#'
get_swissvotes <- function(DB=T, savecitation=F, codebook=F){
  
  if(DB){
    swissvotesDB <- read.csv("https://swissvotes.ch/storage/49cda3153ff1dc17b6c0a9893bc3a15b67a863e4ab64a35bc518c41964e6dc6b",
                             sep=";", stringsAsFactors = F)
  }
  
  
  if(codebook) browseURL("https://swissvotes.onegovcloud.ch/storage/3ba1067a120e2190c5a2ddcea78dc76b505639d36bfb53605c80dad0e32eeb89")
  
  
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
    
    return(swissvotesDB)
    
  }
  
}


