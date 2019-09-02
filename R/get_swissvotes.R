

get_swissvotes <- function(DB=T, cb=F){
  
  if(DB){
    swissvotesDB <- read.csv("https://swissvotes.ch/storage/49cda3153ff1dc17b6c0a9893bc3a15b67a863e4ab64a35bc518c41964e6dc6b",
                             sep=";", stringsAsFactors = F)
  }
  
  
  if(cb) browseURL("https://swissvotes.onegovcloud.ch/storage/3ba1067a120e2190c5a2ddcea78dc76b505639d36bfb53605c80dad0e32eeb89")
  
  
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
    
    
    return(swissvotesDB)
  }
  
}




get_swissvotes(DB=F, cb=T)




