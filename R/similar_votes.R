#' Obtain similarities a vote result shares with other votes
#'
#' \code{similar_votes} allows to obtain correlations of specified vote with other votes.
#'
#'
#' @param federalvotes tibble or data.frame that is returned by 'get_swissvotes'. 
#' @param id identification number of the vote, needs four digits. Vote 626 (Zersiedelungsinitiative) needs 6260.
#' @param corr set to TRUE by default. If FALSE return the variance-covariance matrix.
#' @param from lower limit of correlations.
#' @param to upper limit of correlations.
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr select
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom tidyr spread
#' @export
#' @rdname similar_votes
#' @details placeholder
#' @return a tibble containing the results
#' @examples
#'  \donttest{
#'  federalvotes <- get_swissvotes(geolevel = "canton",from_date = "2010-03-07",to_date="2019-02-10")
#'  results <- similar_votes(federalvotes, id=6260) #Zersiedelungsinitiative, 2019-02-10
#' 
#'  results <- similar_votes(federalvotes, id=6260, from = 0.5) #Zersiedelungsinitiative, 2019-02-10
#'  
#'  OR
#'  
#'  results <- similar_votes(federalvotes, id=6260, from = 0.1, to = 0.2) #Zersiedelungsinitiative, 2019-02-10
#'
#'glimpse(results)
#'
#'
#' }
#'

similar_votes <- function(federalvotes=NULL, id=NULL, corr=T, from=NULL, to=NULL){
  
  if(is.null(federalvotes)){
    stop("Need tibble returned by 'get_swissvotes'")
  }
  
  
  if(!is.null(id)){
    if(nchar(id)!=4) stop("bfnr needs to be four digits")
    to_return <- "col"
  }else{
    to_return <- "mat"
  }
  
  fed <- federalvotes %>% 
    dplyr::select(id, ktid, jaStimmenInProzent) %>%
    mutate(id=paste0("V_", id))%>%
    tidyr::spread(id, jaStimmenInProzent)%>%
    dplyr::select(-ktid)
  
  
  if(corr==T){
    fedcor <- cor(fed)
  }else{
    if(!is.null(id)) {
      to_return <- "mat"
      message("Ignores vote id and returns full variance-covariance matrix")
    }
    fedcor <- cov(fed)
  }
  
  
  
  if(to_return=="col"){
    
    position <- grep(id, colnames(fedcor))
    
    dat <- fedcor[,position]
    dat <- dat[-position]
    
    dat <- as.data.frame(dat)%>%
      tibble::rownames_to_column("id") %>%
      mutate(id=gsub("V_", "", id))%>%
      rename(correlation=dat) %>% 
      arrange(-correlation)
    
    #if range is specified
    if(!is.null(from)){
      
      if(!is.null(to)){
        dat <- dat %>% 
          filter(correlation<=to & correlation >= from)
      }else{
        dat <- dat %>% 
          filter(correlation >= from)
      }
    }else{
      if(!is.null(to)){
        dat <- dat %>% 
          filter(correlation<=to)
      }
      
    }
    
    return(as_tibble(dat))
    
  }
  
  
  
  if(to_return=="mat"){
    
    return(as_tibble(fedcor))
  }
  

  
}








