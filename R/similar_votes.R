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
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom tidyr spread
#' @importFrom stats cor
#' @importFrom stats cov
#' @export
#' @rdname similar_votes
#' @return a tibble containing the results
#' @examples
#'  \donttest{
#'  
#'  fedvotes <- get_nationalvotes(geolevel = "canton",from_date = "2010-03-07",to_date="2019-02-10")
#'  
#'  #Find correlating votes for the 'Zersiedelungsinitiative', 2019-02-10
#'  results <- similar_votes(fedvotes, id=6260)
#'  
#' 
#' #Zersiedelungsinitiative, 2019-02-10, filter stronger correlations (>0.5)
#'  results <- similar_votes(fedvotes, id=6260, from = 0.5)
#'
#' }
#'

similar_votes <- function(federalvotes=NULL, id=NULL, corr=TRUE, from=NULL, to=NULL){
  
  if(is.null(federalvotes)){
    stop("Need tibble returned by 'get_swissvotes'")
  }
  
  if(!is.null(id)){
    if(nchar(id)!=4) stop("bfnr needs to be four digits")
    to_return <- "col"
  }else{
    to_return <- "mat"
  }
  
  
  #this block may be redundant when ktid == geoLevelnummer
  var_mun <- "mun_id"%in%colnames(federalvotes)
  var_dist <- "district_id"%in%colnames(federalvotes)
  
  if(var_mun){
    
    fed <- federalvotes %>% 
      dplyr::select(id, mun_id, jaStimmenInProzent) %>%
      mutate(id=paste0("V_", id))%>%
      tidyr::spread(id, jaStimmenInProzent)%>%
      dplyr::select(-mun_id)

      }else if(var_dist){
      
      fed <- federalvotes %>% 
          dplyr::select(id, district_id, jaStimmenInProzent) %>%
          mutate(id=paste0("V_", id))%>%
          tidyr::spread(id, jaStimmenInProzent)%>%
          dplyr::select(-district_id)
        
      }else {  
    fed <- federalvotes %>% 
      dplyr::select(id,canton_id, jaStimmenInProzent) %>%
      mutate(id=paste0("V_", id))%>%
      tidyr::spread(id, jaStimmenInProzent)%>%
      dplyr::select(-canton_id)

  }
  
  if(corr==TRUE){
    fedcor <- cor(fed, use="complete.obs")
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








