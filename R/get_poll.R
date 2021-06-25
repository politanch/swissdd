#' Download poll data collected after a national vote by gfs.bern and the political science departements of the universities of Berne, Zurich, and Geneva. 
#'
#' \code{get_poll} downloads exit poll data Please cite data.
#'
#' get_poll - retrieve poll data on votes. The unit of analysis are individuals.
#'
#' @param bfsnr number of identification of the vote, by default = NULL. Polls available after September 2020 (from voteid 6360 onwards). Bfsnr corresponds to anr in swissvotes data and has to be four digits (available through get_swissvotes).
#' @param codebook by default = FALSE. If TRUE navigates your browser to the codebook if available.
#' @importFrom RCurl getBinaryURL
#' @export
#' 
#' @return a tibble containing the results
#' 
#' @examples
#' results <- get_poll(bfsnr=6360, codebook=FALSE)

get_poll <- function(bfsnr = NULL, codebook = F) {

  if(is.null(bfsnr)) stop("Identifier number of vote (voteid) has to be specified. See for the variable `anr` obtained through swissdd::get_swissvotes ")
  if(class(bfsnr)=="numeric"){
    if(nchar(bfsnr)<4) stop("vote identifier has to have four digits, for example 6360")
    if(bfsnr<6360) stop("Polls are available from November 2020 onwards. Hence, bfsnr cannot be smaller than 6360.")
  }
  
  #create download url
  if(nchar(bfsnr/10)==3) voteid <- paste0((bfsnr/10),".00")
  if(nchar(bfsnr/10)==5) voteid <- paste0((bfsnr/10),"0")
  download_url <- paste0("https://swissvotes.ch/vote/", voteid, "/nachbefragung.csv")
  
  safe_csv <-purrr::possibly(utils::read.csv, otherwise=tibble())
  
  #download data
  exitpoll <- suppressWarnings(safe_csv(
    file=download_url,
    sep=",", 
    header=T,
    fileEncoding="latin1",
    stringsAsFactors = F
  ))
  
  
  
 
  
  if (codebook) {
    codebook_url <- z <- vector("list", 2)
    codebook_url[[1]] <- paste0("https://swissvotes.ch/vote/", voteid, "/nachbefragung-codebuch-de.xlsx")
    codebook_url[[2]] <- paste0("https://swissvotes.ch/vote/", voteid, "/nachbefragung-codebuch-de.pdf")
    
    #test which version is available (xlsx or pdf)
    for(i in 1:2){
      try(z[[i]] <- RCurl::getBinaryURL(codebook_url[[i]], failonerror = TRUE))   
    }
    
    pdf_or_xlsx <- which(!is.null(z))
    utils::browseURL(codebook_url[[pdf_or_xlsx]])
  }
  

    return(exitpoll)

}

