


#why is get_swissvotes only available for districts and municipalities

library(swissdd)

federalvotes <- get_swissvotes(geolevel = "canton", from_date = "2010-03-07", to_date = "2019-02-10")
federalvotes2 <- get_swissvotes(geolevel = "district", from_date = "2010-03-07", to_date = "2019-02-10")
federalvotes3 <- get_swissvotes(geolevel = "municipality", from_date = "2010-03-07", to_date = "2019-02-10")




a <- similar_votes(federalvotes3, id=5800, from=.4, to=.6)


a <- similar_votes(federalvotes)








teller <- function(interval=300, stopp=F, var="jaStimmenInProzent"){
  



  
  while(stopp==F){
    
    time <- Sys.time()
    federalvotes <- swissdd::get_swissvotes_stream(geolevel = "municipality")
    
    done <- federalvotes %>%
      filter(!is.na(jaStimmenInProzent)) %>%
      dplyr::select(id, geoLevelnummer, geoLevelname, var) %>% 
      mutate(merger=paste0(id, geoLevelnummer),
             time=time)
    
    
    
    
    finished <- table(!is.na(done$jaStimmenInProzent))
    
    cat("Ausgezählt:")
    print(finished)
    
    if(dim(finished)==1){
      stopp=T
    }else{
      Sys.sleep(interval)
    }
    
    
  }
  
  
  
  
  
}

 

