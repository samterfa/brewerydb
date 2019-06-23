sandboxUrl <- 'https://sandbox-api.brewerydb.com/v2'
productionUrl <- 'https://api.brewerydb.com/v2/'

checkAuthentication <- function(){
  
  require(httr)
  require(jsonlite)
  require(tidyverse)
  
  ops <- Sys.getenv()
  requiredOptions <- c('brewerydbKey', 'brewerydbKeyType')
  
  for(option in requiredOptions){
    if(Sys.getenv(option) == ''){
      if(option == 'brewerydbKey') stop(paste0('Required option ', option, ' has not been set! Set its value using Sys.setenv(', option, ' = "...").'))
      if(option == 'keyType') stop(paste0('Required option ', option, ' has not been set! Its value must be either "sandbox" or "production" depending on the type of key you use.'))
    }
  }
}


# This function consistently flattens a json list into a dataframe.
flattenJsonList <- function(jsonList){
  
  require(dplyr)
  require(jsonlite)
  
  # If jsonList is not a named list, make it one.
  if(length(names(jsonList[[1]])) == 0){ 
    
    jsonList <- list(jsonList)
    
  }
  
  for(item in jsonList){
    
    item <- data.frame(item, stringsAsFactors = F)
    
    if(!exists('df', inherits = FALSE)){
      df <- flatten(item) %>% as.data.frame()
    }else{
      df <- bind_rows(df, flatten(item) %>% as.data.frame())
    }
  }
  
  return(df)
}

makeRequest <- function(endpoint, verb, params = NULL){
  
  require(httr)
  require(tidyverse)
  require(magrittr)
  
  checkAuthentication()
  
  endpoint %<>% paste0('/?key=', Sys.getenv('brewerydbKey'))
  
  if(length(params) > 0) endpoint %<>% paste0('&', URLencode(paste(paste(names(params), as.character(params), sep = '='), collapse = '&')))
  
  keyType <- Sys.getenv('brewerydbKeyType')
  
  request <- paste0(verb, '("', ifelse(keyType == 'production', productionUrl, ifelse(keyType == 'sandbox', sandboxUrl, stop(paste('Invalid keytype', keytype)))), endpoint, '", accept_json())')
  
  eval(parse(text = paste0('response <- ', request)))
  
  if(response$status_code < 300){
    return(content(response))
  }else{
    stop(content(response))
  }
}