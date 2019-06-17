
functionsUrl <- 'https://www.brewerydb.com/developers/docs/endpoint/'
baseUrl <- 'https://sandbox-api.brewerydb.com/v2'

checkAuthentication <- function(){
  
  require(httr)
  require(jsonlite)
  require(tidyverse)
  
  ops <- Sys.getenv()
  requiredOptions <- c("brewerydbKey")
  
  for(option in requiredOptions){
    if(Sys.getenv(option) == ''){
      stop(paste0('Required option ', option, ' has not been set! Set its value using Sys.setenv(', option, ' = "{', option, 'Value}").'))
    }
  }
}

# This function consistently flattens a json list into a dataframe.
flattenJsonList = function(jsonList){
  
  require(dplyr)
  require(jsonlite)
  
  for(item in jsonList){
    
    item = data.frame(item, stringsAsFactors = F)
    
    if(!exists('df', inherits = FALSE)){
      df <- flatten(item)
    }else{
      df <- bind_rows(df, flatten(item))
    }
  }
  return(df)
}

generateFunctions <- function(page = 'beer-index', testing = T){
  
  require(rvest)
  require(magrittr)
  require(tidyverse)
  
  # Make sure if there's an error in your code you don't get stuck with output going to a generated file instead of the console.
  on.exit({sink(); sink(); sink(); sink()})
  
  object <- page %>% str_replace('-', ' ') %>% str_to_title() %>% str_replace(' ', '') %>% str_replace('Index', '')
  
  html <- read_html(paste0(functionsUrl, page))
  
  # Each method becomes a function.
  apiMethods <- html %>% html_nodes('h3 span') %>% rvest::html_text()
  
  # Every other paragraph element is a function description.
  functionDescriptions <- html %>% html_nodes('p') %>% rvest::html_text()
  
  #### Generate .R file containing page functions but only if not testing.
  if(!testing){
    
    if(!dir.exists('R')) dir.create('R')
    
      filepath <- paste0('R/', object, ".R")
      sink(file = filepath)
      cat('\n')
  }
 
  ### Generate Roxygen Documentation and functions.
  for(i in 1:length(apiMethods)){
  
    apiMethodText <- apiMethods[[i]]
    shortFunctionDescription <- functionDescriptions[[2*i]]
    longFunctionDescription <- functionDescriptions[[2*i - 1]]
    
    # Grab function paramaters.
    toParse <- paste0("html %>% rvest::html_nodes(xpath = '//*[(@id = ", '"params_', i-1, '"', ")]') %>% html_children() %>% extract2(2)")
    params <- eval(parse(text = toParse))
    ifelse(params %>% html_name() == 'table', params %<>% html_table(), params <- NA)
    
    # Grab function return values.
    toParse <- paste0("html %>% rvest::html_nodes(xpath = '//*[(@id = ", '"return_', i-1, '"', ")]') %>% html_children() %>% extract2(2)")
    returnValues <- eval(parse(text = toParse))
    ifelse(returnValues %>% html_name() == 'table', returnValues %<>% html_table(), returnValues <- NA)
   
    # Grab api method (GET, POST, PUT, DELETE)
    apiMethod <- str_sub(apiMethodText, 1, apiMethodText %>% str_locate(':') %>% extract2(1) - 1)
    
    # Grab endpoint for request.
    endpoint <- apiMethodText %>% str_sub(apiMethodText %>% str_locate(':') %>% extract2(1) + 2, nchar(apiMethodText))
    
    # Replace api methods with human-readable functions. getEvery(object), get(object), create(object), update(object), delete(object).
    functionName <- c('get', 'create', 'update', 'delete')[which(c('GET', 'POST', 'PUT', 'DELETE') == apiMethod)]
    if(endpoint %>% str_sub(-2, -1) != 'Id' & apiMethod != 'POST') functionName %<>% paste0('Every')
    functionName %<>% paste0(object)
    
##### DOCUMENTATION
    documentationText <- paste0("#' ", shortFunctionDescription, "\n#'\n")
    documentationText %<>% paste0("#' ", longFunctionDescription, "\n#'\n")
    documentationText %<>% paste0("#' @concept ", object, "\n#'\n")
    
    # Add params to documentation.
    if(!is.na(params)){
      for(i in 1:nrow(params)){
        
        param <- params$Parameter[[i]] %>% str_squish()
        description <- params$Description[[i]] %>% str_squish()
        
        documentationText %<>% paste0("#' @param ", param, ' ', description, "\n")
      }
    }else{
      documentationText %<>% paste0("#' @param none \n")
    }
    
    # Add return values to documentation.
    if(!is.na(returnValues)){
      for(i in 1:nrow(returnValues)){
        
        returnValue <- returnValues$Name[[i]] %>% str_squish()
        description <- returnValues$Description[[i]] %>% str_squish()
        
        documentationText %<>% paste0("#' @return ", returnValue, ' ', description, "\n")
      }
    }else{
      documentationText %<>% paste0("#' @return none \n")
    }
    
    documentationText %<>% paste0("#' @export\n")
   
##### FUNCTIONS
    functionText <- paste0(functionName, ' <- function(')
    
    # Add params to functions.
    if(!is.na(params)){
      for(i in 1:nrow(params)){
     
        param <- params$Parameter[[i]] %>% str_squish() %>% str_replace('Required', '')
        
        functionText %<>% paste0(ifelse(i == 1, param, paste0(', ', param)))
   
      }
    }     
       
    functionText %<>% paste0('){\n\n')
    
    functionText %<>% paste0('\tparams <- as.list(environment())\n\tparams <- params[params != ""]\n\n')
    
    # Create the API call.
    functionText %<>% paste0('\treturnData <- makeRequest("', endpoint, '", "', apiMethod, '", params)\n\n')

    functionText %<>% paste0('\tflattenJsonList(returnData$data)\n\n}\n\n')
    
    if(!testing){
      cat(documentationText)
      cat(functionText)
    }
  }
}


makeRequest <- function(endpoint, verb, params = NULL){
  
  require(httr)
  require(tidyverse)
  require(magrittr)
  
  endpoint %<>% paste0('/?key=', key)

  if(length(params) > 0) endpoint %<>% paste0('&', paste(paste(names(params), as.character(params), sep = '='), collapse = '&'))

  request <- paste0(verb, '("', baseUrl, endpoint, '", accept_json())')

  eval(parse(text = paste0('response <- ', request)))
  
  if(response$status_code < 300){
    return(content(response))
  }else{
    stop(content(response))
  }
}

generatePkgdownYmlFile <- function(objects = c('Beer', 'Brewery')){
  
  # Update _pkgdown.yml to aid in reference navigation.
  ymlText <- paste0('url: https://samterfa.github.io/brewerydb/
                    
author: Sam Terfa
                    
reference:')
  
  for(object in objects){
    
    ymlText <- paste0(ymlText,'
 - title: ', stringr::str_to_title(object) ,'
   desc:  Functions involving ', object, '.
   contents:
   - has_concept("', object, '")')
  }
  
  if(!dir.exists('pkgdown')) dir.create('pkgdown')
  writeLines(ymlText, 'pkgdown/_pkgdown.yml')
  }
