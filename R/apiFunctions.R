functionsUrl <- 'https://www.brewerydb.com/developers/docs/endpoint/'

# This function returns all api endpoint pages.
getAllAPIpages <- function(){
  
  require(rvest)
  require(magrittr)
  require(tidyverse)
  
  html <- read_html(paste0(functionsUrl))
  
  pages <- html %>% html_nodes('li a') %>% html_attr('href')
  
  pages <- pages[pages %>% str_which('/developers/docs/endpoint/')] %>% str_replace_all('/developers/docs/endpoint/', '')
  
  return(pages)
}

# This function generates R files with functions accessing brewerydb endpoints.
generateFunctions <- function(pages = c('beer-index'), testing = T){
  
  require(rvest)
  require(magrittr)
  require(tidyverse)
 
  for(page in pages){
  
    object <- page %>% str_replace('-', ' ') %>% str_to_title() %>% str_replace(' ', '') %>% str_replace('Index', '')
    
    html <- read_html(paste0(functionsUrl, page))
    
    # Each method becomes a function.
    apiMethods <- html %>% html_nodes('h3 span') %>% rvest::html_text()
    
    # Every other paragraph element is a function description unless there are no examples.
    functionDescriptions <- html %>% html_nodes('p') %>% rvest::html_text()
    
    # Check for no examples.
    noExamples <- html %>% html_nodes('h5') %>% rvest::html_text() %>% str_detect('No Examples') %>% any()
   
    # Check for warnings.
    warnings <- html %>% html_nodes('strong') %>% rvest::html_text() %>% str_detect('deprecated') %>% any()
    if(warnings){
        
      print(paste('Skipping', page, ': Deprecated'))
      
      next()
      
    }else{
        
      print(paste('Scraping', page))
    }
    
    #### Generate .R file containing page functions but only if not testing.
    if(!testing){
      
      if(!dir.exists('R')) dir.create('R')
      
        filepath <- paste0('R/', object, ".R")
        write_lines('\n', filepath)
    }
  
    ### Generate Roxygen Documentation and functions.
    for(i in 1:length(apiMethods)){
    
      apiMethodText <- apiMethods[[i]]
      
      # If no examples are provided functionDescriptions is half as long as normal and there are no longFunctionDescriptions given.
      if(!noExamples){
        shortFunctionDescription <- functionDescriptions[[2*i]]
        longFunctionDescription <- functionDescriptions[[2*i - 1]]
      }else{
        shortFunctionDescription <- functionDescriptions[[i]]
        longFunctionDescription <- ''
      }
      
      # Grab function parameters.
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
      additionalParams <- endpoint %>% str_count(':*Id')
     
      if(additionalParams > 0){
        
        for(j in 1:additionalParams){
          
          colonPos <- gregexpr(':', endpoint)[[1]][[j]] + 1
          idPos <- gregexpr(':*Id', endpoint)[[1]][[j]] + 1
          additionalParam <- substr(endpoint, colonPos, idPos)
          
          newParam <- tibble(Parameter = additionalParam, Description = paste('The', additionalParam), `Access Restriction` = 'none')
          
          ifelse(is.na(params), params <- newParam, params <- bind_rows(newParam, params))
        }
      }
      
      # Replace api methods with human-readable functions. getEvery(object), get(object), create(object), update(object), delete(object).
      functionName <- c('get', 'create', 'update', 'delete')[which(c('GET', 'POST', 'PUT', 'DELETE') == apiMethod)]
      if(endpoint %>% str_sub(-2, -1) != 'Id' & apiMethod != 'POST') functionName %<>% paste0('Every')
      functionName %<>% paste0(object)
      
  ##### DOCUMENTATION
      documentationText <- paste0("#' ", shortFunctionDescription, "\n#'\n")
      if(longFunctionDescription != '') documentationText %<>% paste0("#' ", longFunctionDescription, "\n#'\n")
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
       
          param <- params$Parameter[[i]] %>% str_squish() %>% str_replace(' Required', '')
       
          functionText %<>% paste0(ifelse(i == 1, param, paste0(', ', param)))
     
        }
      }     
         
      functionText %<>% paste0('){\n\n')
      
      functionText %<>% paste0('\tparams <- as.list(environment())\n\tparams <- params[params != ""]\n\n')
      
      functionText %<>% paste0('\tendpoint <- "', endpoint, '"\n\n')
      
      # Modify the endpoint as needed with additional params.
      if(additionalParams > 0){
        
        functionText %<>% paste0('\tadditionalParams <- params[1:', length(additionalParams), ']\n\n')
        
        functionText %<>% paste0('\tfor(j in 1:length(additionalParams)){\n\n\t\tendpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)\n\n\t}\n\n')
       
        functionText %<>% paste0('\tparams <- params[-(1:length(additionalParams))]\n\n')
      }
      
      # Create the API call.
      functionText %<>% paste0('\treturnData <- makeRequest(endpoint, "', apiMethod, '", params)\n\n')
  
      functionText %<>% paste0('\tflattenJsonList(returnData$data)\n\n}\n\n')
      
      if(!testing){
        write_lines(documentationText, filepath, append = T)
        write_lines(functionText, filepath, append = T)
      }
    }
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
