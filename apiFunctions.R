
functionsUrl <- 'https://www.brewerydb.com/developers/docs/endpoint/'
baseUrl <- 'https://sandbox-api.brewerydb.com/v2/'

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

scrapeFunctions <- function(page = 'beer-index', testing = T){
  
  require(rvest)
  require(magrittr)
  require(tidyverse)
  
  # Make sure if there's an error in your code you don't get stuck with output going to a generated file instead of the console.
  on.exit({sink(); sink(); sink(); sink()})
  
  object <- page %>% str_replace('-', ' ') %>% str_to_title() %>% str_replace(' ', '') %>% str_replace('Index', '')
  
  html <- read_html(paste0(functionsUrl, page))
  
  # Each method becomes a function.
  methods <- html %>% html_nodes('h3 span') %>% rvest::html_text()
  descriptions <- html %>% html_nodes('p') %>% rvest::html_text()
  
  # Every other paragraph is the description we want.
  descriptions %<>%  magrittr::extract(2*(1:(length(methods)-1)))
  
  #### Generate .R file containing page functions.
  filepath <- paste0('R/', page, ".R")
  
  if(!testing){
    
    if(!dir.exists('R')) dir.create('R')
    
      sink(file = filepath)
      cat('\n')
    
  }
  
  for(i in 1:length(methods)){
    
    #### Scrape API information
    
    method <- methods[[i]]
    
    description <- descriptions[[i]]
    
    toParse <- paste0("html %>% rvest::html_nodes(xpath = '//*[(@id = ", '"params_', i-1, '"', ")]') %>% html_children() %>% extract2(2)")
    params <- eval(parse(text = toParse))
    if(params %>% html_name() == 'table') params <- params %>% html_table()
    
    toParse <- paste0("html %>% rvest::html_nodes(xpath = '//*[(@id = ", '"return_', i-1, '"', ")]') %>% html_children() %>% extract2(2)")
    returnValues <- eval(parse(text = toParse))
    ifelse(returnValues %>% html_name() == 'table', returnValues <- returnValues %>% html_table(), returnValues <- NA)
   
    toParse <- paste0("html %>% rvest::html_nodes(xpath = '//*[(@id = ", '"example_', i-1, '"', ")]') %>% html_children() %>% extract2(2)")
    example <- eval(parse(text = toParse))
    if(example %>% html_name() == 'table') example <- example %>% html_table()
    
    verb <- method %>% str_sub(1, method %>% str_locate(':') %>% extract2(1) - 1)
    
    endpoint <- method %>% str_sub(method %>% str_locate(':') %>% extract2(1) + 2, nchar(method))
    
    #### Generate human-readable functions
    
    functionName <- c('get', 'create', 'update', 'delete')[which(c('GET', 'POST', 'PUT', 'DELETE') == verb)]

    if(endpoint %>% str_sub(-2, -1) != 'Id' & verb != 'POST') functionName %<>% str_glue('Every')
   
    functionName %<>% str_glue(object)
    
    documentationText <- paste0("#'", descriptions[[i]], "\n#'\n")
    functionText <- paste0('\n\n\t', functionName, ' <- function(')
    
    for(i in 1:nrow(params)){
    
      param <- params$Parameter[[i]]
      description <- params$Description[[i]]
      `Access Restriction` <- params$`Access Restriction`[[i]]
      
      return(description)
    }
    
    cat(documentationText)
    cat(functionText)
  }
}


makeRequest <- function(endpoint, verb, params = NULL){
  
  require(httr)
  require(tidyverse)
  
  request <- paste0(verb, '("', baseUrl, endpoint, '/?key=', key, '", accept_json())')
  
  eval(parse(text = paste0('response <- ', request)))
  
  if(response$status_code < 300){
    return(content(response))
  }else{
    stop(content(response))
  }
}

