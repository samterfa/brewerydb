
#' Gets a listing of beer styles
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/styles"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Gets a listing of all categories
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/categories"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Gets a listing of all glassware
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/glassware"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Gets a listing of all SRM values
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/srm"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Gets a listing of all beer availability values
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/beer-availability"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Gets a listing of all fluid sizes
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/fluidsize"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Gets a listing of all beer temperature values
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/beer-temperature"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Gets a listing of all countries
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/countries"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Gets a listing of all ingredients
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/ingredients"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Gets a listing of all location type values
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/location-types"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Gets a listing of all fluid size volumes
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/fluidsize-volume"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Gets a listing of all event type values
#'
#' @concept Menu
#'
#' @param none 
#' @return none 
#' @export
getEveryMenu <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/menu/event-types"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

