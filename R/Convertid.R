
#' Convert IDs
#'
#' Takes a type (brewery or beer), and a comma delimited list of v1 id's and returns the mapping to corresponding v2 ids.
#'
#' @concept Convertid
#'
#' @param type Required Type of entity to convert the IDs for brewery beer
#' @param ids Required Comma delimited list of BreweryDB API v1 IDs
#' @return none 
#' @export
createConvertid <- function(type, ids){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/convertid"

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

