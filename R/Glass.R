
#' Get List of Glasses
#'
#' Gets a list of all glassware.  Results are paginated.
#'
#' @concept Glass
#'
#' @param p Page Number
#' @return id The unique id of the glass.
#' @return name The name of the glass.
#' @export
getEveryGlass <- function(p){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/glassware"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get A Glass
#'
#' Gets a specific glass by ID.
#'
#' @concept Glass
#'
#' @param glassId The glassId
#' @return none 
#' @export
getGlass <- function(glassId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/glass/:glassId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

