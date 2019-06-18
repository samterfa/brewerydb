
#' Get List of Adjuncts
#'
#' Gets a list of all adjuncts. Results are paginated.
#'
#' @concept Adjunct
#'
#' @param p Page Number
#' @return id The unique id of the adjunct.
#' @return name The name of the adjunct.
#' @return description The description of the adjunct.
#' @return category This value will always be set to "misc".
#' @return categoryDisplay This value will always be set to "Miscellaneous".
#' @export
getEveryAdjunct <- function(p){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/adjuncts"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get An Adjunct
#'
#' Gets a specific adjunct by ID.
#'
#' @concept Adjunct
#'
#' @param adjunctId The adjunctId
#' @return none 
#' @export
getAdjunct <- function(adjunctId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/adjunct/:adjunctId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

