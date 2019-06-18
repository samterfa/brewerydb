
#' Gets all the events a brewery is present at.
#'
#' @concept BreweryEvent
#'
#' @param breweryId The breweryId
#' @param onlyWinners Display only events where the brewery has won an award. Y N
#' @return none 
#' @export
getEveryBreweryEvent <- function(breweryId, onlyWinners){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/events"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

