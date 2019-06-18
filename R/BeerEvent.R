
#' Gets a listing of all events that a beer is at or has won awards at.
#'
#' @concept BeerEvent
#'
#' @param beerId The beerId
#' @param onlyWinners Only return events where the beer has won an award Y N
#' @return none 
#' @export
getEveryBeerEvent <- function(beerId, onlyWinners){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/events"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

