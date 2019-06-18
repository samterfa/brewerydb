
#' Get All Variations Of Beer
#'
#' Gets a listing of all beers that are variations of this beer.
#'
#' @concept BeerVariation
#'
#' @param beerId The beerId
#' @return none 
#' @export
getEveryBeerVariation <- function(beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/variations"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

