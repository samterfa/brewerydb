
#' Get All Ingredients For Beer
#'
#' Gets a listing of all ingredients in the specific beer.
#'
#' @concept BeerIngredient
#'
#' @param beerId The beerId
#' @return none 
#' @export
getEveryBeerIngredient <- function(beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/ingredients"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

