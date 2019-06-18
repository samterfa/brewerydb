
#' Get All Beers For Brewery
#'
#' Gets a listing of all beers for a brewery.
#'
#' @concept BreweryBeer
#'
#' @param breweryId The breweryId
#' @param withBreweries Get beer results with brewery information included. Y N Default
#' @param withSocialAccounts Get beer results with social account information included. Y N Default
#' @param withIngredients Get beer results with ingredients information included. Y N Default
#' @return none 
#' @export
getEveryBreweryBeer <- function(breweryId, withBreweries, withSocialAccounts, withIngredients){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/beers"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

