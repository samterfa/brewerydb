
#' Add a UPC Code
#'
#' Adds a UPC code to an existing beer.
#'
#' @concept BeerUpc
#'
#' @param beerId The beerId
#' @param upcCode Required UPC code to assign to the beer
#' @param fluidSizeId ID of an existing container size
#' @return none 
#' @export
createBeerUpc <- function(beerId, upcCode, fluidSizeId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/upcs"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

