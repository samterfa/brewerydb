
#' Get All Fermentables For Beer
#'
#' Gets a listing of all fermentables in the specific beer.
#'
#' @concept BeerFermentable
#'
#' @param beerId The beerId
#' @return none 
#' @export
getEveryBeerFermentable <- function(beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/fermentables"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Add a Fermentable
#'
#' Adds a fermentable to an existing beer.
#'
#' @concept BeerFermentable
#'
#' @param beerId The beerId
#' @param fermentableId Required ID of an existing fermentable
#' @return none 
#' @export
createBeerFermentable <- function(beerId, fermentableId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/fermentables"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Remove a Fermentable From Beer
#'
#' Removes the given fermentable from the beer.
#'
#' @concept BeerFermentable
#'
#' @param fermentableId The fermentableId
#' @param beerId The beerId
#' @return none 
#' @export
deleteBeerFermentable <- function(fermentableId, beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/fermentable/:fermentableId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

