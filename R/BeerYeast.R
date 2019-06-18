
#' Get All Yeasts For Beer
#'
#' Gets a listing of all yeasts in the specific beer.
#'
#' @concept BeerYeast
#'
#' @param beerId The beerId
#' @return none 
#' @export
getEveryBeerYeast <- function(beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/yeasts"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Add a Yeast
#'
#' Adds a yeast to an existing beer.
#'
#' @concept BeerYeast
#'
#' @param beerId The beerId
#' @param yeastId Required ID of an existing yeast
#' @return none 
#' @export
createBeerYeast <- function(beerId, yeastId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/yeasts"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Remove a Yeast From Beer
#'
#' Removes the given yeast from the beer.
#'
#' @concept BeerYeast
#'
#' @param yeastId The yeastId
#' @param beerId The beerId
#' @return none 
#' @export
deleteBeerYeast <- function(yeastId, beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/yeast/:yeastId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

