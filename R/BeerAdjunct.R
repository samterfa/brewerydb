
#' Get All Adjuncts For Beer
#'
#' Gets a listing of all adjuncts in the specific beer.
#'
#' @concept BeerAdjunct
#'
#' @param beerId The beerId
#' @return none 
#' @export
getEveryBeerAdjunct <- function(beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/adjuncts"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Add an Adjunct
#'
#' Adds an adjunct to an existing beer.
#'
#' @concept BeerAdjunct
#'
#' @param beerId The beerId
#' @param adjunctId Required ID of an existing adjunct
#' @return none 
#' @export
createBeerAdjunct <- function(beerId, adjunctId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/adjuncts"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Remove an Adjunct From Beer
#'
#' Removes the given adjunct from the beer.
#'
#' @concept BeerAdjunct
#'
#' @param adjunctId The adjunctId
#' @param beerId The beerId
#' @return none 
#' @export
deleteBeerAdjunct <- function(adjunctId, beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/adjunct/:adjunctId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

