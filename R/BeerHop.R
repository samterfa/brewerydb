
#' Get All Hops For Beer
#'
#' Gets a listing of all hops in the specific beer.
#'
#' @concept BeerHop
#'
#' @param beerId The beerId
#' @return none 
#' @export
getEveryBeerHop <- function(beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/hops"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Add a Hop
#'
#' Adds a hop to an existing beer.
#'
#' @concept BeerHop
#'
#' @param beerId The beerId
#' @param hopId Required ID of an existing hop
#' @return none 
#' @export
createBeerHop <- function(beerId, hopId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/hops"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Remove a Hop From Beer
#'
#' Removes the given hop from the beer.
#'
#' @concept BeerHop
#'
#' @param hopId The hopId
#' @param beerId The beerId
#' @return none 
#' @export
deleteBeerHop <- function(hopId, beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/hop/:hopId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

