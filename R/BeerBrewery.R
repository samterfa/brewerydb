
#' Get All Breweries For Beer
#'
#' Gets a listing of all breweries that brew the specific beer.
#'
#' @concept BeerBrewery
#'
#' @param beerId The beerId
#' @return none 
#' @export
getEveryBeerBrewery <- function(beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/breweries"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Add a Brewery
#'
#' Adds an existing brewery to an existing beer.
#'
#' @concept BeerBrewery
#'
#' @param beerId The beerId
#' @param breweryId Required ID of an existing brewery
#' @param locationId Specific location of a brewery to add a beer to. If not set, will be added to all locations.
#' @return none 
#' @export
createBeerBrewery <- function(beerId, breweryId, locationId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/breweries"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Remove a brewery
#'
#' Removes the given brewery from the beer.
#'
#' @concept BeerBrewery
#'
#' @param breweryId The breweryId
#' @param beerId The beerId
#' @param locationId Specific location of a brewery to remove the beer from If not set, will be removed from all locations.
#' @return none 
#' @export
deleteBeerBrewery <- function(breweryId, beerId, locationId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/brewery/:breweryId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

