
#' Get All Breweries Alternate Names
#'
#' Gets a listing of all breweries alternate names.
#'
#' @concept BreweryAlternatename
#'
#' @param breweryId The breweryId
#' @return none 
#' @export
getEveryBreweryAlternatename <- function(breweryId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/alternatenames"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Add a Alternate Name
#'
#' Add a Alternate name to a brewery.
#'
#' @concept BreweryAlternatename
#'
#' @param breweryId The breweryId
#' @param name Required Alternate Name of the brewery
#' @return none 
#' @export
createBreweryAlternatename <- function(breweryId, name){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/alternatenames"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Delete a brewery alternate name
#'
#' Delete a breweries alternate Name.
#'
#' @concept BreweryAlternatename
#'
#' @param alternatenameId The alternatenameId
#' @param breweryId The breweryId
#' @return none 
#' @export
deleteBreweryAlternatename <- function(alternatenameId, breweryId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/alternatename/:alternatenameId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

