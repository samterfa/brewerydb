
#' Get All Social Accounts For Brewery
#'
#' Gets a listing of all social accounts for a specific brewery.
#'
#' @concept BrewerySocialaccount
#'
#' @param breweryId The breweryId
#' @return none 
#' @export
getEveryBrewerySocialaccount <- function(breweryId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/socialaccounts"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get Specific Social Account For Brewery
#'
#' Gets a specific social account for a specific brewery.
#'
#' @concept BrewerySocialaccount
#'
#' @param socialaccountId The socialaccountId
#' @param breweryId The breweryId
#' @return none 
#' @export
getBrewerySocialaccount <- function(socialaccountId, breweryId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/socialaccount/:socialaccountId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Add a Social Account
#'
#' Adds a social account to an existing brewery.
#'
#' @concept BrewerySocialaccount
#'
#' @param breweryId The breweryId
#' @param socialmediaId Required ID of an existing social media site
#' @param handle Required Handle, or identifying marker, for the specific social media site
#' @return none 
#' @export
createBrewerySocialaccount <- function(breweryId, socialmediaId, handle){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/socialaccounts"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Update a Social Account For Brewery
#'
#' Edits an existing social account for the brewery.
#'
#' @concept BrewerySocialaccount
#'
#' @param socialaccountId The socialaccountId
#' @param breweryId The breweryId
#' @param socialmediaId Required ID of an existing social media site
#' @param handle Required Handle, or identifying marker, for the specific social media site
#' @return none 
#' @export
updateBrewerySocialaccount <- function(socialaccountId, breweryId, socialmediaId, handle){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/socialaccount/:socialaccountId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "PUT", params)

	flattenJsonList(returnData$data)

}

#' Delete a Social Account From Brewery
#'
#' Deletes a social account for the brewery.
#'
#' @concept BrewerySocialaccount
#'
#' @param socialaccountId The socialaccountId
#' @param breweryId The breweryId
#' @return none 
#' @export
deleteBrewerySocialaccount <- function(socialaccountId, breweryId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/socialaccount/:socialaccountId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

