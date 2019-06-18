
#' Get All Social Accounts For Beer
#'
#' Gets a listing of all social accounts for a specific beer.
#'
#' @concept BeerSocialaccount
#'
#' @param beerId The beerId
#' @return none 
#' @export
getEveryBeerSocialaccount <- function(beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/socialaccounts"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get Specific Social Account For Beer
#'
#' Gets a specific social account for a specific beer.
#'
#' @concept BeerSocialaccount
#'
#' @param socialaccountId The socialaccountId
#' @param beerId The beerId
#' @return none 
#' @export
getBeerSocialaccount <- function(socialaccountId, beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/socialaccount/:socialaccountId"

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
#' Adds a social account to an existing beer.
#'
#' @concept BeerSocialaccount
#'
#' @param beerId The beerId
#' @param socialMediaId Required ID of an existing social media site
#' @param handle Required Handle, or identifying marker, for the specific social media site
#' @return none 
#' @export
createBeerSocialaccount <- function(beerId, socialMediaId, handle){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/socialaccounts"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Update a Social Account For Beer
#'
#' Edits an existing social account for the beer.
#'
#' @concept BeerSocialaccount
#'
#' @param socialaccountId The socialaccountId
#' @param beerId The beerId
#' @param socialMediaId Required ID of an existing social media site
#' @param handle Required Handle, or identifying marker, for the specific social media site
#' @return none 
#' @export
updateBeerSocialaccount <- function(socialaccountId, beerId, socialMediaId, handle){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/socialaccount/:socialaccountId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "PUT", params)

	flattenJsonList(returnData$data)

}

#' Delete a Social Account From Beer
#'
#' Deletes a social account for the beer.
#'
#' @concept BeerSocialaccount
#'
#' @param socialaccountId The socialaccountId
#' @param beerId The beerId
#' @return none 
#' @export
deleteBeerSocialaccount <- function(socialaccountId, beerId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/:beerId/socialaccount/:socialaccountId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

