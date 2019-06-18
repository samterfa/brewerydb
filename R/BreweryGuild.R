
#' Get All Guilds
#'
#' Gets all guilds that a brewery belongs to.  Results will be paginated
#'
#' @concept BreweryGuild
#'
#' @param breweryId The breweryId
#' @param p Page Number
#' @return none 
#' @export
getEveryBreweryGuild <- function(breweryId, p){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/guilds"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Add a Guild to brewery
#'
#' Associates a guild to a specific brewery.
#'
#' @concept BreweryGuild
#'
#' @param breweryId The breweryId
#' @param guildId Required Id of the Guild
#' @param discount If the brewery offers a discount for members of this guild, put it here.
#' @return none 
#' @export
createBreweryGuild <- function(breweryId, guildId, discount){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/guilds"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Delete brewery guild association
#'
#' Delete a guild from the brewery.
#'
#' @concept BreweryGuild
#'
#' @param guildId The guildId
#' @param breweryId The breweryId
#' @return none 
#' @export
deleteBreweryGuild <- function(guildId, breweryId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/guild/:guildId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

