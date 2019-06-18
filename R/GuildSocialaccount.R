
#' Get All Social Accounts For Guild
#'
#' Gets a listing of all social accounts for a specific guild.
#'
#' @concept GuildSocialaccount
#'
#' @param guildId The guildId
#' @return none 
#' @export
getEveryGuildSocialaccount <- function(guildId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/guild/:guildId/socialaccounts"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get Specific Social Account For Guild
#'
#' Gets a specific social account for a specific guild.
#'
#' @concept GuildSocialaccount
#'
#' @param socialaccountId The socialaccountId
#' @param guildId The guildId
#' @return none 
#' @export
getGuildSocialaccount <- function(socialaccountId, guildId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/guild/:guildId/socialaccount/:socialaccountId"

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
#' Adds a social account to an existing guild.
#'
#' @concept GuildSocialaccount
#'
#' @param guildId The guildId
#' @param socialmediaId Required ID of an existing social media site
#' @param handle Required Handle, or identifying marker, for the specific social media site
#' @return none 
#' @export
createGuildSocialaccount <- function(guildId, socialmediaId, handle){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/guild/:guildId/socialaccounts"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Update a Social Account For Guild
#'
#' Edits a social account for the guild.
#'
#' @concept GuildSocialaccount
#'
#' @param socialaccountId The socialaccountId
#' @param guildId The guildId
#' @param socialmediaId Required ID of an existing social media site
#' @param handle Required Handle, or identifying marker, for the specific social media site
#' @return none 
#' @export
updateGuildSocialaccount <- function(socialaccountId, guildId, socialmediaId, handle){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/guild/:guildId/socialaccount/:socialaccountId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "PUT", params)

	flattenJsonList(returnData$data)

}

#' Deletes a Social Account For Guild
#'
#' Deletes a social account for the guild.
#'
#' @concept GuildSocialaccount
#'
#' @param socialaccountId The socialaccountId
#' @param guildId The guildId
#' @param socialmediaId Required ID of an existing social media site
#' @param handle Required Handle, or identifying marker, for the specific social media site
#' @return none 
#' @export
deleteGuildSocialaccount <- function(socialaccountId, guildId, socialmediaId, handle){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/guild/:guildId/socialaccount/:socialaccountId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

