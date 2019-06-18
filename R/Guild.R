
#' Get All Guilds
#'
#' Gets a listing of all guilds.  One of the following attributes must be set:  name
#'
#' @concept Guild
#'
#' @param p Page Number
#' @param ids ID's of the guilds to return, comma separated. Max 10.
#' @param name Name of a guild
#' @param established Year the guild was established
#' @param since Returns everything that has been updated since that date. Max 30 days. In UNIX timestamp format.
#' @param status Status of the guild in the API
#' @param hasImages Whether the guild has an image or not Y N
#' @param order How the results should be ordered name Default description established status createDate updateDate
#' @param sort How the results should be sorted. ASC Default DESC
#' @return id The unique id of the guild.
#' @return name The name of the guild.
#' @return description The description of the guild.
#' @return website The URL to the website for the guild.
#' @return images The images object contains URLs to the logo of the guild. This will contain values for icon, medium, and large.
#' @return established The year that the guild was established.
#' @export
getEveryGuild <- function(p, ids, name, established, since, status, hasImages, order, sort){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/guilds"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get a single guild
#'
#' Gets a single Guild
#'
#' @concept Guild
#'
#' @param guildId The guildId
#' @return none 
#' @export
getGuild <- function(guildId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/guild/:guildId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Add an guild
#'
#' Adds an guild to the system.
#'
#' @concept Guild
#'
#' @param name Required Name of the guild
#' @param description Description of the guild
#' @param established Year the guild was established
#' @param website Website of the guild
#' @param image An image or logo for the guild, base64 encoded source.
#' @return none 
#' @export
createGuild <- function(name, description, established, website, image){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/guilds"

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Update an guild
#'
#' Updates an existing guild.
#'
#' @concept Guild
#'
#' @param guildId The guildId
#' @param name Required Name of the guild
#' @param description Description of the guild
#' @param established Year the guild was established
#' @param website Website of the guild
#' @param image An image or logo for the guild, base64 encoded source.
#' @return none 
#' @export
updateGuild <- function(guildId, name, description, established, website, image){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/guild/:guildId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "PUT", params)

	flattenJsonList(returnData$data)

}

#' Delete an Guild
#'
#' Deletes an existing guild
#'
#' @concept Guild
#'
#' @param guildId The guildId
#' @return none 
#' @export
deleteGuild <- function(guildId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/guild/:guildId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

