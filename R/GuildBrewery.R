
#' Get All Breweries For Guild
#'
#' Gets a listing of all breweries that are members of a specific guild.
#'
#' @concept GuildBrewery
#'
#' @param guildId The guildId
#' @return none 
#' @export
getEveryGuildBrewery <- function(guildId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/guild/:guildId/breweries"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

