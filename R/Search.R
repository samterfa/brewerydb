
#' Search
#'
#' Search
#'
#' @concept Search
#'
#' @param p Page Number
#' @param q Required Query String
#' @param type Search on a specific type of data. Leave blank to search all data. brewery beer guild event
#' @param withBreweries Get results with brewery information included. Y N Default
#' @param withSocialAccounts Get brewery results with social account information included. Y N Default
#' @param withGuilds Get brewery results with guild information included. Y N Default
#' @param withLocations Get brewery results with location information included. Y N Default
#' @param withAlternateNames Get brewery results with alternate name information included. Y N Default
#' @param withIngredients Get beer results with ingredient information included. Y N Default
#' @param status System statuses of beer results to return, comma separated. verified Default update_pending Default delete_pending Default
#' @return none 
#' @export
getEverySearch <- function(p, q, type, withBreweries, withSocialAccounts, withGuilds, withLocations, withAlternateNames, withIngredients, status){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/search"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

