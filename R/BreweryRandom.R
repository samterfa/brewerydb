
#' Get Random Breweries
#'
#' Gets a random active brewery.
#'
#' @concept BreweryRandom
#'
#' @param established Year a brewery was established. Format YYYY
#' @param isOrganic Is the brewery an Organic brewery Y N Default
#' @param withSocialAccounts Get brewery with social account information included. Y N Default
#' @param withGuilds Get brewery with guild information included. Y N Default
#' @param withLocations Get brewery with location information included. Y N Default
#' @param withAlternateNames Get brewery with alternate name information included. Y N Default
#' @return none 
#' @export
getEveryBreweryRandom <- function(established, isOrganic, withSocialAccounts, withGuilds, withLocations, withAlternateNames){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/random"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

