
#' Get Random Beer
#'
#' Gets a random active beer
#'
#' @concept BeerRandom
#'
#' @param abv ABV for a beer. Premium users may use advanced filtering. "+10" will return everything above 10%, "-10" will return everything under 10%, "8,10" will return everything between 8% and 10% inclusive.
#' @param ibu IBUs for a beer. Premium users may use advanced filtering. "+50" will return everything above 50 IBUs, "-50" will return everything less than 50 IBUs, "30,50" will return everything between 30 and 50 IBUs inclusive.
#' @param glasswareId ID for glassware
#' @param srmId ID for SRM
#' @param availableId ID for availability
#' @param styleId ID for style
#' @param isOrganic Whether the beer is certified organic or not Y N
#' @param hasLabels Whether or not to return a beer that has a label image Y N empty Default
#' @param year Year vintage of the beer. Format YYYY
#' @param withBreweries Get beer with brewery information included. Y N Default
#' @param withSocialAccounts Get beer with social account information included. Y N Default
#' @param withIngredients Get beer with ingredients information included. Y N Default
#' @return none 
#' @export
getEveryBeerRandom <- function(abv, ibu, glasswareId, srmId, availableId, styleId, isOrganic, hasLabels, year, withBreweries, withSocialAccounts, withIngredients){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/beer/random"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

