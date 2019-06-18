
#' Search by Coordinate
#'
#' Search for breweries near a specified coordinate.
#'
#' @concept SearchGeopoint
#'
#' @param lat Required Latitude
#' @param lng Required Longitude
#' @param radius Radius from point. Defaults to 10 miles.
#' @param unit Unit of Measure. Defaults to Miles mi Default km
#' @param withSocialAccounts Get location results with social account information included. Y N Default
#' @param withGuilds Get location results with guild information included. Y N Default
#' @param withAlternateNames Get location results with alternate name information included. Y N Default
#' @return none 
#' @export
getEverySearchGeopoint <- function(lat, lng, radius, unit, withSocialAccounts, withGuilds, withAlternateNames){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/search/geo/point"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

