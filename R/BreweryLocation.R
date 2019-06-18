
#' Get locations for a brewery
#'
#' Gets a listing of all locations for the specified brewery.  Results will be paginated.
#'
#' @concept BreweryLocation
#'
#' @param breweryId The breweryId
#' @return none 
#' @export
getEveryBreweryLocation <- function(breweryId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/locations"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Add a new location
#'
#' Add a new Location to a brewery.  Location will be added with status of new_unverified.
#'
#' @concept BreweryLocation
#'
#' @param breweryId The breweryId
#' @param name Nickname for the location
#' @param streetAddress Street address
#' @param extendedAddress Extended address, such as suite or apartment number
#' @param locality Locality, or city, where the location is at
#' @param region Region, also known as state or province
#' @param postalCode Postal code
#' @param phone Phone number for the location
#' @param website Location-specific website, if different from the brewery
#' @param hoursOfOperationExplicitString Explict breakdown of the hours of operation. Should be in format "ddd-hh:mm(am/pm)-hh:mm(am/pm)". Multiple days can be passed with the given format, separated by commas. Example: 'mon-8:00am-10:00pm,tue-9:00am-1:00pm' is valid.
#' @param hoursOfOperationNotes Additional, non-time-related notes about the hours of operation.
#' @param tourInfo Brewery tour information for visitors of the location
#' @param timezoneId Timezone ID for the location. This value is automatically updated based on the latitude and longitude of the location, but can be passed as well. Example: America/New_York
#' @param latitude Latitude for the location
#' @param longitude Longitude for the location
#' @param isPrimary Whether or not this is the primary location for the brewery Y N Default
#' @param inPlanning Whether or not this location is in planning Y N Default
#' @param isClosed Whether or not this location is currently closed Y N Default
#' @param openToPublic Whether or not this location is open to the public Y N Default
#' @param locationType Type of location, such as micro brewery or brew pub micro macro nano prewpub production office tasting restaurant cidery meadery
#' @param countryIsoCode Required Country that the location is located in
#' @return none 
#' @export
createBreweryLocation <- function(breweryId, name, streetAddress, extendedAddress, locality, region, postalCode, phone, website, hoursOfOperationExplicitString, hoursOfOperationNotes, tourInfo, timezoneId, latitude, longitude, isPrimary, inPlanning, isClosed, openToPublic, locationType, countryIsoCode){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId/locations"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

