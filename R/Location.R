
#' Get All Locations
#'
#' Gets a listing of all locations.  One of the following attributes must be set:  locality, postalCode, region
#'
#' @concept Location
#'
#' @param p Page Number
#' @param ids ID's of the locations to return, comma separated. Max 10.
#' @param locality Locality of the location (in US, city)
#' @param region Region of the location (in US, state)
#' @param postalCode Postal Code of a location
#' @param isPrimary Whether the location is the primary one or not
#' @param inPlanning Whether the location is in planning and not open yet
#' @param isClosed Whether the location is closed.
#' @param locationType Key(s) of the type of location. Comma separated. micro macro nano prewpub production office tasting restaurant cidery meadery
#' @param countryIsoCode Two-letter country code of the location
#' @param since Returns everything that has been updated since that date. Max 30 days. In UNIX timestamp format.
#' @param status Status of the location in the API
#' @param order How the results should be ordered name Default breweryName locality region postalCode isPrimary inPlanning isClosed locationType countryIsoCode status createDate updateDate
#' @param sort How the results should be sorted. ASC Default DESC
#' @return id The unique id of the location.
#' @return name The name of the location. Typically this will be like "Main Brewery" for breweries that have one location.
#' @return streetAddress The street address of the location.
#' @return extendedAddress The extended part of the address of the location. This is normally the second line and could be something like Suite #100.
#' @return locality This is the locality / city of the location.
#' @return region This is the region / state of the location.
#' @return postalCode The postal code / zip code of the location.
#' @return phone The phone number of the location.
#' @return website The URL to the website for the location.
#' @return hoursOfOperation A combination of the hoursOfOperationExplicit and hoursOfOperationNotes field, put together in a human-readable format.
#' @return hoursOfOperationExplicitString An array of days and open times. Days may have multiple start and end time sets.
#' @return hoursOfOperationNotes A free-text field for notes about the hours of operation.
#' @return tourInfo A free-text field containing information about brewery tours at the location.
#' @return timezoneId The timezone ID for the location. Example: America/New_York
#' @return latitude The latitude of the location.
#' @return longitude The longitude of the location.
#' @return isPrimary Whether or not this location is the primary location for the associated brewery.
#' @return inPlanning Whether or not the location is in planning.
#' @return isClosed Whether or not the location is closed (this means permanently closed)
#' @return openToPublic Whether or not the location is open to the public.
#' @return locationType The key for the type of location. See the locationTypeDisplay for the full display string.
#' @return locationTypeDisplay The display string that corresponds to the locationType.
#' @return countryIsoCode The two character country code of the location. See the country field for detailed information about the country.
#' @return country Detailed information on the country. This field is an object that contains isoCode, name, displayName, isoThree, numbercode, and urlTitle.
#' @return yearOpened The year that the location opened.
#' @return breweryId The id of the associated brewery. See the brewer object for all the details on the associated brewery.
#' @return brewery All the information for the brewery associated with the location.
#' @export
getEveryLocation <- function(p, ids, locality, region, postalCode, isPrimary, inPlanning, isClosed, locationType, countryIsoCode, since, status, order, sort){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/locations"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get a single location
#'
#' Gets a single Location
#'
#' @concept Location
#'
#' @param locationId The locationId
#' @return none 
#' @export
getLocation <- function(locationId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/location/:locationId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Update an location
#'
#' Updates an existing location.
#'
#' @concept Location
#'
#' @param locationId The locationId
#' @param name Nickname for the location
#' @param streetAddress Street address
#' @param extendedAddress Extended address, such as suite or apartment number
#' @param locality Locality, or city, where the location is at
#' @param region Region, also known as state or province
#' @param postalCode Postal code
#' @param phone Phone number for the location
#' @param website Location-specific website, if different from the brewery
#' @param hoursOfOperationExplicit Explict breakdown of the hours of operation. Should be in format "ddd-hh:mm(am/pm)-hh:mm(am/pm)". Multiple days can be passed with the given format, separated by commas. Example: 'mon-8:00am-10:00pm,tue-9:00am-1:00pm' is valid.
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
updateLocation <- function(locationId, name, streetAddress, extendedAddress, locality, region, postalCode, phone, website, hoursOfOperationExplicit, hoursOfOperationNotes, tourInfo, timezoneId, latitude, longitude, isPrimary, inPlanning, isClosed, openToPublic, locationType, countryIsoCode){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/location/:locationId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "PUT", params)

	flattenJsonList(returnData$data)

}

#' Delete an Location
#'
#' Deletes an existing location
#'
#' @concept Location
#'
#' @param locationId The locationId
#' @return none 
#' @export
deleteLocation <- function(locationId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/location/:locationId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

