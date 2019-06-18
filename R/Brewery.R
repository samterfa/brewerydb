
#' Get All Breweries
#'
#' Gets a listing of all breweries.  Results will be paginated.  One of the following attributes must be set:  name, established
#'
#' @concept Brewery
#'
#' @param p Page Number
#' @param name Name of a brewery.
#' @param ids ID's of the breweries to return, comma separated. Max 10.
#' @param established Year a brewery was established. Format YYYY
#' @param isOrganic Is the brewery an Organic brewery Y N Default
#' @param hasImages Whether the brewery has an image or not Y N
#' @param since Returns everything that has been updated since that date. Max 30 days. In UNIX timestamp format.
#' @param status Status of the brewery in the API
#' @param order How the results should be ordered name Default description website established mailingListUrl isOrganic status createDate updateDate random
#' @param sort How the results should be sorted. ASC Default DESC
#' @param randomCount If the order parameter is set to random, this option specifies how many random breweries to return. It has a max value of 10
#' @param withSocialAccounts Get brewery results with social account information included. Y N Default
#' @param withGuilds Get brewery results with guild information included. Y N Default
#' @param withLocations Get brewery results with location information included. Y N Default
#' @param withAlternateNames Get brewery results with alternate name information included. Y N Default
#' @return id The unique id of the brewery.
#' @return name The name of the brewery.
#' @return nameShortDisplay The short name of the brewery.
#' @return isVerified Wheather or not the brewery has been claimed.
#' @return description The description of the brewery.
#' @return website The URL to the brewery's website
#' @return established The year that the brewery was established.
#' @return mailingListUrl The url to the brewery's mailing list.
#' @return isOrganic Whether or not the brewery is certified organic.
#' @return images The images field will contain the three image sizes for the brewery logos (icon, medium, and large).
#' @export
getEveryBrewery <- function(p, name, ids, established, isOrganic, hasImages, since, status, order, sort, randomCount, withSocialAccounts, withGuilds, withLocations, withAlternateNames){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/breweries"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get a single brewery
#'
#' Gets a single brewery
#'
#' @concept Brewery
#'
#' @param breweryId The breweryId
#' @param withSocialAccounts Get brewery results with social account information included. Y N Default
#' @param withGuilds Get brewery results with guild information included. Y N Default
#' @param withLocations Get brewery results with location information included. Y N Default
#' @param withAlternateNames Get brewery results with alternate name information included. Y N Default
#' @return none 
#' @export
getBrewery <- function(breweryId, withSocialAccounts, withGuilds, withLocations, withAlternateNames){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Add a new brewery
#'
#' Add a brewery.  Brewery will be added with status of new_unverified.
#'
#' @concept Brewery
#'
#' @param name Required Name of the brewery
#' @param description A little bit about the brewery
#' @param website Url of the breweries website
#' @param established Year brewery was established. Format YYYY
#' @param mailingListUrl Url of the breweries mailing list
#' @param isOrganic Y N Default
#' @param image Base64 encoded image
#' @return none 
#' @export
createBrewery <- function(name, description, website, established, mailingListUrl, isOrganic, image){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/breweries"

	returnData <- makeRequest(endpoint, "POST", params)

	flattenJsonList(returnData$data)

}

#' Update a new brewery
#'
#' Update a brewery.  Brewery will be added with status of update_pending.
#'
#' @concept Brewery
#'
#' @param breweryId The breweryId
#' @param name Name of the brewery
#' @param description A little bit about the brewery
#' @param website Url of the breweries website
#' @param established Year brewery was established. Format YYYY
#' @param mailingListUrl Url of the breweries mailing list
#' @param isOrganic Y N Default
#' @param image Base64 encoded image
#' @return none 
#' @export
updateBrewery <- function(breweryId, name, description, website, established, mailingListUrl, isOrganic, image){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "PUT", params)

	flattenJsonList(returnData$data)

}

#' Delete a new brewery
#'
#' Delete a brewery.  Brewery will be added with status of delete_pending.
#'
#' @concept Brewery
#'
#' @param breweryId The breweryId
#' @return none 
#' @export
deleteBrewery <- function(breweryId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/brewery/:breweryId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "DELETE", params)

	flattenJsonList(returnData$data)

}

