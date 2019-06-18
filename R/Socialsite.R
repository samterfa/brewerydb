
#' Get List of Social Sites
#'
#' Gets a list of all social sites  Results are paginated.
#'
#' @concept Socialsite
#'
#' @param p Page Number
#' @return id The unique id of the social site.
#' @return name The name of the social site.
#' @return website The URL to the website for the social site.
#' @export
getEverySocialsite <- function(p){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/socialsites"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get A Social Site
#'
#' Gets a specific social site by ID.
#'
#' @concept Socialsite
#'
#' @param socialsiteId The socialsiteId
#' @return none 
#' @export
getSocialsite <- function(socialsiteId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/socialsite/:socialsiteId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

