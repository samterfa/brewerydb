
#' Get List of Styles
#'
#' Gets a list of all styles.
#'
#' @concept Style
#'
#' @param none 
#' @return id The unique id of the style.
#' @return name The name of the style.
#' @return description The description of the style.
#' @return categoryId The id of the category that the style is in. See the category return object for the details about the cateogry.
#' @return category Details about the category for the corresponding categoryId. This object contains the id and name of the category.
#' @return ibuMin The minimum in the typical IBU range for this style.
#' @return ibuMax The maximum in the typical IBU range for this style.
#' @return abvMin The minimum in the typical ABV range for this style.
#' @return abvMax The maximum in the typical ABV range for this style.
#' @return srmMin The minimum in the typical SRM range for this style.
#' @return srmMax The maximum in the typical SRM range for this style.
#' @return ogMin The minimum in the typical original gravity range for this style.
#' @return ogMax The maximum in the typical original gravity range for this style.
#' @return fgMin The minimum in the typical final gravity range for this style.
#' @return fgMax The maximum in the typical final gravity range for this style.
#' @export
getEveryStyle <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/styles"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get A Style
#'
#' Gets a specific style by ID.
#'
#' @concept Style
#'
#' @param styleId The styleId
#' @return none 
#' @export
getStyle <- function(styleId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/style/:styleId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

