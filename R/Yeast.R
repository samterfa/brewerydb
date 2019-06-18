
#' Get List of Yeasts
#'
#' Gets a list of all yeasts.  Results are paginated.
#'
#' @concept Yeast
#'
#' @param p Page Number
#' @return id The unique id of the yeast.
#' @return name The name of the yeast.
#' @return description The description of the yeast.
#' @return yeastType The type of yeast that this yeast is. Will be one of: ale, wheat, lager, wine, champagne.
#' @return attenuationMin 
#' @return attenuationMax 
#' @return fermentTempMin The minimum recommended fermentation temperature, in Fahrenheit.
#' @return fermentTempMax The maximum recommended fermentation temperature, in Fahrenheit.
#' @return alcoholToleranceMin If there is a range of the upper limit alcohol tolerance of the yeast, this will be the minimum value. Expressed as percentage alcohol by volume.
#' @return alcoholToleranceMax The maximum alcohol tolerance of the yeast. Expressed as percentage alcohol by volume.
#' @return productId The supplier's product ID of the yeast.
#' @return supplier The supplier or company the provides the yeast.
#' @return yeastFormat The format(s) that yeast is available in. Will be one of: liquid, dry, or both.
#' @return category This value will always be set to "yeast".
#' @return categoryDisplay This value will always be set to "Yeast".
#' @export
getEveryYeast <- function(p){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/yeasts"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get A Yeast
#'
#' Gets a specific yeast by ID.
#'
#' @concept Yeast
#'
#' @param yeastId The yeastId
#' @return none 
#' @export
getYeast <- function(yeastId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/yeast/:yeastId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

