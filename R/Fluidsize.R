
#' Get List of Fluid Sizes
#'
#' Gets a list of all fluid sizes.  Results are paginated.
#'
#' @concept Fluidsize
#'
#' @param p Page Number
#' @return id The unique id of the fluid size.
#' @return volume The unit that corresponds to the quantity of the fluid size.
#' @return volumeDisplay A more human readable version of the volume value.
#' @return quantity The value of the fluid size.
#' @export
getEveryFluidsize <- function(p){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/fluidsizes"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get A Fluid Size
#'
#' Gets a specific fluid size by ID.
#'
#' @concept Fluidsize
#'
#' @param fluidsizeId The fluidsizeId
#' @return none 
#' @export
getFluidsize <- function(fluidsizeId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/fluidsize/:fluidsizeId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

