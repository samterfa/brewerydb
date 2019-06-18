
#' Search by UPC
#'
#' Search for beers by UPC. Will return one or more beers.
#'
#' @concept SearchUpc
#'
#' @param p Page Number
#' @param code Required UPC
#' @return none 
#' @export
getEverySearchUpc <- function(p, code){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/search/upc"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

