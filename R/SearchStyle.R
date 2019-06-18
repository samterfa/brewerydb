
#' Style Search
#'
#' Search
#'
#' @concept SearchStyle
#'
#' @param p Page Number
#' @param q Required Query String
#' @param withDescriptions Get results with style descriptions included. Y N Default
#' @return none 
#' @export
getEverySearchStyle <- function(p, q, withDescriptions){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/search/style"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

