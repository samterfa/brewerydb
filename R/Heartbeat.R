
#' Get Heartbeat
#'
#' Ensures the API is up and listening.  Will take any list of arguments, and return them back along with a timestamp.
#'
#' @concept Heartbeat
#'
#' @param none 
#' @return none 
#' @export
EveryHeartbeat <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/heartbeat"

	returnData <- makeRequest(endpoint, "GET,POST,PUT,DELETE", params)

	flattenJsonList(returnData$data)

}

