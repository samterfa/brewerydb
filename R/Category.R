
#' Get All Categories
#'
#' Gets a listing of All categories
#'
#' @concept Category
#'
#' @param none 
#' @return id The unique id of the category.
#' @return name The name of the category.
#' @export
getEveryCategory <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/categories"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get a single category
#'
#' Gets a single Category
#'
#' @concept Category
#'
#' @param categoryId The categoryId
#' @return none 
#' @export
getCategory <- function(categoryId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/category/:categoryId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

