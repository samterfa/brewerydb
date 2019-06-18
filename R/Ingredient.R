
#' Get List of Ingredients
#'
#' Gets a list of all ingredients.  Results are paginated.
#'
#' @concept Ingredient
#'
#' @param p Page Number
#' @return none 
#' @export
getEveryIngredient <- function(p){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/ingredients"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get An Ingredient
#'
#' Gets a specific ingredient by ID.
#'
#' @concept Ingredient
#'
#' @param ingredientId The ingredientId
#' @return none 
#' @export
getIngredient <- function(ingredientId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/ingredient/:ingredientId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

