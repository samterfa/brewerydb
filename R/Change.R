
#' Get All Changes
#'
#' Gets a listing of all changes to all attributes that have happened in the last 30 days.  Results will be paginated with 50 results per page.
#'
#' @concept Change
#'
#' @param p Page Number
#' @param attributeName Name of an attribute in BreweryDB. Multiple attributeNames can be separated by a comma. beer brewery location guild event
#' @param attributeId ID of an attribute in BreweryDB. Will only be recognized if a single attributeName parameter is also passed.
#' @param since UNIX timestamp of a date in which to get changes since. This can be a max of 30 days in the past.
#' @return attributeName Attribute name of the affected attribute. Either "beer", "brewery", "location", "guild" or "event".
#' @return action Action taken on that attribute. Either "edit", "insert", or "delete".
#' @return attribute Data structure related to the type of attribute that changes. If a beer was changed, you would get the basic beer structure. If a brewery was changed, you would get the basic brewery structure.
#' @return subAttributeName Name of the sub attribute that was changed. Only set in the case of relationships.
#' @return subAction Action taken on the sub attribute. Either "edit", "insert", or "delete".
#' @return subAttribute Data structure related to the type of attribute that changed.
#' @export
getEveryChange <- function(p, attributeName, attributeId, since){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/changes"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

