
#' Get List of Hops
#'
#' Gets a list of all hops.  Results are paginated.
#'
#' @concept Hop
#'
#' @param p Page Number
#' @return id The unique id of the hop.
#' @return name The name of the hop.
#' @return description The description of the hop.
#' @return countryOfOrigin The two-letter ISO country code. For the complete country name and information, see the country field.
#' @return alphaAcidMin The minimum value for the typical range of alpha acids in the hop.
#' @return alphaAcidMax The maximum value for the typical range of alpha acids in the hop.
#' @return betaAcidMin The minimum value for the typical range of beta acids in the hop.
#' @return betaAcidMax The maxiumum value for the typical range of beta acids in the hop.
#' @return humuleneMin The minimum value for the typical range of humulene in the hop. Expressed as a percentage of the total oils.
#' @return humuleneMax The maximum value for the typical range of humulene in the hop. Expressed as a percentage of the total oils.
#' @return caryophylleneMin The minimum value for the typical range of caryophyllene in the hop. Expressed as a percentage of the total oils.
#' @return caryophylleneMax The maximum value for the typical range of caryophyllene in the hop. Expressed as a percentage of the total oils.
#' @return cohumuloneMin The minimum value for the typical range of cohumulone in the hop. Expressed as a percentage of the alpha acids.
#' @return cohumuloneMax The maximum value for the typical range of cohumulone in the hop. Expressed as a percentage of the alpha acids.
#' @return myrceneMin The minimum value for the typical range of myrcene in the hop. Expressed as a percentage of the total oils.
#' @return myrceneMax The maximum value for the typical range of myrcene in the hop. Expressed as a percentage of the total oils.
#' @return farneseneMin The minimum value for the typical range of farnesene in the hop. Expressed as a percentage of the total oils.
#' @return farneseneMax The maximum value for the typical range of farnesene in the hop. Expressed as a percentage of the total oils.
#' @return isNoble This is set to Y if the hop is a noble hop.
#' @return forBittering Set to Y if the hop is typically used for bittering.
#' @return forFlavor Set to Y if the hop is typically used for its flavor.
#' @return forAroma Set to Y if the hop is typically used for its aroma.
#' @return category This value will always be set to "hop".
#' @return categoryDisplay This value will always be set to "Hops".
#' @return country This is an object that provides more information about the country matching the hop's countryOfOrigin value.
#' @export
getEveryHop <- function(p){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/hops"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get A Hop
#'
#' Gets a specific hop by ID.
#'
#' @concept Hop
#'
#' @param hopId The hopId
#' @return none 
#' @export
getHop <- function(hopId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/hop/:hopId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

