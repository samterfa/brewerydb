
#' Get List of Fermentables
#'
#' Gets a list of all fermentables.  Results are paginated.
#'
#' @concept Fermentable
#'
#' @param p Page Number
#' @return id The unique id of the fermentable.
#' @return name The name of the fermentable.
#' @return description The description of the fermentable.
#' @return countryOfOrigin The two-letter ISO country code. For the complete country name and information, see the country field.
#' @return srmId The ID of the corresponding SRM object. See the srm field for more information like hex color.
#' @return srmPrecise The precise SRM value of the fermentable. Whereas the srmId is intended for things like providing the color, the srmPrecise value is ideal for things like calculations in recipes. Expressed as a percentage.
#' @return moistureContent The average moisture content of the grain, expressed as a percent of weight.
#' @return coarseFineDifference For grains, the difference between the coarse mill yield and a fine powder milling of the grain.
#' @return diastaticPower A measure of how much starch converting enzyme is in the grain. A higher number (common in pale malts) indicates the presence of more enzymes for starch conversion. Expressed as a percentage.
#' @return dryYield Equivalent to the potential field, but expressed as a percentage of total weight converted.
#' @return potential The potential yield of the ingredient, expressed in specific gravity units.
#' @return protein The percentage of the fermentable that is protein.
#' @return solubleNitrogenRatio The ratio of soluble nitrogen to total nitrogen. Expressed as a percentage. The soluble nitrogen ratio is an important indicator of malt modification. The higher the number, the more highly modified the malt.
#' @return maxInBatch The maximum recommended percentage of the mash makeup that this fermentable can be.
#' @return requiresMashing Whether or not the fermentable should be mashed.
#' @return category This value will always be set to "malt".
#' @return categoryDisplay This value will always be set to "Malts, Grains, & Fermentables".
#' @return srm The is an object that provides the details about the assigned srmId.
#' @return country This is an object that provides more information about the country matching the hop's countryOfOrigin value.
#' @return characteristics The list of characteristics / qualities of the fermentable.
#' @export
getEveryFermentable <- function(p){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/fermentables"

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

#' Get A Fermentable
#'
#' Gets a specific fermentable by ID.
#'
#' @concept Fermentable
#'
#' @param fermentableId The fermentableId
#' @return none 
#' @export
getFermentable <- function(fermentableId){

	params <- as.list(environment())
	params <- params[params != ""]

	endpoint <- "/fermentable/:fermentableId"

	additionalParams <- params[1:1]

	for(j in 1:length(additionalParams)){

		endpoint <- sub(paste0(":", names(additionalParams)[[j]]), additionalParams[[j]], endpoint)

	}

	params <- params[-(1:length(additionalParams))]

	returnData <- makeRequest(endpoint, "GET", params)

	flattenJsonList(returnData$data)

}

