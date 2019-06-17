
#' Get All Beers
#'
#' Gets a listing of all beers.  Results will be paginated.
#'
#' @concept Beer
#'
#' @param p Page Number
#' @param ids ID's of the beers to return, comma separated. Max 10.
#' @param name Name of a beer.
#' @param abv ABV for a beer. Premium users may use advanced filtering. "+10" will return everything above 10%, "-10" will return everything under 10%, "8,10" will return everything between 8% and 10% inclusive.
#' @param ibu IBUs for a beer. Premium users may use advanced filtering. "+50" will return everything above 50 IBUs, "-50" will return everything less than 50 IBUs, "30,50" will return everything between 30 and 50 IBUs inclusive.
#' @param glasswareId ID for glassware
#' @param srmId ID for SRM
#' @param availableId ID for availability
#' @param styleId ID for style
#' @param isOrganic Whether the beer is certified organic or not Y N
#' @param hasLabels Whether or not the beer has a label Y N
#' @param year Year vintage of the beer. Format YYYY
#' @param since Returns everything that has been updated since that date. Max 30 days. In UNIX timestamp format.
#' @param status Status of the brewery in the API
#' @param order How the results should be ordered name Default description abv ibu glasswareId srmId availableId styleId isOrganic status createDate updateDate random
#' @param sort How the results should be sorted. ASC Default DESC
#' @param randomCount If the order parameter is set to random, this option specifies how many random beers to return. It has a max value of 10
#' @param withBreweries Get beer results with brewery information included. Y N Default
#' @param withSocialAccounts Get beer results with social account information included. Y N Default
#' @param withIngredients Get beer results with ingredients information included. Y N Default
#' @return id The unique id of the beer.
#' @return name The name of the beer.
#' @return nameDisplay Display name of the beer.
#' @return description The description of the beer.
#' @return foodPairings A free text field containing any food pairing information for the beer.
#' @return originalGravity The original gravity of the beer.
#' @return abv The alcohol by volume of the beer (expressed as a percentage).
#' @return ibu The IBU (international bittering unit) value is a measure of how bitter a beer is. The higher the number, the more bitter the beer.
#' @return isRetired Displays if the beer is no longer produced. ('Y' or 'N')
#' @return glasswareId The id corresponding to the glass object that is assigned to this beer. If this exists, then so will the glass object. See the glass object for more information.
#' @return glass Contains the details about the assigned glass (id, name). The name of the glass is the type of glass in which the beer is best served.
#' @return styleId The id corresponding to the style object that is assigned to the beer. See the style object for more information.
#' @return style The style object contains details about the assigned style: id, categoryId, category (id, name), name, description, ibuMin, ibuMax, abvMax, srmMin, srmMax, ogMin, fgMin, fgMax).
#' @return isOrganic Whether or not the beer is certified organic.
#' @return labels If this object is set then labels exist and it will contain items icon, medium, and large that are URLs to the images.
#' @return servingTemperature The key that corresponds to the serving temperature information. See the servingTemperatureDisplay field for the full details.
#' @return servingTemperatureDisplay The serving temperature information. If the servingTemperature was "cool" then this field would be like "Cool - (8-12C/45-54F)"
#' @return status The status key for the object
#' @return statusDisplay The display string corresponding to the status.
#' @return availableId The id corresponding to the assigned availability object.
#' @return available The object that provides details for the assigned availability. It contain: id, name, and description.
#' @return beerVariationId If this beer is a variation of another beer, then the beerVariationId will be set to the id of the "source" beer. For example, if "My IPA" has id abc123 and you look up "Oak Aged My IPA" (id def456) then the beerVariationId of "Oak Aged My IPA" would be abc123.
#' @return beerVariation If this object will be set if there is a beerVariationId set. This is an instance of a beer object that has all the information for the "source" beer if the beer is a variation.
#' @return year The year field is for vintages of a beer.
#' @return nutritionServingSize Displays the serving size that the nutritional information based on.
#' @return nutritionEstimated Displays if the beer has estimated nutritional values. ('Y' or 'N')
#' @return calories Total calories in beer based on Serving Size
#' @return fat Total Fat in beer based on Serving Size
#' @return saturatedFat Total Saturated Fat in beer based on Serving Size
#' @return transFat Total Trans Fat in beer based on Serving Size
#' @return cholesterol Total Cholesterol in beer based on Serving Size
#' @return sodium Total Sodium in beer based on Serving Size
#' @return carbohydrates Total Carbohydrates in beer based on Serving Size
#' @return dietaryFiber Total Dietary Fiber in beer based on Serving Size
#' @return sugars Total Sugars in beer based on Serving Size
#' @return protein Total Protein in beer based on Serving Size
#' @export
getEveryBeer <- function(p, ids, name, abv, ibu, glasswareId, srmId, availableId, styleId, isOrganic, hasLabels, year, since, status, order, sort, randomCount, withBreweries, withSocialAccounts, withIngredients){

	params <- as.list(environment())
	params <- params[params != ""]

	returnData <- makeRequest("/beers", "GET", params)

	flattenJsonList(returnData$data)

}

#' Get a single beer
#'
#' Gets a single beer by ID
#'
#' @concept Beer
#'
#' @param withBreweries Get beer results with brewery information included. Y N Default
#' @param withSocialAccounts Get beer results with social account information included. Y N Default
#' @param withIngredients Get beer results with ingredients information included. Y N Default
#' @return none 
#' @export
getBeer <- function(withBreweries, withSocialAccounts, withIngredients){

	params <- as.list(environment())
	params <- params[params != ""]

	returnData <- makeRequest("/beer/:beerId", "GET", params)

	flattenJsonList(returnData$data)

}

#' Add a new beer
#'
#' Add a beer.  Beer will be added with status of new_unverified.
#'
#' @concept Beer
#'
#' @param name Required Name of the beer
#' @param styleId Required ID corresponding to the appropriate style
#' @param description A little bit about the beer
#' @param abv ABV percentage of the beer
#' @param ibu Measured IBUs of the beer
#' @param glasswareId ID corresponding to the appropriate glassware
#' @param srmId ID corresponding to the appropriate SRM
#' @param availableId ID corresponding to the appropriate beer availability setting
#' @param isRetired Whether or not the beer is produced or not Y N Default
#' @param isOrganic Whether or not the beer is certified organic or not Y N Default
#' @param beerVariationId ID of an existing beer that this beer is a variation of
#' @param year Vintage of the beer, if applicable
#' @param foodPairings Information about what foods should be paired with the beer
#' @param servingTemperature Recommended serving temperature of the beer
#' @param originalGravity Measured original gravity of the beer
#' @param brewery Comma separated list of existing brewery IDs to add this beer to
#' @param label Base64 encoded image to be assigned as the beer label
#' @param nutritionServingSize Displays the serving size that the nutritional information based on.
#' @param calories Total calories in beer based on serving size. Serving size is Required if this is set.
#' @param fat Total Fat in beer based on serving size. Serving size is Required if this is set.
#' @param saturatedFat Total Saturated Fat in beer based on serving size. Serving size is Required if this is set.
#' @param transFat Total Trans Fat in beer based on serving size. Serving size is Required if this is set.
#' @param cholesterol Total Cholesterol in beer based on serving size. Serving size is Required if this is set.
#' @param sodium Total Sodium in beer based on serving size. Serving size is Required if this is set.
#' @param carbohydrates Total Carbohydrates in beer based on serving size. Serving size is Required if this is set.
#' @param dietaryFiber Total Dietary Fiber in beer based on serving size. Serving size is Required if this is set.
#' @param sugars Total Sugars in beer based on serving size. Serving size is Required if this is set.
#' @param protein Total Protein in beer based on serving size. Serving size is Required if this is set.
#' @return none 
#' @export
createBeer <- function(name , styleId , description, abv, ibu, glasswareId, srmId, availableId, isRetired, isOrganic, beerVariationId, year, foodPairings, servingTemperature, originalGravity, brewery, label, nutritionServingSize, calories, fat, saturatedFat, transFat, cholesterol, sodium, carbohydrates, dietaryFiber, sugars, protein){

	params <- as.list(environment())
	params <- params[params != ""]

	returnData <- makeRequest("/beers", "POST", params)

	flattenJsonList(returnData$data)

}

#' Update a beer
#'
#' Update a beer.  Beer will have a status of update_pending after. Note: There may be some beers that do not have a Style. If this is the case a StyleId will be required for update.
#'
#' @concept Beer
#'
#' @param name Required Name of the beer
#' @param styleId Required ID corresponding to the appropriate style
#' @param description A little bit about the beer
#' @param abv ABV percentage of the beer
#' @param ibu Measured IBUs of the beer
#' @param glasswareId ID corresponding to the appropriate glassware
#' @param srmId ID corresponding to the appropriate SRM
#' @param availableId ID corresponding to the appropriate beer availability setting
#' @param isOrganic Whether or not the beer is certified organic or not Y N Default
#' @param isRetired Whether or not the beer is produced or not Y N Default
#' @param beerVariationId ID of an existing beer that this beer is a variation of
#' @param year Vintage of the beer, if applicable
#' @param foodPairings Information about what foods should be paired with the beer
#' @param servingTemperature Recommended serving temperature of the beer
#' @param originalGravity Measured original gravity of the beer
#' @param label Base64 encoded image to be assigned as the beer label
#' @param nutritionServingSize Displays the serving size that the nutritional information based on.
#' @param calories Total calories in beer based on serving size. Serving size is Required if this is set.
#' @param fat Total Fat in beer based on serving size. Serving size is Required if this is set.
#' @param saturatedFat Total Saturated Fat in beer based on serving size. Serving size is Required if this is set.
#' @param transFat Total Trans Fat in beer based on serving size. Serving size is Required if this is set.
#' @param cholesterol Total Cholesterol in beer based on serving size. Serving size is Required if this is set.
#' @param sodium Total Sodium in beer based on serving size. Serving size is Required if this is set.
#' @param carbohydrates Total Carbohydrates in beer based on serving size. Serving size is Required if this is set.
#' @param dietaryFiber Total Dietary Fiber in beer based on serving size. Serving size is Required if this is set.
#' @param sugars Total Sugars in beer based on serving size. Serving size is Required if this is set.
#' @param protein Total Protein in beer based on serving size. Serving size is Required if this is set.
#' @return none 
#' @export
updateBeer <- function(name , styleId , description, abv, ibu, glasswareId, srmId, availableId, isOrganic, isRetired, beerVariationId, year, foodPairings, servingTemperature, originalGravity, label, nutritionServingSize, calories, fat, saturatedFat, transFat, cholesterol, sodium, carbohydrates, dietaryFiber, sugars, protein){

	params <- as.list(environment())
	params <- params[params != ""]

	returnData <- makeRequest("/beer/:beerId", "PUT", params)

	flattenJsonList(returnData$data)

}

#' Delete a beer
#'
#' Delete a beer  Beer will have a status of delete_pending after.
#'
#' @concept Beer
#'
#' @param none 
#' @return none 
#' @export
deleteBeer <- function(){

	params <- as.list(environment())
	params <- params[params != ""]

	returnData <- makeRequest("/beer/:beerId", "DELETE", params)

	flattenJsonList(returnData$data)

}

