#
#  HDX COUNTRIES: ------------------------------------
#
#  Collects the list of countries available
#  in HDX for merging and sorting the country data
#  form NOAA.
#
#  ---------------------------------------------------
#
library(dplyr)
library(jsonlite)

#
#  Fetches the latest list of ISO3 countries
#  from the HDX repository. This list will then
#  be used to parse the NOAA data.
#
fetchHDXCountryList <- function() {
  URL='https://data.hdx.rwlabs.org/api/3/action/group_list'
  hdx_country_list <- data.frame(
    names = fromJSON(txt=URL)$result
  )
  
  out <- filter(hdx_country_list, nchar(as.character(names)) == 3)
  return(out)
}