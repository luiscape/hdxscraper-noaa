#
#  NCDC COLLECTOR: --------------------------------------
#  
#  Collector for NCDC / NOAA precipitation data.
#  This collector will query NCDC's API for
#  a list of indicators, for about 10 years,
#  on all available countries. Two datasets
#  will then be generated:
#
#    - country_summaries.csv: containing coverage
#      information for all available countries
#
#    - precipitation_data.csv: containing precipitation
#      data from all available countries.
#
#  ------------------------------------------------------
#
library(rnoaa)

#
#  HELPER: ----------------------------------------------
#
#  Here we handle the relative folders of the ScraperWiki
#  environments -- generally inside the `tool` folder.
#  Also, two helper scripts are loaded:
#
#  - sw_status.R: loads a function to control the
#    collector box.
#
#  - write_table.R: loads a function to store a table
#    in the database.
#
#  ------------------------------------------------------
#
onSw <- function(p = NULL, d = 'tool/', a = F) {
  if (a == T) return(paste0(d,p))
  else return(p)
}

source(onSw('app/helpers/sw_status.R'))
source(onSw('app/helpers/write_table.R'))

#
#  API TOKEN: -------------------------------------------
#
#  Here goes an API token. Especifically, it uses the
#  token from: Luis Capelo <capelo@un.org>
#
#  ------------------------------------------------------
#
token = ''
options(noaakey = token)

fetchCountries <- function() {
  countries = ncdc_locs(locationcategoryid='CNTRY', limit=300)
  return(countries$data)
}

countries <- fetchCountries()

#
#  INDICATORS: ----------------------------------------
# 
#  Here we are interested in collecting all indicators
#  of interest. Those come from the "precipitation"
#  classification. They are:
#  
#    - TPCP: Total precipitation
#    - MXSD: Maximum snow depth
#    - TSNW: Total snow fall
#    - EMXP: Extreme maximum daily precipitation
#
#  ----------------------------------------------------
#
indicators = c('TPCP', 'MXSD', 'TSNW', 'EMXP')

#
#  LOGIC: ---------------------------------------------
#
#  Two functions are outlined here:
#
#   - fetchCountryData: collects all indicator data
#     for a particular country.
#
#   - fetchAllCountries: calls the previous function
#     for each country.
#
#  ----------------------------------------------------
#
fetchCountryData <- function(country=NULL, indicators=NULL, start='2006-01-01', end=NULL) {
  for (i in 1:length(indicators)) {
    it <- ncdc(
      datasetid='GHCNDMS', 
      locationid=country,
      datatypeid=indicators[i], 
      startdate = start,
      enddate = end
    )
    if (i == 1) {
      out <- it$data
    } else {
      out <- rbind(out, it$data)
    }
  }
  return(it)
}

fetchAllCountries <- function(country_list=NULL) {
  for (i in 1:nrow(country_list)) {
    cat(paste(country_list$id[i]), ':')
    it <- fetchCountryData(country=country_list$id[i], end=country_list$maxdate[i])
    country = substr(country_list$id[i],nchar(country_list$id[i])-1, nchar(country_list$id[i]))
    if (!is.null(it)) {
      if (i == 1) {
        it$country <- country
        out <- it$data
      } else {
        it$country <- country
        out <- rbind(out, it$data)
      } 
    } else {
      print('No data.')
      next
    }
    cat(paste(nrow(out)), '\n')
  }
  return (out)
}

#
#  STORE DATA: ---------------------------------------------
#
#  Here we store data in a SQLite database.
#
#  ---------------------------------------------------------
#
runScraper <- function() {
  #
  # Collect data from NOAA.
  #
  data <- fetchAllCountries(countries)
  
  #
  #  Writting tables in SQLite.
  #
  writeTable(countries, 'country_summaries')
  writeTable(data, 'precipitation_data')
}


#
#  SCRAPERWIKI: --------------------------------------------
#
#  ScraperWiki wrapper. This helps to set the corresponding
#  box's status.
#
#  ---------------------------------------------------------
#
tryCatch(runScraper(),
         error = function(e) {
           cat('Error detected ... sending notification.')
           system('mail -s "NOAA scraper failed." capelo@un.org')
           changeSwStatus(type = "error", message = "Scraper failed.")
           { stop("!!") }
         }
)

changeSwStatus(type = 'ok')