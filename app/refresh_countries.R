#
#  REFRESH COUNTRIES --------------
#
#  This script will refresh the country
#  table in the database. That table is
#  important because it records the state
#  of when each country data was collected.
# 
#  ---------------------------------------
#
library(sqldf)
library(rnoaa)
library(countrycode)

#
#  API TOKEN: -------------------------------------------
#
#  Here goes an API token. Especifically, it uses the
#  token from: Luis Capelo <capelo@un.org>
#
#  ------------------------------------------------------
#
TOKEN = 'exWejMnqCKSFJGqbutdfbchkoiqtuoOq'
options(noaakey = TOKEN)

fetchCountries <- function() {
  countries = ncdc_locs(locationcategoryid='CNTRY', limit=300)
  return(countries$data)
}

refreshCountries <- function(table_name = 'countries') {
  #
  #  Generate fresh table.
  #
  countries <- data.frame(
    names = fetchCountries()$name,
    queried_this_week = FALSE,
    latest_query = NA
  )
  countries$names <- tolower(countrycode(countries$names, 'country.name', 'iso3c'))
  
  
  #
  #  Connect to database and store data.
  #
  db <- dbConnect(SQLite(), dbname = 'scraperwiki.sqlite')
  dbWriteTable(db, table_name, countries, row.names = FALSE, overwrite = TRUE)
  
  cat('Weekly country-list refreshed successfully!\n')
}

refreshCountries()
