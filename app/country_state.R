#
#  COUNTRY STATE -----------------------------------
#  
#  Script containing logic for getting and setting
#  the state of countries. This functions in way
#  of making sure that, at least once a week, 
#  all data for a particular country is effectively
#  fetched. 
# 
#  -------------------------------------------------
#
library(sqldf)

countryGet <- function(country=NULL) {
  
  #
  #  Checking input.
  #
  if (is.null(country)) {
    stop('No country ISO3 code provided.')
  }
  
  #
  #  Connecting to database.
  #
  db <- dbConnect(SQLite(), dbname = 'scraperwiki.sqlite')
  
  #
  #  Make query and send result back.
  #
  result <- dbGetQuery(db, paste0('SELECT * FROM countries WHERE names="', country, '"'))
  if (result$queried_this_week == 0) return(FALSE)
  if (result$queried_this_week == 1) return(TRUE)
}

countrySet <- function(country=NULL) {
  
  #
  #  Checking input.
  #
  if (is.null(country)) {
    stop('No country ISO3 code provided.')
  }
  
  #
  #  Connecting to database.
  #
  db <- dbConnect(SQLite(), dbname = 'scraperwiki.sqlite')
  
  #
  #  Make query and send result back.
  #
  result <- dbGetQuery(db, paste0('UPDATE countries SET queried_this_week=1 WHERE names="', country,'"'))
}