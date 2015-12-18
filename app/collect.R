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
indicator_list = c('TPCP', 'MXSD', 'TSNW', 'EMXP')

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
fetchCountryData <- function(
  country=NULL,
  country_name=NULL,
  indicators=indicator_list,
  start='2010-01-01',
  end=NULL
  ) {

  #
  #  Defining merge data.frame.
  #
  out <- data.frame(
    date = NA,
    datatype = NA,
    station = NA,
    value = NA,
    fl_miss = NA,
    fl_cmiss = NA,
    country = NA,
    indicator = NA
  )
  for (i in 1:length(indicators)) {
    o = 0
    total = 1000
    it <- out
    while(nrow(it) <= total) {
      cat('.')
      Sys.sleep(1)
      a <- ncdc(
        datasetid='GHCNDMS',
        locationid=country,
        datatypeid=indicators[i],
        startdate = start,
        enddate = end,
        limit = 1000,
        offset = o
      )

      #
      #  Organizing iterator.
      #
      if (is.null(a$meta$totalCount)) {
        break
      } else {
        total = a$meta$totalCount
        o = o + 1
      }

      if (is.null(a$data) == FALSE) {
        #
        #  Building data.frame.
        #
        a$data$indicator <- indicators[i]
        a$data$country <- country_name

        it <- rbind(it, a$data)
      }

    }
    if (nrow(it) > 0) {
      out <- rbind(out, it)
    }
  }

  #
  #  Filters the NA introduced
  #  by the data.frame definition.
  #
  return(filter(out, is.na(datatype) == FALSE))
}

fetchAllCountries <- function(country_list=x) {

  #
  #  Defining merge data.frame.
  #
  out <- data.frame(
    date = NA,
    datatype = NA,
    station = NA,
    value = NA,
    fl_miss = NA,
    fl_cmiss = NA,
    country = NA,
    indicator = NA
  )

  #
  #  Creates an iterator for each
  #  country.
  #
  for (i in 1:nrow(country_list)) {
    cat(paste(country_list$id[i]), ': ')

    try(
      #
      #  Collecting all data from
      #  a single country.
      #
      it <- fetchCountryData(
        country=country_list$id[i],
        country_name=country_list$name[i],
        end=country_list$maxdate[i]
      )
    )
    if(class(it) == "try-error") { next }

    #
    #  Save temporary results in disk.
    #
    cat(paste(nrow(it)), 'records.\n')
    write.csv(out, paste0('data/', country_list$name[i], '.csv'), row.names=FALSE)
    if (!is.null(it) && nrow(it) > 0) {
      out <- rbind(out, it)
    } else {
      print('No data.')
      next
    }
  }
  return(out)
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
  countries <- fetchCountries()
  data <- fetchAllCountries(c)

  write.csv(countries, 'countries.csv', row.names = FALSE)
  write.csv(data, 'data', row.names = FALSE)

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






x <- fetchCountryData('FIPS:BF', indicators, end='2015-11-01')
