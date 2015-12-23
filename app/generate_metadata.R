#
#  GENERATE METADATA: ------------------------
#
#  Generates metadata for the NOAA files. 
#  This script will query the SQLite database
#  to figure out what tables are available
#  for what countries; and what is the lastest
#  data available. All of that will be used
#  to generate the JSON metadata files necessary
#  for registering datasets on HDX.
#
#  -------------------------------------------
#

onSw <- function(p = NULL, d = 'tool/', a = F) {
  if (a == T) return(paste0(d,p))
  else return(p)
}

source(onSw('app/hdx_country_list.R'))

#
#  CONSTANTS: ---------------------------------------
#
#  Currently, contants are:
# 
#    - DB_NAME: the file name of the SQLite database.
#
#  --------------------------------------------------
#
DB_NAME <- 'scraperwiki.sqlite'

generateMetadata <- function() {
  cat('Generating metadata files ... ')
  
  #
  #  Connects to SQLite database
  #  and extract the current list 
  #  of tables.
  #
  db <- dbConnect(SQLite(), dbname = DB_NAME)
  table_list <- data.frame(
    name = db_list_tables(db),
    exists = NA
    )
  
  #
  #  Collect list of countries and compare.
  #  The result of comparison should generate
  #  a combination of metadata files.
  #
  country_list <- fetchHDXCountryList()
  table_list$exists <- tolower(table_list$name) %in% country_list$name
  hdx_ready_list <- filter(table_list, exists == TRUE)
  
  #
  # Creating and writting JSON metadata
  # on disk.
  #
  datasets_json <- createDatasetsJson(hdx_ready_list)
  resources_json <- createResourcesJson(hdx_ready_list)
  
  jsons <- list(datasets_json, resources_json)
  for (i in 1:length(jsons)) {
    p = c("metadata/datasets.json", "metadata/resources.json")
    sink(onSw(p[i]))
      cat(toJSON(jsons[i]))
    sink()
  }
  
  cat('done!\n')
}