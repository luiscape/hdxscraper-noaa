#
#  METADATA : ------------------------------------------
#
#  Script with a number of helper functions that creates
#  HDX-style metadata to each dataset to-be registed on
#  HDX. Most of the functions on this script are called
#  after the collector job is complete. They generate
#  the JSON input necessary for the `hdx_register`
#  program to function.
#
#  ------------------------------------------------------
#

library(countrycode)

#
# Add default missing metadata to
# each dataset.
#
addBaseMetadata <- function(country_list=NULL, is_private=FALSE) {
  cat('Adding metadata ...')
  

  #
  # Dataset name and title.
  #
  for (i in 1:nrow(country_list)) {
    country_name = countrycode(toupper(as.character(country_list$name[i])), 'iso3c','country.name')
    
    country_list$title[i] = paste('Daily Summaries of Precipitation Indicators for', country_name)
    country_list$notes[i] = paste0("This dataset contains the daily summaries on base stations across **", country_name, "**. The four indicators included are:   <br />\n <br />\n* **TPCP**: Total precipitation <br />\n* **MXSD**: Maximum snow depth <br />\n* **TSNW**: Total snow fall <br />\n* **EMXP**: Extreme maximum daily precipitation <br />\n <br />\nIndicators are compiled by the  National Centers for Environmental Information (NCEI), which is administrated by National Oceanic and Atmospheric Administration (NOAA) an organization part of the United States government. NOAA has access to data collected from thousands of base stations around the world, which collect data periodically on weather and climate conditions. <br />\n <br />\nThis dataset contains the latest **5 years of available data**.")
    
    #
    # Cleaning country name.
    #
    country_name <- tolower(country_name)
    country_name <- gsub("'", "", country_name)
    country_name <- gsub(" ", "-", country_name)
    country_name <- gsub(",", "-", country_name)
    country_name <- gsub("\\(", "-", country_name)
    country_name <- gsub("\\)", "-", country_name)
    
    country_list$dataset_name[i] = paste0('daily-summaries-of-precipitation-indicators-for-', country_name)
    
    #
    # Adding resource metadata.
    #
    country_list$url[i] <- paste0('https://ds-ec2.scraperwiki.com/fhsehwp/vdocua8hjwptucu/cgi-bin/csv/', country_list$name[i], '.csv')
    country_list$file_name[i] <- paste0('precipitation_', country_list$name[i], '.csv')
    
  }
  
  #
  # Adding country.
  #
  country_list$group = country_list$name
  
  #
  # Dataset date.
  #
  country_list$dataset_date = country_list$latest_date

  #
  # License
  #
  country_list$license_id = "other-pd-nr"
  country_list$license_title = "other-pd-nr"
  country_list$license_other = ""
  
  #
  # Author and maintainer.
  #
  country_list$author = "noaa"
  country_list$author_email = "ncdc.orders@noaa.gov"
  country_list$maintainer = "luiscape"
  country_list$maintainer_email = "capelo@un.org"
  country_list$dataset_source = "National Centers for Environmental Information (NCEI / NOAA)"
  country_list$owner_org = "hdx"
  
  #
  # Organization id that created dataset.
  #
  country_list$package_creator = "luiscape"
  
  #
  # Private attribute.
  #
  country_list$private = is_private
  
  #
  # Methodology and caveats.
  #
  country_list$methodology = "Other"
  country_list$methodology_other = 'For a station to be considered for any parameter, it must have a minimum of 30 years of data with more than 182 days complete each year. This is effectively a "30-year record of service" requirement, but allows for inclusion of some stations which routinely shut down during certain seasons. Small station moves, such as a move from one property to an adjacent property, may occur within a station history. However, larger moves, such as a station moving from downtown to the city airport, generally result in the commissioning of a new station identifier. This tool treats each of these histories as a different station. In this way, it does not "thread" the separate histories into one record for a city. For more information, please refer to NOAA NCDC\'s official [methodology page](https://www.ncdc.noaa.gov/cdo-web/datatools/records).'
  country_list$caveats = "Due to late-arriving data, the number of recent records is likely underrepresented in all categories, but the ratio of records (warm to cold, for example) should be a fairly strong estimate of a final outcome."
  
  cat('done.\n')
  return(country_list)
}


#### JSON SERIALIZATION ####

#
# Function to transform a UNOSAT data.frame
# into a CKAN / HDX dataset JSON object.
#
createDatasetsJson <- function(df = NULL) {
  cat('Creating CKAN datasets JSON object ...')
  
  #
  # Making all variables character -- and !factors.
  #
  df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
  
  for (i in 1:nrow(df)) {
    with(df,
         it <<- list(
           name = dataset_name[i],
           title = title[i],
           author = author[i],
           author_email = author_email[i],
           maintainer = maintainer[i],
           maintainer_email = maintainer_email[i],
           license_id = license_id[i],
           license_other = license_other[i],
           # dataset_date = format(as.Date(dataset_date[i]), "%m/%d/%Y"),  # HDX doesn't use ISO here.
           dataset_date = '12/23/2015',
           subnational = "1",
           notes = notes[i],
           caveats = caveats[i],
           methodology = "Other",
           methodology_other = methodology_other[i],
           dataset_source = dataset_source[i],
           package_creator = package_creator[i],
           private = TRUE,  # Public to the world?
           url = NULL,
           state = "active",  # Better don't touch this.
           tags = list(
             list(name = 'climate change'),
             list(name = 'precipitation'),
             list(name = 'weather'),
             list(name = 'climate'), 
             list(name = 'el nino')
             ),
           groups = list(
             list(id = group[i])
             ),
           owner_org = owner_org[i]
         )
    )
    if (i == 1) out <- it
    else out <- rbind(out, it)
  }
  
  cat('done.\n')
  return(out)
}



#
# Serializing resources.
#
createResourcesJson <- function(df = NULL) {
  cat('Creating CKAN resources JSON object ...')
  
  # Making all variables character -- and !factors.
  df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
  
  for (i in 1:nrow(df)) {
    with(df,
         resource <<-
           list(
             package_id = dataset_name[i],
             url = url[i],
             name = file_name[i],
             format = 'CSV'
           )
    )
    
    it <- list(resource)
    
    #
    # Filter resources without URL
    #
    if (i == 1) out <- it
    else out <- c(out, it)
  }
  
  cat('done.\n')
  return(out)
}