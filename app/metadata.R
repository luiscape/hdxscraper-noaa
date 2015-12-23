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

library(counrycode)

#
# Add default missing metadata to
# each dataset.
#
addBaseMetadata <- function(df=NULL, is_private=TRUE) {
  cat('Adding metadata ...')
  
  #
  # License
  #
  df$license_id = "other-pd-nr"
  df$license_title = "other-pd-nr"
  df$license_other = ""
  
  #
  # Author and maintainer.
  #
  df$author = "ncdc"
  df$author_email = "ncdc.orders@noaa.gov"
  df$maintainer = "luiscape"
  df$maintainer_email = "capelo@un.org"
  df$dataset_source = "National Oceanic and Atmospheric Administration (NOAA)"
  df$owner_org = "hdx"
  
  #
  # Organization id that created dataset.
  #
  df$package_creator = "luiscape"
  
  #
  # Private attribute.
  #
  df$private = is_private
  
  #
  # Methodology and caveats.
  #
  df$methodology = "Other"
  df$methodology_other = 'For a station to be considered for any parameter, it must have a minimum of 30 years of data with more than 182 days complete each year. This is effectively a "30-year record of service" requirement, but allows for inclusion of some stations which routinely shut down during certain seasons. Small station moves, such as a move from one property to an adjacent property, may occur within a station history. However, larger moves, such as a station moving from downtown to the city airport, generally result in the commissioning of a new station identifier. This tool treats each of these histories as a different station. In this way, it does not "thread" the separate histories into one record for a city. For more information, please refer to NOAA NCDC\'s official [methodology page](https://www.ncdc.noaa.gov/cdo-web/datatools/records).'
  df$caveats = "Due to late-arriving data, the number of recent records is likely underrepresented in all categories, but the ratio of records (warm to cold, for example) should be a fairly strong estimate of a final outcome."
  
  #
  # Tags.
  #
  df$list_of_tags = c('climate change', 'precipitation', 'weather', 'climate', 'el nino')
  
  cat('done.\n')
  return(df)
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
           dataset_date = format(as.Date(dataset_date[i]), "%m/%d/%Y"),  # HDX doesn't use ISO here.
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
           tags = list(list_of_tags),
           groups = list(
             list(
               id = group_id[i]
             )
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
         resource_1 <<-
           list(
             package_id = dataset_name[i],
             url = url_1[i],
             name = file_name_1[i],
             format = 'CSV'
           )
    )
    with(df,
         resource_2 <<-
           list(
             package_id = dataset_name[i],
             url = url_2[i],
             name = file_name_2[i],
             format = 'CSV'
           )
    )
    
    it <- c(
      list(resource_1),
      list(resource_2)
    )
    
    #
    # Filter resources without URL
    #
    if (i == 1) out <- it
    else out <- c(out, it)
  }
  
  cat('done.\n')
  return(out)
}