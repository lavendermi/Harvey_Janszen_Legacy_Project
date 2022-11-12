#######
# this script will take occurrence data from the data entry template and place it in darwin core terms

#install.packages("groundhog")
library(groundhog)

## LOADING & INSTALLING PACKAGES ----
date <- "2022-11-02"
requiredPackages <-  c("readxl","dplyr","here", "lubridate","purrr","stringi","tidyverse","tidyr" )

for (pkg in requiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {groundhog.library(pkg, date)}
}


## READING IN DATA ----
# change to read.csv after - coversion into csv should be the last step once template completed
template <- read_excel(here::here("data","digitized_data","HJ-occ-entry-template_22-11-11.xlsx"))

## ADDING SIMPLE DARWIN CORE COLUMNS----

  ## "splitting occurrenceID into record and catalog numbers"
  # last number corresponding to record number in a given journal
  # the first number corresponding to the archive number assigned to the particular journal/ book 
  template$recordNumber <- sapply(strsplit(template$occID, "-"), "[", 3)
  template$catalogueNumber <- sapply(strsplit(template$occID, "-"), "[", 2)

  ## "datasetName"
  template$datasetName <- "Harvey Janszen Collections"

  ## "basisOfRecord"
  # Human observation = (report by a known observer that an organism was present at the place and time)
  template$basisOfRecord <- "HumanObservation"

  ## "year", "month", "day" 
  # splitting YYYYMMDD date entered into seperate year, month, day columns
  template <- template %>%
    dplyr::mutate(.data = template, fulldate = lubridate::ymd(date),
                  year = lubridate::year(fulldate), 
                  month = lubridate::month(fulldate), 
                  day = lubridate::day(fulldate))
  
  ## renaming miscellaneous columns 
  colnames(template)[c(1, 7,8,17,22,23,24,25)] <- c("occurrenceID","verbatimTaxonRank","occurrenceStatus", 
                                                             "verbatimElevation", "individualCount",
                                                             "occurrenceRemarks","recordedBy","identificationBy")
  colnames(template)[1] <-"occurrenceID"                                                     
                                                             
  ## adding journal page number to occurrence ID
  # function from: ----
    stri_sub(template$occurrenceID, 7, 6) <- template$page
    stri_sub(template$occurrenceID, 8, 7) <- "-"
    
## TAXONOMY FIELDS ----
  # "kingdom"
  # "phylum
  # "class"
  # "order"
  # "family"
  # "subfamily"
  # "genus"
  # "specific epiphet"
  # "infraspecific epiphet"
  
  colnames(template)["vTaxonRank"] <- "verbatimTaxonRank" 
  
  template$verbatimIdentification <- template$vSciName
  
  # updated taxonomy in accepted name 
  
  ## taxaID Gbif 
  
#using taxize

# GEOREFERENCING ----
# take the necessary fields from the occ template, place in georeferencing template 
# take GeoLocate columns and place in occ template "latitude","longitude", "precision" 
# if vLat, vLong != NA or vUTM != na then decimalLatitude = vLat 
# else decim
# package to convert UTM to decimal coordinates? 
  
  if (!is.na(template$vLat) & !is.na(template$vLon)){
    template$decimalLatidue <- template$vLat
    tempalte$decimalLongitude <- template$vLon
  } else if (!is.na(template$vUTM)){
    
    # convert UTM coordinates into decimal degrees
    projection <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs" # may need to change - not sure what he was using!
    
    template <- template %>% separate(vUTM, c("zone","x","y"), " ") # seperating UTM into x, and y components
    library(terra)
    points <- cbind(template$x, template$y) # making dataframe with x y components
    v <- vect(points, crs=projection) # making spatial points data frame
    z <- project(v, "+proj=longlat +datum=WGS84")  # projecting points 
    lonlat <- geom(z)[, c("x", "y")] # extracting lat lon from spatial points frame
    
    # assigning official lat and lon to these coordinates
    decimalLat <- lonlat$y
    decimalLat <- lonlat$x
  
  } else {
    template$decimalLatitude <- template$geoLocLat
    template$decimalLongitude <- template$geoLocLon
  } 
  
## ASSIGNING ASSOCIATE ROWS and TAXA---
# Associate if on the same day and in same location 
  

  
## CONSERVATION STATUS ----
  # dynamicProperties
  # some way to assign based on current gbif info with R package? 
  
  
