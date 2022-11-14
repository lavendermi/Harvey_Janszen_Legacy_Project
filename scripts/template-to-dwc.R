#######
# this script will take occurrence data from the data entry template and place it in darwin core terms

#install.packages("groundhog")
library(groundhog)

## LOADING & INSTALLING PACKAGES ----
date <- "2022-11-02"
requiredPackages <-  c("readxl","dplyr","here", "lubridate","magrittr","purrr","ritis","stringi","taxize","terra","tidyverse","tidyr")

for (pkg in requiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {groundhog.library(pkg, date)}
}
rm(requiredPackages)

## READING IN DATA ----
# change to read.csv after - coversion into csv should be the last step once template completed
template <- read_excel(here::here("data","digitized_data","HJ-occ-entry-template_22-11-11.xlsx"))

## ADDING SIMPLE DARWIN CORE COLUMNS----

  ## obtaining journal number
  template$catalogueNumber <- sapply(strsplit(template$archiveID, "-"), "[", 2)

  ## obtaining record number
  template$recordNumber <- seq.int(nrow(template)) 

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
  template <- template %>% rename(verbatimTaxonRank = vTaxonRank,
                                  occurrenceStatus = occStatus, verbatimScientificName = vSciName,
                                  scientificName = sciName, 
                                  verbatimElevation= vElevm, individualCount = numPlants, 
                                  occurrenceRemarks=occRemarks, recordedBy= collector, identificationBy = idBy, 
                                  eventDate = fulldate)
                                                             
  ## creating unique occurrence ID from archive number, page number and number on page
  template$occurrenceID <- paste0(template$archiveID, "-", template$pageNum, "-", template$numPage)
  
  ## selecting fields we want to keep for darwin core archive (tidying data)
  occ_data <- template %>% select(-pageNum, -taxonAbb, -conf, -date)   %>% arrange(occurrenceID)
                                       
## TAXONOMY FIELDS ----
    
  # Using GBIF Species-Lookup tool to check taxon names
    # create and write data frame with updated species names 
    Names <- data.frame(occ_data$occurrenceID, occ_data$scientificName)
    colnames(Names) <- c("occurrenceID","scientificName")
    write.csv(Names, here::here("data", paste0("taxa-names_", Sys.Date(), ".csv")), row.names = F)
    
    # Upload csv file to https://www.gbif.org/tools/species-lookup
    # select "match to backbone" button
    # Review the outputs - paying attention to the "matchType" column
    # if yellow or red, click edit field in the scienfificName column
    # select the desired accepted taxon - often the first one, often with authority L. 
    # repeat for all other entries with yellow or red match types
    # once all are exact or "edited" click "generate.csv" in the bottom corner
    # place in working directory
    # add the date that this table was generated
  
  # loading in the normalization table 
    normalized_names <- read.csv(here::here("data","normalized.csv"))
    
  # linking to occurrenceID in template table 
   
    normalized_names <- normalized_names %>% 
      dplyr::rename(taxonRank= rank, occurrenceID = occurrenceId) %>% 
      separate(col = species,
               into = c("GenusRepeat", "species"),
               sep = " ", remove = FALSE) %>% 
      select(-GenusRepeat, -verbatimScientificName, -confidence) %>% 
      dplyr::rename()
      
    # combining with occurrence data 
    occ_data <- occ_data %>% select(-scientificName) # removing columns to avoid duplication of updated scientific names
    
    occ_data <- merge(occ_data, normalized_names, by = "occurrenceID")

# GEOREFERENCING ----
# take the necessary fields from the occ template, place in georeferencing template 
# take GeoLocate columns and place in occ template "latitude","longitude", "precision" 
# if vLat, vLong != NA or vUTM != na then decimalLatitude = vLat 
# else decim
# package to convert UTM to decimal coordinates? 
    
  ## write a csv file to use in GEOLocate
    
    # creating formatted file for GEOLocate with relevant columns from data
    occ_to_georef <- occ_data %>% select(locality, country, stateProvince, county) %>% 
      dplyr::rename("locality string" = locality, state = stateProvince) %>% 
      add_column(latitude=NA, longitude = NA, "correction status" = NA, precision=NA, 
                 "error polygon" = NA, "multiple results" = NA, uncertainty=NA) %>% 
      add_column(occurrenceID = occ_data$occurrenceID)
    
    # writing 
    write.csv(occ_to_georef, here::here("data", paste0("occ-to-georef_", Sys.Date(), ".csv")), row.names = F)
    
  ## visit GEOLocate batch processor: https://geo-locate.org/web/WebFileGeoref.aspx
    # upload file 
    # select 128 entries per page option 
    # select options, check uncertainty box 
    # select "page georeference" 
    # might take some time
    # repeat for all pages
    # select file management at bottom
    # "export" --> delimited text csv, exclude all polygons
    # select ok
    # find exported file in downloads
    # rename to georef-occ_date.csv
    # place in data folder
    
  ## loading referenced occurrences 
    GEOlocate <- read.csv(here::here("data", "georef-occ_2022-11-14.csv"))
    # renaming columns so that they are unique when we combine them with occ_data 
    GEOlocate <- GEOlocate %>% dplyr::rename(geoLocLat = latitude, geoLocLon = longitude, geoLocPrecision = uncertainty) %>% 
      select(geoLocLat, geoLocLon, geoLocPrecision, occurrenceID)
    
  ## combining with occ_data
    occ_data <- merge(occ_data, GEOlocate, by="occurrenceID") %>% arrange(occurrenceID)

  ## separating UTM into different columns 
  ## assigning official coordinate estimates (and precision if provided verbatim or by geoLocate)
    projection <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs" # may need to change - not sure what he was using!
    
    # initializing vectors 
    UTM <- data.frame()
    points <- NULL
    v <- NULL
    z <- NULL
    lonlat <- NULL
    
 for (i in 1:dim(occ_data)[1]){   
  if (!is.na(occ_data$vLat[i]) & !is.na(occ_data$vLon[i])){
    occ_data$decimalLatidue[i] <- occ_data$vLat[i]
    occ_data$decimalLongitude[i] <- occ_data$vLon[i]
    occ_data$coordinatePrecision[i] <- occ_data$vCoodUncM[i]
    occ_data$verbatimCoordinateSystem[i] <- "degrees minutes seconds"
    occ_data$georeferenceSources[i] <- "Source"
    
  } else if (!is.na(occ_data$vUTM[i])){
    
    # convert UTM coordinates into decimal degrees
    UTM <- occ_data[i,] %>% separate(vUTM, c("zone","x","y"), " ") # seperating UTM into x, and y components
    points <- cbind(as.numeric(UTM$x), as.numeric(UTM$y)) # making dataframe with x y components
    v <- vect(points, crs=projection) # making spatial points data frame
    z <- project(v, "+proj=longlat +datum=WGS84")  # projecting points 
    lonlat <- as.data.frame(t((geom(z)[, c("x", "y")]))) # extracting lat lon from spatial points frame
    
    # assigning official lat and lon to these coordinates
    occ_data[i, "decimalLatitude"] <- lonlat$y
    occ_data[i, "decimalLongitude"] <- lonlat$x
    occ_data[i, "coordinatePrecision"] <- occ_data[i,"vCoodUncM"]
    occ_data[i, "verbatimCoordinateSystem"] <- "UTM"
    occ_data[i, "georeferenceSources"] <- "Source"
    
    # removing variables for next time through the loop
    UTM <- NULL
    points <- NULL
    v <- NULL
    z <- NULL
    lonlat <- NULL 
    
  } else if (is.na(occ_data$vUTM[i]) & (is.na(occ_data$vLat[i]) &is.na(occ_data$vLon[i]))) {
    occ_data[i, "decimalLatitude"] <- occ_data[i, "geoLocLat"]
    occ_data[i, "decimalLongitude"]<- occ_data[i,"geoLocLon"]
    occ_data[i,"coordinatePrecision"] <- occ_data[i,"geoLocPrecision"]
    occ_data[i,"georeferenceProtocol"] <- "GEOLocate batch process"
    occ_data[i, "georeferenceSources$georeferenceSource"] <- "GEOLocate"
    
  } 
 }
    
## ASSIGNING ASSOCIATE ROWS and TAXA---
# Associate if on the same day and in same location 
    
# include remarks about more specific groupings? 
    
    for (i in 1:dim(occ_data)[1]){
    occ_data[i,"assTaxa"]<- occ_data[(occ_data$eventDate[i] == occ_data$eventDate & occ_data$locality[i]==occ_data$locality),"canonicalName"] %>%  paste(collapse = "|") %>% 
    str_split(., boundary("word")) 
    }
      # take and make into "sympatric" : " X taxa" from commas? 
    # place "" around all

## CONSERVATION STATUS ----
  # dynamicProperties
  # some way to assign based on current gbif info with R package? 
    
## remove extraneous rows and export to upload to Canadensys IPT
    select(-dataEntryRemarks)
 