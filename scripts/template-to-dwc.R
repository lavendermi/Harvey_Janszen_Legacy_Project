#######
# this script will take occurrence data from the data entry template and place it in darwin core terms

#install.packages("groundhog")
library(groundhog)

## LOADING & INSTALLING PACKAGES ----
date <- "2022-11-02"
requiredPackages <-  c("readxl","dplyr","here", "lubridate","magrittr","purrr","ritis","stringi","taxize","tidyverse","tidyr")

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
  template <- template %>% rename(occurrenceID = occID, verbatimTaxonRank = vTaxonRank,
                                  occurrenceStatus = occStatus, verbatimScientificName = vSciName,
                                  scientificName = sciName, 
                                  verbatimElevation= vElevm, individualCount = numPlants, 
                                  occurrenceRemarks=occRemarks, recordedBy= collector, identificationBy = idBy, 
                                  eventDate = fulldate)
                                                             
  ## adding journal page number to occurrence ID
    stri_sub(template$occurrenceID, 7, 6) <- template$page
    stri_sub(template$occurrenceID, 8, 7) <- "-"
    
  ## selecting fields we want to keep for darwin core archive (tidying data)
    occ_data <- template %>% select(-page, -taxonAbb, -conf, -date)  
                                       
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
  
  # Using GBIF Species-Lookup tool to check taxon names
    # create and write data frame with updated species names 
    Names <- data.frame(occ_data$occurrenceID, occ_data$scientificName)
    colnames(Names) <- c("occurrenceID","scientificName")
    write.csv(Names, "names.csv", row.names = F)
    
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
    normalized_names <- read.csv(here::here("data","digitized_data","normalized.csv"))
    
  # linking to occurrenceID in template table 
   # removing columns to avoid duplication of updated scientific names
    template <- template %>%
      select(-sciName)
    
    normalized_names <- normalized_names %>% 
      dplyr::rename(taxonRank= rank) %>% 
      separate(col = species,
               into = c("GenusRepeat", "species"),
               sep = " ", remove = FALSE) %>% 
      select(-GenusRepeat) %>% 
      select(-verbatimScientificName) %>% 
      dplyr::rename(specificEpiphet=species)
      
    occ_data <- occ_data %>% select(occurrenceID, )
    
    
  # 2) Using acceptedName? to indicate updated scienfic name
    
    
  # 3) Upstream taxonomy 
    taxa <- classification(unlist(template$sciName), db = "gbif")
    status="ACCEPTED"
    ## convert this into a useable data format (from a nested list to a tibble)
    taxaList <- taxa %>% 
      rbind() %>% ## bind_rows() doesn't work because the output is not a typical list...
      tibble() %>% 
      ## drop database 'id' column
      ## keep 'query' column for indexing while pivoting below
      
      select(-id) %>% 
      ## we only want the 'traditional' taxonomic levels (i.e., KPCOFGP)
      filter(rank %in% c("kingdom", "phylum", "class", "order", 
                         "family", "genus", "species")) %>%
      pivot_wider(., id_cols = "query", names_from = "rank", values_from = "name") %>% 
      ## drop the 'query' column (same as 'species')
      select(-query)
    
    template$family <- sapply(template$sciName, itis_name, get = "family", USE.NAMES = F)
    x <- tax_name(template$sciName, get = 'family', db = 'ncbi')
    newNamesgnr <- gnr_resolve(template$vSciName)
  
    x <- gbif_parse(template$vSciName)
  # 4) assign GBIF taxa ID 
    
  template$verbatimIdentification <- template$vSciName
  template$acceptedNameUsage
  
  library(taxize)
  try <- taxize::iplant_tnrsmatch(retrieve = "all", taxnames = template$vSciName, output = "names")
  newNames <- gnr_resolve( canonical = T, data_source_ids = 11, sci = template$vSciName, returndf = TRUE)
  newNames <- gnr_resolve( canonical = T, sci = template$vSciName, returndf = TRUE)
  
  newNames <- plantminer(template$vSciName)
  NewNames <- tpl_search(template$vSciName)
  
  install.packages("rgbif")
  library(rgbif)
  
  gbifnames <- name_backbone(template$vSciName)
  
  classification(template$vSciName[1], db = 'gbif')
  
  # updated taxonomy in accepted name 
  
  ## taxaID Gbif 
  
#using taxize

# GEOREFERENCING ----
# take the necessary fields from the occ template, place in georeferencing template 
# take GeoLocate columns and place in occ template "latitude","longitude", "precision" 
# if vLat, vLong != NA or vUTM != na then decimalLatitude = vLat 
# else decim
# package to convert UTM to decimal coordinates? 
  
  ## assigning official coordinate estimates (and precision if provided verbatim or by geoLocate)
    
  if (!is.na(template$vLat) & !is.na(template$vLon)){
    template$decimalLatidue <- template$vLat
    tempalte$decimalLongitude <- template$vLon
    template$coordinatePrecision <- template$vCoodUncM
    template$verbatimCoordinateSystem <- "degrees minutes seconds"
    template$georeferenceSources <- "Source"
    
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
    template$decimalLat <- lonlat$y
    template$decimalLat <- lonlat$x
    template$coordinatePrecision <- template$vCoodUncM
    template$verbatimCoordinateSystem <- "UTM"
    template$georeferenceSources <- "Source"
    
  } else {
    template$decimalLatitude <- template$geoLocLat
    template$decimalLongitude <- template$geoLocLon
    template$coordinatePrecision <- template$geoLocPrecision
    template$georeferenceProtocol <- "GEOLocate batch process"
    template$georeferenceSources <- "GEOLocate"
    
  } 
    
    
  
## ASSIGNING ASSOCIATE ROWS and TAXA---
# Associate if on the same day and in same location 
  

  
## CONSERVATION STATUS ----
  # dynamicProperties
  # some way to assign based on current gbif info with R package? 
  
 