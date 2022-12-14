#######################################################################
###       Processing Step 3: conversion from Data Entry Template    ###
###                      to Darwin Core Occurrences                 ###
###                         Emma Menchions                          ###
###                         Created Nov. 7/22                       ###
#######################################################################

# to do: 

# decide on separating character
# field notes linking 

## Issues to still address: 
# what if geolocate guesses wrong locality? - should we be doing massive batch processes
# or smaller, more in depth ones? + # how to process so many locations in Geolocate
# verbatim coordinates, but should we be assinging those with coordinate uncertainty 
# what direction to enter locality string in (doesn't matter but need to keep consistent when entered? )
# find way to streamline to avoid going into another application
# change file path to checked data



## OVERVIEW ----
# this script will take occurrence data from the data entry 
# template and place it in darwin core terms

# OCCURRENCE DATA = data in field journals including:
  # 1) observation only occurrences (where specimen has not been collected - 
      # no collection number and no note of collection)
  # 2) observations of associated taxa in notes of collected specimen 
  # 3) checklist / surveys containing groups of species names in different sites 
      # (each species observation is a row)
  # 4) Survey data - where abundances have been collected

# what should not be entered? 
  # checklist data from other people's observations/ collections 

# before using this script should have
  # 1) a completed template with occurrence data 
  # 2) observations with low confidence in deciphering field notes have been 
      # removed or checked over and corrected (mostly with species names)

## 1) LOADING & INSTALLING PACKAGES ----
  # using groundhog to manage package versioning 
  #install.packages("groundhog")
  library(groundhog)
  
  date <- "2022-11-02"
  requiredPackages <-  c("assertr","dplyr","here", "lubridate",
                         "magrittr","purrr","ritis",
                         "stringi","taxize","terra","tidyverse","tidyr")
  
  for (pkg in requiredPackages) {
    if (pkg %in% rownames(installed.packages()) == FALSE)
    {install.packages(pkg)}
    if (pkg %in% rownames(.packages()) == FALSE)
    {groundhog.library(pkg, date)}
  }
  rm(requiredPackages)

## 2) READING IN DATA ----
  
  index <- c(7,8,9,27) # USER INPUT 
  
  for (i in index){
    if (i == index[1]){
      data <- read.csv(
        here::here("data","digitized_data",
                   "occurrence_data", 
                    "clean_data", 
                    paste0("HJ-",index[1], "_clean-occurrences.csv"))) 
    } else{ 
      data <- rbind(data, read.csv(
        here::here("data","digitized_data",
                   "occurrence_data",
                    "clean_data", 
                    paste0("HJ-",i,"_clean-occurrences.csv"))))
    }
  }
  
## 3) ADDING/ AUTOMATICALLY FILLING IN SIMPLE DARWIN CORE FIELDS ----

  ## dwc:datasetName
  data$datasetName <- "Harvey Janszen Observations"

  ## dwc:basisOfRecord
  # Human observation = (report by a known observer that an organism was 
  # present at the place and time)
  # all records should be this type 
  data$basisOfRecord <- "HumanObservation"

  ## dwc: "year", "month", "day" 
  # splitting YYYYMMDD date entered into separate year, month, day columns
  data <- data %>%
    dplyr::mutate(.data = data, fulldate = lubridate::ymd(date),
                  year = lubridate::year(fulldate), 
                  month = lubridate::month(fulldate), 
                  day = lubridate::day(fulldate))
  
  ## Converting abundance codes into averages of their indicated range 
  # on the front cover of HJ-7 field journal, and then assigning code to the 
  # occurrence remarks for reference of the uncertainty range
  # which can be linked to the metadata
  data$numPlantsCode <- as.numeric(data$numPlantsCode)
  
  for (i in 1:dim(data)[1]){ # for all rows
    if(!is.na(data$numPlantsCode[i])){ # if there is a number of plants code
      if (data$numPlantsCode[i] == 0){ # if the num plants code is =0 ...
        data[i,"orgQuantity"] <- "1" # assign it to an individual count of 1
        data[i,"orgQtype"] <- "individuals"
        
      }else if (data$numPlantsCode[i] == 1){ 
        data[i,"orgQuantity"] <- "1-5"
        data[i,"orgQtype"] <- "individuals"
      
      }else if (data$numPlantsCode[i] == 2){
        data[i,"orgQuantity"] <- "5-25"
        data[i,"orgQtype"] <- "individuals"
      
      } else if (data$numPlantsCode[i] == 3){
        data[i,"orgQuantity"] <- "25-50"
        data[i,"orgQtype"] <- "individuals"
        
      }else if (data$numPlantsCode[i] == 4){
        data[i,"orgQuantity"] <- "50-75"
        data[i,"orgQtype"] <- "individuals"
        
      }else if (data$numPlantsCode[i] == 5){
        data[i,"orgQuantity"] <- "75+"
        data[i,"orgQtype"] <- "individuals"
      }
      if(!is.na(data$occRemarks[i])){ # if there are already other occurrence remarks...
        data[i,"occRemarks"] <- paste(data[i, "occRemarks"],
                                      ";","abundance code", # add this to the end
                                paste0("'",data[i,"numPlantsCode"],"'"))
      } else { # if there are no occurrence remarks already...
        data[i,"occRemarks"] <- paste0("abundance code:", " '",
                                data[i,"numPlantsCode"],"'") # assign it as the occurrence remark
      }
    }
  }
      
  ## dwc: recordedBy
  # automatically assigning Harvey Janszen as collector if no other notes
  
  for(i in 1:dim(data)[1]){ # for each observation
    if(is.na(data$recordedBy[i])){ # if there were not other collectors mentioned
      data$recordedBy[i] <- "Harvey Janszen" # fill field with name
    }else{ # if other collector names noted 
      data$recordedBy[i] <- paste0("Harvey Jaszen, ", data$recordedBy[i]) # add his name as first
      # collector 
    }
  } 
  
  ## dwc: verbatimTaxonRank 
  # automatically adding "genus" if length(sciName) == 1, otherwise "species"
  
  for(i in 1:dim(data)[1]){  # for each row
    if(is.na(data$vTaxonRank[i])){ # if there is no taxon rank already...
      if (length(strsplit(data$sciName[i], " ")[[1]])==1){ # if the length of the sciName string ==1,,,
        data$vTaxonRank[i] <- "genus" # assign genus
      }else if (length(strsplit(data$sciName[i], " ")[[1]])==2){ # if 2...
        data$vTaxonRank[i] <- "species" # assign species
      }else if (length(strsplit(data$sciName[i], " ")[[1]])==3) # if 3...
        data$vTaxonRank[i] <- "subspecies" # assing subspecies
    }
  }
  
  # checking to make sure all rows now have vTaxonRank
  # constraints: 1) in set of ranks
  # 2) read as character
  
  ranks <- c("subspecies","species", "genus", "family", "order", 
             "class", "phylum", "kindgom")
  
  # if there are non-Na elements...
  if(length(data$vTaxonRank[is.na(data$vTaxonRank)])
     <length(data$vTaxonRank)){
    # check that... 
    data %>% 
      group_by(vTaxonRank) %>% 
      chain_start %>% 
      #column is read as character 
      verify(., has_class("vTaxonRank", class="character")) %>% 
      # characters are within set 
      assert(in_set(ranks),vTaxonRank)  %>% 
      chain_end
  }
  
  ## dwc: occurrenceStatus
  # automatically assigning occurrence status as present if it 
  # was not filled in
  for(i in 1:dim(data)[1]){ # for each observation
    if(is.na(data$occStatus[i])){ # if there was no remark
      data$occStatus[i] <- "present" # assign as present
    }
  } 
  
  ## dwc: islandGroup
  ## automatically assigning island group, if the island field was filled in 
  places <- read.csv(here::here("data", "reference_data",
                                "islands-and-districts_22-11-29.csv"))
  
  for(i in 1:dim(data)[1]){ # for each observation
    if(!is.na(data$island[i])){ # if there was no remark
      data$islandGroup[i] <- places[places$island==data$island[i], 
                                    "islandGroup"]
    }
  } 
  
  # checking that all were assigned an island group and that it is 
  # spelled correctly
  data %>% 
    chain_start %>% 
    assert(not_na,islandGroup) %>% 
    assert(in_set(places$islandGroup),islandGroup) %>% 
    chain_end
  
  ## dwc: county
  ## automatically assign as the island if there is an island which 
  # is not Vancouver island 
  
  for(i in 1:dim(data)[1]){ # for each observation
    
    if(is.na(data$county[i])){ # if the county is not stated
      if(data$island[i] != "Vancouver Island" | # and it is not on Vancouver island 
       data$island[i] != "Vancouver"){ 
        
      # assing the county to the island group of that island 
      data$county[i] <- places[places$island==data$island[i],
                               "islandGroup"]
      }
    }
  }
  
    # checking that all were assinged a county
    data %>% 
    assert(not_na,county) 
  
  ## assColl (associated collections)
  # adding "HJC-" to the beginning of collection numbers entered here to indicate
  # that it is a collected specimen 
  # if string split > 1 " " 
  
  taxa <- NULL #initializing vectors
  taxa_coll <- NULL
  
  for (i in 1:dim(data)[1]){ # for each obs
    if (!is.na(data$assColl[i])){ # if there is an associated species 
                                  # already listed (collected specimens)
      if (length(str_split(data$assColl[i], ", ")[[1]])>1){ 
        # if there is more than one ...
        taxa <- str_split(data$assColl[i], ", ")[[1]]
        
        for (j in 1:length(taxa)){ # for each occurrence number 
          # input "HJC-" before the number to indicate collected specimen number
          taxa_coll[j] <- paste0("HJC-",taxa[j]) 
        }
        # and assign this to the associated species column for that row
        
        data$assColl[i] <- paste(taxa_coll, collapse= "; ")
        
      } else if (length(str_split(data$assColl[i], ", "))==1) { # if there is only one 
                                                    # associated occurrence already...
        # paste "HJC-" infront and assign to the associated species column for that row
        data$assColl[i] <- paste0("HJC-",data$assColl[i])
      }
    }
  }
  
  ## dwc: recordNumber
  # assigning record number as a combined number of the archiveID, 
  # pageNumber and number on page
  data$recordNumber <- paste0("HJ",data$archiveID, "-", 
                              data$pageNum, "-", data$numPage)
  
  # dwc:fieldNotes
  # stating where the field notes are stored and where the images are 
  data$fieldNotes <- "Imaged notes: X,  Original notes housed at UBC herbarium"
  
  ## dwc: occurrenceId: creating globally unique identifier in 
  # chronological order and sequence in journals
  # starting at 1 
  data$occurrenceID <- paste0("HJO",1:dim(data)[1])
  
  ## removing fields we don't need anymore for darwin core archive
  # (tidying data)
  occ_data <- data %>% 
    select(-pageNum,-numPage, -vName, -conf, -date) %>%
    arrange(occurrenceID)
                                       
## 4) TAXONOMY FIELDS ----
  
  # Using GBIF Species-Lookup tool to check taxon names (updated 
  # input names, not verbatim)
    # create and write data frame with updated species names from template 
    Names <- occ_data %>% 
    dplyr::select(occurrenceID, sciName) %>% 
    distinct(sciName, .keep_all =T) %>% 
    filter(!is.na(sciName)) %>% 
    dplyr::rename(scientificName = sciName)
  
    write.csv(Names, here::here("data", "reference_data", "taxonomy", 
                                paste0("taxa-names_",
                                Sys.Date(), ".csv")), row.names = F)
    
    # Upload csv file to https://www.gbif.org/tools/species-lookup
    # select "match to backbone" button
    # Review the outputs - paying attention to the "matchType" column
    # if yellow or red, click edit field in the scienfificName column
    # select the desired accepted taxon - often the first one, 
    # often with authority L. 
    # repeat for all other entries with yellow or red match types
    # once all are exact or "edited" click "generate.csv" in the 
    # bottom corner
    # place in working directory
    # add the date that this table was generated
  
  # loading in the names normalization table 
    normalized_names <- read.csv(here::here("data","reference_data",
                                            "taxonomy","normalized.csv"))
    
  # linking to occurrenceID in template table
      
    # combining gbif taxon match data with occurrence data 
      cols <- names(normalized_names)[3:length(names(normalized_names))]
      
      # initializing empty columns to fill 
      for (i in 1:length(cols)){
        occ_data[,cols[i]] <-NA
      }
      
     occ_data <- occ_data %>% # temporary!!!!!!!!!!!!!!!!!!!!!!!!!!!
       filter(!is.na(sciName))
     
      for (i in 1:dim(occ_data)[1]){
        for (k in cols){
          occ_data[i, k] <- normalized_names[which(occ_data$sciName[i] ==
                                  normalized_names$verbatimScientificName), k]
        }
        
      }
      
     speciesSep <- as.data.frame(str_split_fixed(occ_data$canonicalName, " ", 3)) %>% 
       dplyr::rename(genus = V1, specificEpithet = V2, intraspecificEpithet = V3) %>% 
       select(-genus)
       
     occ_data$specificEpithet <- speciesSep[,1]
     occ_data$intraspecificEpithet <- speciesSep[,2]
      occ_data <- occ_data %>% 
        # making sure that empty cells are NA
        mutate_all(na_if, "") %>% 
        # removing columns to avoid duplication 
        select(-sciName) 

## 5) GEOREFERENCING ----
    
  ## write a csv file to use in GEOLocate
    
    # creating formatted file for GEOLocate with relevant columns from data
    # maybe remove ones with coordinates already? 
    
    # selecting 
    localities_to_georef <- occ_data %>% 
                      # filtering for rows without verbatim coordinate info
                      dplyr::filter(is.na(vLat)) %>% 
                      dplyr::filter(is.na(vLon)) %>% 
                      dplyr::filter(is.na(vUTM)) %>% 
                      # selecting columns with relevant location info
                      select(locality, country, 
                              stateProvince, county) %>% 
      # only selecting rows with distinct locality strings
      distinct(locality,country,stateProvince, county) %>% 
      # renaming columns to match GEOLocate column formatting conventions
      dplyr::rename("locality string" = locality, state = 
                                        stateProvince) %>% 
      # adding columns for extra information collection in GEOLocate
      add_column(latitude=NA, longitude = NA, "correction 
                 status" = NA, precision=NA, 
                 "error polygon" = NA, "multiple results" = NA, uncertainty=NA)
    
    # writing to data folder
    write.csv(localities_to_georef, here::here("data", "reference_data", "georeferencing",
                                        paste0("localities-to-georef_", 
                                        Sys.Date(), ".csv")), 
                                        row.names = F)
    
  ## visit GEOLocate batch processor: https://geo-locate.org/web/WebFileGeoref.aspx
    # upload file 
    # select options, check uncertainty box 
    # select "page georeference" or georeferencing one at a time
    # click on first row in table
    # assess the options it provides (green as highest probability, red as other options)
    # ... add protocol 
    # edit uncertainty
    # press correct button
    # move onto next observation
    # might take some time
    # repeat for all pages
    
    # once finished
    # select file management at bottom
    # "export" --> delimited text csv, exclude all polygons
    # select ok
    # find exported file in downloads
    # rename to geoLocate_YYYY-MM-DD.csv
    # place in data folder
    
  ## loading referenced occurrences 
    GEOlocate <- read.csv(here::here("data", "reference_data",
                                     "georeferencing", "geoLocate.csv")) 
    # renaming columns so that they are unique when we combine them with occ_data 
    GEOlocate <- GEOlocate %>% dplyr::rename(geoLocLat = 
                                               latitude, geoLocLon = longitude, 
                                             geoLocPrecision = uncertainty, 
                                             locality = locality.string) %>% 
      # selecting relevant columns 
      select(locality, geoLocLat, geoLocLon, geoLocPrecision, county) %>% 
      # removing the notation "m" from some rows for coordinate uncertainty
      separate(geoLocPrecision, into = c("geoLocPrecision", "m"), sep = " ") %>% 
      select(-m) 


  ## assigning official coordinate estimates 
    # (and precision if provided verbatim or by GEOLocate)
    
    # for loop that checks for verbatim coordinates 
    # (in decimal degrees in vLat, vLon columns or UTM in vUTM column). 
    # if verbatim degrees provided and in degrees, minutes, seconds format
    # it converts these to decimal degrees and assigns them as official coordinates. 
    # if verbatim degrees provided in decimal degrees
    # automatically assigns these as official coordinates.
    # if verbatim coordinates provided in UTM, 
    # converts these to decimal degrees and assigns them as official
    # if NO verbatim coordinates provided, 
    # coordinate estimates from GEOlocate table used
    # and assigned as official coordinates.
    
    # converts UTM coordinates based on this projection: 
    # may need to change - not sure what he was using!
    projection <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs" 
    
    # Additionally assigns verbatim coordinate uncertainty from verbatim
    # coordinates if present or assigns GEOLocate estimated uncertainty in METERS
    # official coordinate fields = "decimalLatitude" and "decimalLongitude" 
    
    # initializing vectors 
    UTM <- data.frame()
    points <- NULL
    v <- NULL
    z <- NULL
    lonlat <- NULL
    occ_data$decimalLatitude <- NA
    occ_data$decimalLongitude <- NA
    occ_data$coordinateUncertaintyInMeters <- NA
    occ_data$verbatimCoordinateSystem <- NA
    occ_data$georeferenceSources <- NA
    
 for (i in 1:dim(occ_data)[1]){  # for each row 
  if (!is.na(occ_data$vLat[i]) & !is.na(occ_data$vLon[i]) & is.na(occ_data$vUTM)[i] | 
      !is.na(occ_data$vLat[i]) & !is.na(occ_data$vLon[i]) & !is.na(occ_data$vUTM)[i]){ 
    # if there are verbatim lat and lon coordinates...
    # of if there are verbatim lat, lon and UTM coordinates... (use lat lon)
    
    if((length(strsplit(occ_data$vLat[i], " ")[[1]]) > 1)){ 
      # if the coordinates are in degrees, minutes, second ...
      
          # convert degrees, minutes, seconds to decimal
          source(here::here("scripts", "functions", "angle2dec.R"))
      
          occ_data$decimalLatitude[i] <- angle2dec(occ_data$vLat[i])
          occ_data$decimalLongitude[i] <- angle2dec(occ_data$vLon[i])
          occ_data$coordinateUncertaintyInMeters[i] <- occ_data$vCoordUncM[i]
          
          # assigning dwc field regarding coordinate system 
          occ_data$verbatimCoordinateSystem[i] <- "degrees minutes seconds" 
          occ_data$georeferenceSources[i] <- "Source" # indicates coordinates verbatim 
          
    } else if (length(strsplit(occ_data$vLat[i], " ")[[1]]) ==1){
      # if lat lon are in decimal degrees...
      
      occ_data$decimalLatidue[i] <- occ_data$vLat[i]
      occ_data$decimalLongitude[i] <- occ_data$vLon[i]
      occ_data$coordinateUncertaintyInMeters[i] <- occ_data$vCoodUncM[i]
      occ_data$verbatimCoordinateSystem[i] <- "decimal degrees"
      occ_data$georeferenceSources[i] <- "Source" # indicates coordinates verbatim 
    }
    
  } else if (!is.na(occ_data$vUTM[i]) & is.na(occ_data$vLat[i]) & is.na(occ_data$vLon[i])){
    # if only UTM coordinates provided...
    
    ## convert UTM coordinates into decimal degrees
    
      # seperating UTM into x, and y components
      UTM <- occ_data[i,] %>% separate(vUTM, c("zone","x","y"), " ") 
      # making dataframe with x y components
      points <- cbind(as.numeric(UTM$x), as.numeric(UTM$y)) 
      # making spatial points data frame
      v <- vect(points, crs=projection) 
      # projecting points using assinged crs  
      z <- project(v, projection)  
      # extracting lat lon from spatial points frame
      lonlat <- as.data.frame(t((geom(z)[, c("x", "y")]))) 
    
    # assigning official lat and lon to these coordinates
    occ_data[i, "decimalLatitude"] <- lonlat$y
    occ_data[i, "decimalLongitude"] <- lonlat$x
    occ_data[i, "coordinateUncertaintyInMeters"] <- occ_data[i,"vCoodUncM"]
    occ_data[i, "verbatimCoordinateSystem"] <- "UTM" # assigning dwc field 
    occ_data[i, "georeferenceSources"] <- "Source" # indicates coordinates verbatim 
    
    # removing variables for next time through the loop
    UTM <- NULL
    points <- NULL
    v <- NULL
    z <- NULL
    lonlat <- NULL 
    
  } else if (is.na(occ_data$vUTM[i]) & (is.na(occ_data$vLat[i]) 
                                        & is.na(occ_data$vLon[i]))) {
    
    # if no verbatim coordinates provided in either format...
      
      occ_data[i, c("decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters")] <-  
        GEOlocate[which(occ_data$locality[i] == GEOlocate$locality &
                        occ_data$county[i] == GEOlocate$county),
                         c("geoLocLat", "geoLocLon", "geoLocPrecision")] 
      occ_data[i,"georeferenceProtocol"] <- "GEOLocate batch process"
      occ_data[i, "georeferenceSources$georeferenceSource"] <- "GEOLocate"
    
  } 
 }
    
## 6) ASSIGNING ASSOCIATE ROWS and TAXA----
    ## Issues to still address: 
      # concaternate with already entered associated species not in occurrence sheet - ones associated with specimen
      # include remarks about more specific groupings? - like in shade at a particular site, or base vs seep vs edge? - might be more difficult because these don't neatly fit into dwc terms...
      # assigning quotes around list terms 
      # "sympatric" : " X taxa" or another term? 
    
    # at this point is should be taking occurrences from same time place, locality,
    # habitat and writing species name and occurrence in field, pasted onto those already there
    # MAKE SURE SAME UTM / COORDINATES IF PROVIDED TOO 
    
# for loop: for each row, find other rows with matching date AND locality AND habitat, assign the scientificNames names of these rows 
    # to an "associatedTaxa" field, where names are separated by "|" preffered for dwc lists like these
    
# temporarily make missing values in habitat column "missing"
    occ_data[is.na(occ_data$habitat), "habitat"] <- "missing" 
    
for (i in 1:dim(occ_data)[1]){ # for every row
  
  # if there is another row that matches date, and place...
  if (length(which((occ_data$fulldate[i] == occ_data$fulldate & 
                    occ_data$locality[i]==occ_data$locality &  
                    occ_data$habitat[i]==occ_data$habitat &
                    occ_data$decimalLatitude[i]==occ_data$decimalLatitude &
                    occ_data$decimalLongitude[i]==occ_data$decimalLongitude))) > 1){
    
    nameVec <- as.vector(occ_data[occ_data$fulldate[i] == occ_data$fulldate & 
                                     occ_data$locality[i]==occ_data$locality &  
                                     occ_data$habitat[i]==occ_data$habitat &
                                     occ_data$decimalLatitude[i]==occ_data$decimalLatitude &
                                     occ_data$decimalLongitude[i]==occ_data$decimalLongitude
                                   ,"canonicalName"])
        
        # if there is no collected associated taxa recorded...
        if (is.na(occ_data$assCollTaxa[i])){
            
          # for every match 
          for (j in 1:length(nameVec)){
            # for the first match, just input this name
            if (j == 1){
              
              occ_data[i,"assCollTaxa"] <- nameVec[j]
            
            # for the subsequent matches, paste it to the other name
            # separated by a special character
            } else if (j > 1) {
              
              occ_data[i,"assCollTaxa"]<- paste(occ_data[i, "assCollTaxa"],nameVec[j],sep = " ; ")
              
            }
          }
          
        # or if there is already a collected associated taxa listed...
        } else if (!is.na(occ_data$assCollTaxa[i])){   
          # for every match...
          for (j in 1:length(nameVec)){
              
              occ_data[i,"assCollTaxa"] <- paste(occ_data[i,"assCollTaxa"],nameVec[j], sep = " ; ")
                            
          }
       }
  }
}
    
# for loop: repeating the loop above but for record and collection numbers
    
for (i in 1:dim(occ_data)[1]){ # for every row
      
      # if there is another row that matches date, and place...
      if (length(which((occ_data$fulldate[i] == occ_data$fulldate & 
                        occ_data$locality[i]==occ_data$locality &  
                        occ_data$habitat[i]==occ_data$habitat &
                        occ_data$decimalLatitude[i]==occ_data$decimalLatitude &
                        occ_data$decimalLongitude[i]==occ_data$decimalLongitude))) > 1){
        
        numVec <- as.vector(occ_data[occ_data$fulldate[i] == occ_data$fulldate & 
                                        occ_data$locality[i]==occ_data$locality &  
                                        occ_data$habitat[i]==occ_data$habitat &
                                        occ_data$decimalLatitude[i]==occ_data$decimalLatitude &
                                        occ_data$decimalLongitude[i]==occ_data$decimalLongitude
                                      ,"recordNumber"])
        
        # if there is no collected associated taxa recorded...
        if (is.na(occ_data$assColl[i])){
          
          # for every match 
          for (j in 1:length(numVec)){
            # for the first match, just input this name
            if (j == 1){
              
              occ_data[i,"assColl"] <- numVec[j]
              
              # for the subsequent matches, paste it to the other number
              # separated by a special character
            } else if (j > 1) {
              
              occ_data[i,"assColl"]<- paste(occ_data[i, "assColl"],numVec[j],sep = " ; ")
              
            }
          }
          
          # or if there is already a collected associated record num listed...
        } else if (!is.na(occ_data$assColl[i])){   
          # for every match...
          for (j in 1:length(numVec)){
            
            occ_data[i,"assColl"] <- paste(occ_data[i,"assColl"],numVec[j], sep = " ; ")
            
          }
        }
      }
    } 
    
    
# converting "missing" habitat data back into NA
  occ_data[occ_data$habitat=="missing", "habitat"] <- NA
  
## 7) CONSERVATION STATUS ----
  ## Issues to address
    # 1) what columns should status info go in - dynamic properties (RBCM), taxonRemarks 
    # 2) way to automate download of file into R 
    # IUCN, COSEWIC AND CDC status?
    # 3) format of fields
    # BC CDC list 
    # {"Conservation Status: CDC: ,"Conservation status: COSEWIC: , "Conservation Status: IUCN: ",
    
    # example Symph alb - two subspecies with different ranks - one ssp. albus - can we assume it is this one or leave undefined?
    # scripts to periodically update the statuses of these data? 
    
## Provincial ranks 
    # visit BC Ecosystems Explorer: https://a100.gov.bc.ca/pub/eswp/
    # search plants category
    # "export results"
    # results table
    # open and export as .csv 
    
    ## reading downloaded BC Conservation Data centre ranks (BC CDC)
    CDC <- read.csv(here::here("data","reference_data","CDC-resultsExport_2022-11-15.csv"))
    
    ## splitting scientific name into genus, species, var/ subspecies, and intraspecific epithet 
    CDC_cleaned <- CDC %>% 
      separate(col = Scientific.Name, # separating species column into genus and species to obtain specific ephithet
               into = c("genus", "species", "abb", "intraspecificEpithet"),
               sep = " ", remove = FALSE) %>% 
     unite("fullName", genus, species, intraspecificEpithet,sep = " ", na.rm=T, remove=F) %>% 
     unite("scientificName", genus, species, sep=" ", na.rm=T) # uniting genus and species again for ease of searching names
    
  ## assigning CDC status (S5 to S1)
    # initializing vectors for for loop 
    occ_data$provincialStatus <- NA
    
    for (k in 1:dim(occ_data)[1]){ # for each occurrence observation
      
      if (!is.na(occ_data$intraspecificEpithet[k])){ # for observations that have species with intraspecific epithet (subspecies)
        # does the genus, species and intraspeicific epithet match one in the CDC? 
        if(length(CDC_cleaned[occ_data$canonicalName[k]==CDC_cleaned$fullName, "Provincial"])==1){
        occ_data$provincialStatus[k]<- CDC_cleaned[occ_data$canonicalName[k]==CDC_cleaned$fullName, "Provincial"]
        }
        
      } else { # if there is no intraspecific epithet listed... 
        
        # if there is match for genus and species name in CDC...
        if(length(CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "Provincial"]) > 1){ 
          
          # for how ever many number of subspecies there are ...
          for (j in 1:length(CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "Provincial"])){ 
            
            # do all of possible subspecies have same list code? 
            if (length(unique(CDC_cleaned[(CDC_cleaned$scientificName == occ_data$canonicalName[k]),"Provincial"])) == 1){ 
              # if so, then apply that list category to that row
              occ_data[k, "provincialStatus"] <- CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "Provincial"][1]
              
            } else { # if multiple subspecies with different list categories and we don't know what subspecies
              occ_data[k, "provincialStatus"]  <- NA 
            } 
          }
          
        # if only one name that matches
        } else if (length(CDC_cleaned[CDC_cleaned$fullName == occ_data$canonicalName[k], "Provincial"]) == 1){ 
          occ_data[k, "provincialStatus"] <- CDC_cleaned[CDC_cleaned$fullName == occ_data$canonicalName[k], "Provincial"]
          
        }
      }
    }
    
  ## assigning global status (G5 to G1)
    # initializing vectors for for loop 
    occ_data$globalStatus <- NA
    
    for (k in 1:dim(occ_data)[1]){ # for each occurrence observation
      
      if (!is.na(occ_data$intraspecificEpithet[k])){ # for observations that have species with intraspecific epithet (subspecies)
        # does the genus, species and intraspeicific epithet match one in the CDC? 
        if(length(CDC_cleaned[occ_data$canonicalName[k]==CDC_cleaned$fullName, "Global"])==1){
          occ_data$globalStatus[k]<- CDC_cleaned[occ_data$canonicalName[k]==CDC_cleaned$fullName, "Global"]
        }
        
      } else { # if there is no intraspecific epithet listed... 
        
        # if there is match for genus and species name in CDC...
        if(length(CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "Global"]) > 1){ 
          
          # for how ever many number of subspecies there are ...
          for (j in 1:length(CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "Global"])){ 
            
            # do all of possible subspecies have same list code? 
            if (length(unique(CDC_cleaned[(CDC_cleaned$scientificName == occ_data$canonicalName[k]),"Global"])) == 1){ 
              # if so, then apply that list category to that row
              occ_data[k, "globalStatus"] <- CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "Global"][1]
              
            } else { # if multiple subspecies with different list categories and we don't know what subspecies
              occ_data[k, "globalStatus"]  <- NA 
            } 
          }
          
          # if only one name that matches
        } else if (length(CDC_cleaned[CDC_cleaned$fullName == occ_data$canonicalName[k], "Global"]) == 1){ 
          occ_data[k, "globalStatus"] <- CDC_cleaned[CDC_cleaned$fullName == occ_data$canonicalName[k], "Global"]
          
        }
      }
    }
    

    
## saving as .csv file  ----
## remove extraneous columns  
    
    dwc_data <- occ_data %>% 
      ## assigning statuses as dwc::dyamic properties
      #unite("dynamicProperties", provincialStatus, globalStatus, sep="; ") %>% 
      
      # add new column for min and max elevs
      
      # renaming columns to dwc terms 
      dplyr::rename(verbatimScientificName = vSciName, 
                    verbatimElevtion = vElevM,
                    identificationQualifier = idQualifier,
                    occurrenceStatus = occStatus, 
                    associatedTaxa = assCollTaxa, 
                    associatedOccurrences = assColl, 
                    verbatimTaxonRank=vTaxonRank, 
                    verbatimLatitude = vLat, 
                    verbatimLongitude = vLon, 
                    verbatimCoordinates = vUTM, 
                    organismQuanity = orgQuantity, 
                    organismQuantityType = orgQtype, 
                    occurrenceRemarks = occRemarks, 
                    lifeStage = phenology, 
                    identificationBy= idBy, 
                    scientificNameauthorship = authorship, 
                    taxonRank = rank, 
                    taxonomicStatus = status, eventDate = fulldate) %>% 
      
      # removing columns 
      select(-archiveID, -dataEntryRemarks, -canonicalName, -confidence,
             -numPlantsCode)
    
## saving csv file
   write.csv(dwc_data, here::here("data", "digitized_data", "occurrence_data",paste0("darwin-core-occurrences_", Sys.Date(), ".csv")), row.names = F)
    
