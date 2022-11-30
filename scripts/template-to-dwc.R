#######################################################
### Data Entry Template to Darwin Core Occurrences  ###
###               Emma Menchions                    ###
###             Created Nov. 7/22                   ###
#######################################################
# to do: 

# shoudl taxon name checking go in step ii of processing? 
# ass taxa names separation 

# decide on separating character

# event date
# field notes linking 

## Issues to still address: 
# What happens if row has both degrees and UTM? Which one is more reliable
# what if geolocate guesses wrong locality? - should we be doing massive batch processes
# or smaller, more in depth ones? 
# find way to streamline to avoid going into another application
# if both degrees and UTM - which one should I take? which one most accurate? 

# length of time it takes to go through and edit taxon names to gbif backbone? 1-2 min for 237 obs. 


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
  requiredPackages <-  c("readxl","dplyr","here", "lubridate","magrittr","purrr","ritis",
                         "stringi","taxize","terra","tidyverse","tidyr")
  
  for (pkg in requiredPackages) {
    if (pkg %in% rownames(installed.packages()) == FALSE)
    {install.packages(pkg)}
    if (pkg %in% rownames(.packages()) == FALSE)
    {groundhog.library(pkg, date)}
  }
  rm(requiredPackages)

## 2) READING IN DATA ----
  index <- c(7,8,9,27)
  for (i in index){
    if (i == index[1]){
      data <- read_excel(here::here("data","digitized_data",
                                    paste0("HJ-",index[1], "-occ-entry-template.xlsx")))
    } else{ 
      data <- rbind(data, read_excel(here::here("data","digitized_data",
                                                paste0("HJ-",i,"-occ-entry-template.xlsx"))))
    }
  }
  
## 3) ADDING/ AUTOMATICALLY FILLING IN SIMPLE DARWIN CORE FIELDS ----

  ## dwc:datasetName
  data$datasetName <- "Harvey Janszen Collections"

  ## dwc:basisOfRecord
  # Human observation = (report by a known observer that an organism was present at the place and time)
  # all records should be this type 
  data$basisOfRecord <- "HumanObservation"

  ## dwc: "year", "month", "day" 
  # splitting YYYYMMDD date entered into separate year, month, day columns
  data <- data %>%
    dplyr::mutate(.data = data, fulldate = lubridate::ymd(date),
                  year = lubridate::year(fulldate), 
                  month = lubridate::month(fulldate), 
                  day = lubridate::day(fulldate))
  
  ## Converting abundance codes into averages of their indicated range on the front cover of 
  # HJ-7 field journal, and then assigning code to the occurrence remarks for reference of the uncertainty range
  # which can be linked to the metadata
  data$numPlantsCode <- as.numeric(data$numPlantsCode)
  
  for (i in 1:dim(data)[1]){ # for all rows
    if(!is.na(data$numPlantsCode[i])){ # if there is a number of plants code
      if (data$numPlantsCode[i] == 0){ # if the num plants code is =0 ...
        data[i,"invididualCount"] <- "1" # assign it to an individual count of 1
          
      }else if (data$numPlantsCode[i] == 1){ 
        data[i,"invididualCount"] <- "1-5"
      
      }else if (data$numPlantsCode[i] == 2){
        data[i,"invididualCount"] <- "5-25"
      
      } else if (data$numPlantsCode[i] == 3){
        data[i,"invididualCount"] <- "25-50"
       
      }else if (data$numPlantsCode[i] == 4){
        data[i,"invididualCount"] <- "50-75"
       
      }else if (data$numPlantsCode[i] == 5){
        data[i,"invididualCount"] <- "75+"
      }
      if(!is.na(data$occRemarks[i])){ # if there are already other occurrence remarks...
        data[i,"occRemarks"] <- paste(data[i, "occRemarks"],";","abundance code", # add this to the end
                                          paste0("'",data[i,"numPlantsCode"],"'"))
      } else { # if there are no occurrence remarks already...
        data[i,"occRemarks"] <- paste0("abundance code:", " '",data[i,"numPlantsCode"],"'") # assing it as the occurrence remark
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
  # automatically adding "genus" if length(sciName) ==1, otherwise "species"
  
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
  
  ## dwc: occurrenceStatus
  # automatically assigning occurrence status as present if it was not filled in
  for(i in 1:dim(data)[1]){ # for each observation
    if(is.na(data$occStatus[i])){ # if there was no remark
      data$occStatus[i] <- "present" # assign as present
    }
  } 
  
  ## dwc: islandGroup
  ## automatically assigning island group, if the island field was filled in 
  places <- read.csv(here::here("data", "reference_data","islands-and-districts_22-11-29.csv"))
  
  for(i in 1:dim(data)[1]){ # for each observation
    if(!is.na(data$island[i])){ # if there was no remark
      data$islandGroup[i] <- places[places$island==data$island[i], "islandGroup"]
    }
  } 
  
  ## dwc: associatedOccurrences 
  # adding "HJC-" to the beginning of colelction numbers entered here to indicate
  # that it is a collected specimen 
  # if string split > 1 " " 
  
  taxa <- NULL #initializing vectors
  taxa_coll <- NULL
  
  for (i in 1:dim(data)[1]){ # for each obs
    if (!is.na(data$assOcc[i])){ # if there is an associated species already listed (collected specimens)
      if (length(str_split(data$assOcc[i], ", ")[[1]])>1){ # if there is more than one ...
        taxa <- str_split(data$assOcc[i], ", ")[[1]]
        
        for (j in 1:length(taxa)){ # for each occurrence number 
          taxa_coll[j] <- paste0("HJC-",taxa[j]) # input "HJC-" before the number to indicate collected specimen number
        }
        # and assign this to the associated species column for that row
        
        data$assOcc[i] <- paste(taxa_coll, collapse= "; ")
        
      } else if (length(str_split(data$assOcc[i], ", "))==1) { # if there is only one associated occurrence already...
        # paste "HJC-" infront and assign to the associated species column for that row
        data$assOcc[i] <- paste0("HJC-",data$assOcc[i])
      }
    }
  }
  
  ## dwc: recordNumber
  # assigning record number as a combined number of the archiveID, pageNumber and number on page
  data$recordNumber <- paste0("HJ",data$archiveID, "-", data$pageNum, "-", data$numPage)
  
  # dwc:fieldNotes
  # stating where the field notes are stored and where the images are 
  data$fieldNotes <- "Imaged notes: X,  Original notes housed at UBC herbarium"
  
  ## dwc: occurrenceId: creating globally unique identifier in chronological order and sequence in journals
  # starting at 1 
  data$occurrenceID <- 1:dim(data)[1]
  
  ## removing fields we don't need anymore for darwin core archive (tidying data)
  occ_data <- data %>% select(-pageNum,-numPage, -vName, -conf, -date)   %>% arrange(occurrenceID)
                                       
## 4) TAXONOMY FIELDS ----
    
  ## Issues to still address: 
    # find way to automatically update taxonomy of verbatim names? 
    # find a way to streamline to avoid having to go into another application
    # find way to assign taxon gbif ID? 
  
  # Using GBIF Species-Lookup tool to check taxon names (updated input names, not verbatim)
    # create and write data frame with updated species names from template 
    Names <- data.frame(occ_data$occurrenceID, occ_data$sciName)
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
      dplyr::rename(taxonRank= rank, occurrenceID = occurrenceId) %>% # renaming columns to darwin core terms
      separate(col = species, # separating species column into genus and species to obtain specific ephiphet
               into = c("GenusRepeat", "species"),
               sep = " ", remove = FALSE) %>% 
      select(-GenusRepeat, -verbatimScientificName, -confidence) %>% # removing duplicated genus name
      dplyr::rename(specificEpiphet = species)  # renaming to match darwin core terms 
      
    # combining gbif taxon match data with occurrence data 
    occ_data <- occ_data %>% select(-sciName) # removing columns to avoid duplication of updated scientific names
    occ_data <- merge(occ_data, normalized_names, by = "occurrenceID")

## 5) GEOREFERENCING ----
    
  ## write a csv file to use in GEOLocate
    
    # creating formatted file for GEOLocate with relevant columns from data
    occ_to_georef <- occ_data %>% select(locality, country, stateProvince, county) %>% 
      dplyr::rename("locality string" = locality, state = stateProvince) %>% # renaming to match GEOlocate column format
      add_column(latitude=NA, longitude = NA, "correction status" = NA, precision=NA, # adding columns for extra information
                 "error polygon" = NA, "multiple results" = NA, uncertainty=NA) %>% 
      add_column(occurrenceID = occ_data$occurrenceID) # attaching occurrence ID to improve matching up later
    
    # writing to data folder
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
    # rename to georef-occ_YYYY-MM-DD.csv
    # place in data folder
    
  ## loading referenced occurrences 
    GEOlocate <- read.csv(here::here("data", "georef-occ_2022-11-14.csv"))
    # renaming columns so that they are unique when we combine them with occ_data 
    GEOlocate <- GEOlocate %>% dplyr::rename(geoLocLat = latitude, geoLocLon = longitude, geoLocPrecision = uncertainty) %>% 
      select(geoLocLat, geoLocLon, geoLocPrecision, occurrenceID)
    
  ## combining with occ_data
    occ_data <- merge(occ_data, GEOlocate, by="occurrenceID") %>% arrange(occurrenceID)

  ## assigning official coordinate estimates (and precision if provided verbatim or by GEOLocate)
    
    # for loop that checks for verbatim coordinates ( in decimal degrees in vLat, 
    # vLon columns or UTM in vUTM column). if UTM coordinates present for given observation, 
    # takes these and converts to decimal degrees based on this projection
    projection <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs" # may need to change - not sure what he was using!
    # if either type of verbatim coordinates are present, assings them to 
    # "decimalLatitude" and "decimalLongitude" darwin core terms
    # if no verbatim coordinates exists, assigns the GEOLocate estimate for 
    # latitude and longitude to these terms instead. 
    # Additinally assigns verbatim coordinate uncertainty from verbatim
    # coordinates if present or assigns GEOLocate estimated uncertainty in METERS
    
    # initializing vectors 
    UTM <- data.frame()
    points <- NULL
    v <- NULL
    z <- NULL
    lonlat <- NULL
    
 for (i in 1:dim(occ_data)[1]){   
  if (!is.na(occ_data$vLat[i]) & !is.na(occ_data$vLon[i])){
    if((length(strsplit(occ_data$vLat[i], " ")[[1]]) > 1)){ # if the coordinates are in degrees, minutes, second 
    
      # convert degrees, minites, seconds to decimal
      # function from: https://stackoverflow.com/questions/30879429/how-can-i-convert-degree-minute-sec-to-decimal-in-r
          angle2dec <- function(angle) {
            angle <- as.character(angle)
            x <- do.call(rbind, strsplit(angle, split=' '))
            x <- apply(x, 1L, function(y) {
              y <- as.numeric(y)
              y[1] + y[2]/60 + y[3]/3600
            })
            return(x)
          }
          
          occ_data$decimalLatidue[i] <- angle2dec(occ_data$vLat[i])
          occ_data$decimalLongitude[i] <- angle2dec(occ_data$vLon[i])
          occ_data$coordinatePrecision[i] <- occ_data$vCoodUncM[i]
          occ_data$verbatimCoordinateSystem[i] <- "degrees minutes seconds" # assigning dwc field - rarely provides lat lon, but so far only dms? 
          occ_data$georeferenceSources[i] <- "Source" # indicates coordinates verbatim 
          
    } else if (length(strsplit(occ_data$vLat[i], " ")[[1]]) ==1){
      # when lat lon coordinates provided...
      occ_data$decimalLatidue[i] <- occ_data$vLat[i]
      occ_data$decimalLongitude[i] <- occ_data$vLon[i]
      occ_data$coordinatePrecision[i] <- occ_data$vCoodUncM[i]
      occ_data$verbatimCoordinateSystem[i] <- "decimal degrees" # assigning dwc field - rarely provides lat lon, but so far only dms? 
      occ_data$georeferenceSources[i] <- "Source" # indicates coordinates verbatim 
    }
    
  } else if (!is.na(occ_data$vUTM[i])){
    
    # when UTM coordinates provided...
    
    # convert UTM coordinates into decimal degrees
    UTM <- occ_data[i,] %>% separate(vUTM, c("zone","x","y"), " ") # seperating UTM into x, and y components
    points <- cbind(as.numeric(UTM$x), as.numeric(UTM$y)) # making dataframe with x y components
    v <- vect(points, crs=projection) # making spatial points data frame
    z <- project(v, projection)  # projecting points using assinged crs  
    lonlat <- as.data.frame(t((geom(z)[, c("x", "y")]))) # extracting lat lon from spatial points frame
    
    # assigning official lat and lon to these coordinates
    occ_data[i, "decimalLatitude"] <- lonlat$y
    occ_data[i, "decimalLongitude"] <- lonlat$x
    occ_data[i, "coordinatePrecision"] <- occ_data[i,"vCoodUncM"]
    occ_data[i, "verbatimCoordinateSystem"] <- "UTM" # assigning dwc field 
    occ_data[i, "georeferenceSources"] <- "Source" # indicates coordinates verbatim 
    
    # removing variables for next time through the loop
    UTM <- NULL
    points <- NULL
    v <- NULL
    z <- NULL
    lonlat <- NULL 
    
  } else if (is.na(occ_data$vUTM[i]) & (is.na(occ_data$vLat[i]) &is.na(occ_data$vLon[i]))) {
    
    # when no verbatim coordinates provided...
    
    occ_data[i, "decimalLatitude"] <- occ_data[i, "geoLocLat"] # assigning GEOLocate lat
    occ_data[i, "decimalLongitude"]<- occ_data[i,"geoLocLon"] # assigning GEOLocate lon
    occ_data[i,"coordinatePrecision"] <- occ_data[i,"geoLocPrecision"] # assigning GEOLocate uncertainty
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
    
    # MAKE SURE SAME UTM / COORDINATES IF PROVIDED TOO 
    
# for loop: for each row, find other rows with matching date AND locality AND habitat, assign the scientificNames names of these rows 
    # to an "associatedTaxa" field, where names are separated by "|" preffered for dwc lists like these
    
    for (i in 1:dim(occ_data)[1]){
    occ_data[i,"assTaxa"]<- occ_data[i, "assTaxa"] + occ_data[(occ_data$eventDate[i] == occ_data$eventDate & 
                                        occ_data$locality[i]==occ_data$locality &  occ_data$habitat[i]==occ_data$habitat),"canonicalName"] %>% 
                                        paste(collapse = "|") %>% 
                            dplyr::rename(associatedTaxa = assTaxa) # renaming field to dwc term
    }
    
# for loop: for each row, find other rows matching date AND locality AND habitat, assing the occurrence ID of these rows to 
    # an "associatedOccurrences" field where names are separated by "|" preffered for dwc lists like these
    for (i in 1:dim(occ_data)[1]){
      occ_data[i,"assOcc"]<- occ_data[i, "assOcc"] + occ_data[(occ_data$eventDate[i] == occ_data$eventDate & 
                                occ_data$locality[i]==occ_data$locality &  occ_data$habitat[i]==occ_data$habitat),"occurrenceID"] %>% 
                                paste(collapse = "|") %>% 
                                dplyr::rename(associatedOccurrences = assOcc)
    }
    
    
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
    CDC <- read.csv(here::here("data","CDC-resultsExport_2022-11-15.csv"))
    
    ## splitting scientific name into genus, species, var/ subspecies, and intraspecific epiphet 
    CDC_cleaned <- CDC %>% 
      separate(col = Scientific.Name, # separating species column into genus and species to obtain specific ephiphet
               into = c("genus", "species", "abb", "intraspecificEpiphet"),
               sep = " ", remove = FALSE) %>% 
     unite("scientificName", genus, species, sep = " ") # uniting genus and species again for ease of searching names
    
    #creating fake intraspecific epiphet to test loop 
    # occ_data$intraspecificEpiphet <- NULL
    
    # initializing vectors for for loop 
    occ_data$provincialListStatus <- NULL
    
      for (k in 1:dim(occ_data)[1]){ # for each occurrence observation
        
        if (!is.null(occ_data[k, "intraspecificEpiphet"])){ # for observations that have species with intraspecific epiphet (subspecies)
          # does the genus, species and intraspeicific epithet match one in the CDC? 
          occ_data[k, "provincialListStatus"]<- CDC_cleaned[(CDC_cleaned$scientificName == occ_data$canonicalName[k] & CDC_cleaned$intraspecificEpiphet == occ_data$intraspecificEpiphet[k]), "BC.List"]
        
        } else { # if there is no intraspecific epiphet listed  
            if(length(CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "BC.List"]) > 1){  # if there is match for genus and species name in CDC
            
                  for (j in 1:length(CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "BC.List"])){ # for how ever many number of subspecies there are 
                  
                    if (length(unique(CDC_cleaned[(CDC_cleaned$scientificName == occ_data$canonicalName[k]),"BC.List"])) == 1){ # do all of possible subspecies have same list code? 
                    # if so, then apply that list category to that row
                    occ_data[k, "provincialListStatus"] <- CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "BC.List"][1]
                
                   } else { # if multiple subspecies with different list categories and we don't know what subspecies
                    occ_data[k, "provincialListStatus"]  <- NA 
                  } 
                }
            } else if (length(CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "BC.List"]) == 1){ # if only one intraspecific variety
            occ_data[k, "provincialListStatus"] <- CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "BC.List"]
            
          }
      }
    }

  ## repeating loop to assing CDC status (S5 to S1)
    # initializing vectors for for loop 
    occ_data$provincialStatus <- NULL
    
    for (k in 1:dim(occ_data)[1]){ # for each occurrence observation
      
      if (!is.null(occ_data[k, "intraspecificEpiphet"])){ # for observations that have species with intraspecific epiphet (subspecies)
        # does the genus, species and intraspeicific epithet match one in the CDC? 
        occ_data[k, "provincialStatus"]<- CDC_cleaned[(CDC_cleaned$scientificName == occ_data$canonicalName[k] & CDC_cleaned$intraspecificEpiphet == occ_data$intraspecificEpiphet[k]), "Provincial"]

      } else { # if there is no intraspecific epiphet listed  
        if(length(CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "Provincial"]) > 1){  # if there is match for genus and species name in CDC
          
          for (j in 1:length(CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "Provincial"])){ # for how ever many number of subspecies there are 
            
            if (length(unique(CDC_cleaned[(CDC_cleaned$scientificName == occ_data$canonicalName[k]),"Provincial"])) == 1){ # do all of possible subspecies have same list code? 
              # if so, then apply that list category to that row
              occ_data[k, "provincialStatus"] <- CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "Provincial"][1]
              
            } else { # if multiple subspecies with different list categories and we don't know what subspecies
              occ_data[k, "provincialStatus"]  <- NA 
            } 
          }
        } else if (length(CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "Provincial"]) == 1){ # if only one intraspecific variety
          occ_data[k, "provincialStatus"] <- CDC_cleaned[CDC_cleaned$scientificName == occ_data$canonicalName[k], "Provincial"]
          
        }
      }
    }
    
## COSEWIC  - use from same table kept by BC CDC? 
    
## IUCN or natureserve from BC CDC global 
    install.packages("rredlist")
    library(rredlist)
    rl_use_iucn()
    
    
## 8) remove extraneous rows and export to upload to Canadensys IPT ----
    select(-dataEntryRemarks)
    
## Phenology
    # if occurrenceRemarks(contains "flowering") lifeStage <- "flowering"
    
## renaming miscellaneous columns to darwin core terms ----
    template <- template %>% rename(verbatimTaxonRank = vTaxonRank,
                                    occurrenceStatus = occStatus, verbatimScientificName = vSciName,
                                    scientificName = sciName, 
                                    verbatimElevation= vElevM,
                                    occurrenceRemarks=occRemarks, identificationBy = idBy, 
                                    eventDate = fulldate)
    
## Saving as .csv file ----