#######################################################################
###       Processing Step 3: conversion from Data Entry Template    ###
###                      to Darwin Core Occurrences                 ###
###                         Emma Menchions                          ###
###                         Created Nov. 7/22                       ###
#######################################################################

## OVERVIEW ----
# this script will take occurrence data and place it in approximate
# darwin core format
# see: https://dwc.tdwg.org/list/#dwc_fieldNotes
  
## LOADING & INSTALLING PACKAGES ----
 
  library(groundhog)
  
  set.groundhog.folder(here::here("packages"))
  date <- "2022-11-02"
  requiredPackages <-  c("assertr","dplyr","here", "lubridate",
                         "magrittr","mapview","purrr","ritis","sp",
                         "stringi","taxize","terra","tidyverse","tidyr","cowsay")
  
  for (pkg in requiredPackages) {
    groundhog.library(pkg, date)
  }
  
  ## removing all objects from workspace to avoid errors by carrying 
  # over similar variable names
  rm(list = ls())
  
## 1) USER INPUT ----
  
  J <- c(5,7,8,9,27) # JOURNAL NUMBERS 
  AI <- "HJ" # AUTHOR INITIALS 
  
  fieldnote_storage_facility <- " " # where are field notes stored? 
  image_repo <- " " # repository where field note images are stored
  
  tax_ref_sys <- "GBIF" # taxonomy reference system --> either "FPNW2" or "GBIF"
  
## 2) READING IN DATA ----
  
  # loading most recent cleaned data file  
      
    data <- read.csv(
          here::here("data","data_digitization",
          "occurrence_data","4_clean_data", 
          # reading in most recent round of cleaned occurrences
          unique(as.character(max(list.files(  
          here::here("data","data_digitization",
                     "occurrence_data", 
                      "4_clean_data"
                      )))))))
  
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
        data[i,"orgQuantity"] <- "1-5 plants"
        data[i,"orgQtype"] <- "individuals"
      
      }else if (data$numPlantsCode[i] == 2){
        data[i,"orgQuantity"] <- "5-25 plants"
        data[i,"orgQtype"] <- "individuals"
      
      } else if (data$numPlantsCode[i] == 3){
        data[i,"orgQuantity"] <- "25-50 plants"
        data[i,"orgQtype"] <- "individuals"
        
      }else if (data$numPlantsCode[i] == 4){
        data[i,"orgQuantity"] <- "50-75 plants"
        data[i,"orgQtype"] <- "individuals"
        
      }else if (data$numPlantsCode[i] == 5){
        data[i,"orgQuantity"] <- "75+ plants"
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
      # if other collector names noted 
    }else if (!is.na(data$recordedBy[i]) & data$recordedBy[i] != "Harvey Janszen" ){ 
      # add his name as first collector
      data$recordedBy[i] <- paste0("Harvey Jaszen, ", data$recordedBy[i]) 
    }
  } 
  
  ## dwc: verbatimTaxonRank 
  # automatically adding "genus" if length(sciName) == 1, otherwise "species"
  
  for(i in 1:dim(data)[1]){  # for each row
    if(is.na(data$vTaxonRank[i])){ # if there is no taxon rank already...
      # if the length of the sciName string ==1,,,
      if (length(strsplit(data$sciName[i], " ")[[1]])==1){ 
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
  data$islandGroup <- c()
  for(i in 1:dim(data)[1]){ # for each observation
    if(!is.na(data$island[i])){ # if there was no remark
      data$islandGroup[i] <- places[places$island==data$island[i], 
                                    "islandGroup"]
    }
  } 
  
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
  
    # checking that all were assigned a county
    data %>% 
      group_by(county) %>% 
      assert(not_na,county) 
  
## assColl (associated collections)
  # adding collector initials to the beginning of collection numbers entered here to indicate
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
          # input initials before the number to indicate collected specimen number
          taxa_coll[j] <- paste0(AI,"C-",taxa[j]) 
        }
        # and assign this to the associated species column for that row
        
        data$assColl[i] <- paste(taxa_coll, collapse= "; ")
        
      } else if (length(str_split(data$assColl[i], ", "))==1) { # if there is only one 
                                                    # associated occurrence already...
        # paste initials infront and assign to the associated species column for that row
        data$assColl[i] <- paste0(AI,"C-",data$assColl[i])
      }
    }
  }
  
  ## assCollTaxa
  # replacing commas with semicolons for consistency
  data$assCollTaxa <- str_replace_all(data$assCollTaxa, ",", ";")
  
  ## dwc: recordNumber
  # assigning record number as a combined number of the archiveID, 
  # pageNumber and number on page
  data$recordNumber <- paste0(AI,data$archiveID, "-", 
                              data$pageNum, "-", data$numPage)
  
  # dwc:fieldNotes
  # stating where the field notes are stored and where the images are 
  data$fieldNotes <- paste0(AI,"-", data$archiveID, " page: ", data$pageNum, " num: ", data$numPage, ",
                            Imaged notes: ",image_repo, ", Original notes housed at: ", fieldnote_storage_facility)
  
  
  ## dwc: occurrenceId: creating globally unique identifier in 
  # chronological order and sequence in journals
  # starting at 1 
  data$occurrenceID <- paste0(AI,"O-",1:dim(data)[1])
  
  ## removing fields we don't need anymore for darwin core archive
  # (tidying data)
  occ_data <- data %>% 
    select(-pageNum,-numPage, -vName, -conf, -date, -index) %>%
    arrange(occurrenceID)
  
  ## based on the recordNumbers generated above, removing rows already processed
  # to avoid extra work in the following steps
  
  if(length(list.files(here::here("data", "data_digitization","occurrence_data",
                                  "darwin_core_data")))>=1){
    
    # read in the file with the latest date (with most observations) 
    old_dwc_data <- read.csv(here::here("data", "data_digitization","occurrence_data",
                                        "darwin_core_data",
                                        unique(as.character(max(list.files(here::here(
                                          "data", "data_digitization","occurrence_data",
                                          "darwin_core_data"))))))) %>% select(recordNumber)
    
    # remove rows from current data table that contain previously dwc processed data                              
    occ_data <- anti_join(occ_data, old_dwc_data)
    
  }
  
  
## 4) TAXONOMY FIELDS ----
    
## formatting and writing file of names to check: 
    
  if(tax_ref_sys == "FPNW2"){
  # Using PNW herbaria batch taxonomy name 
  # checker: https://www.pnwherbaria.org/florapnw/namechecker.php
    
    # selecting columns specified by the name checker, renaming
    Names <- occ_data %>% 
      dplyr::select(sciName, country, stateProvince) %>% 
      group_by(sciName) %>% 
      sample_n(1) %>% 
      filter(!is.na(sciName)) %>% 
      dplyr::rename(ScientificName = sciName,
                    Country = country, 
                    StateProvince = stateProvince) 
    
    # writing as tab-delimted .txt
    write.table(Names, here::here("data","data_digitization","occurrence_data", 
                                "occ_reference_data", "taxonomy", "raw",
                                paste0("taxa-names_",
                                       Sys.Date(), ".txt")),
              row.names = F, sep =   '\t', col.names = T,quote=FALSE)
    
    cowsay::say("file ready to check with FPNW2 standards", by="signbunny")
    
  } else if (tax_ref_sys == "GBIF"){
    
  # Using GBIF Species-Lookup tool to check taxon names (updated 
  # input names, not verbatim)
    
    # selecting columns specified by the name checker, renaming
    Names <- occ_data %>% 
    dplyr::select(occurrenceID, sciName) %>% 
    group_by(sciName) %>% 
    sample_n(1) %>% 
    filter(!is.na(sciName)) %>% 
    dplyr::rename(scientificName = sciName)
  
    # writing as csv file
    write.csv(Names, here::here("data","data_digitization","occurrence_data", 
                                "occ_reference_data", "taxonomy", "raw",
                                paste0("taxa-names_",
                                Sys.Date(), ".csv")), row.names = F)
    
    cowsay::say("file ready to check with GBIF standards", by="signbunny")
      
  }
  
## removing old files to save storage
  if(length(list.files(here::here("data","data_digitization","occurrence_data", 
                                      "occ_reference_data", "taxonomy","raw")))>2){
        file.remove(here::here("data","data_digitization","occurrence_data", 
                               "occ_reference_data", "taxonomy","raw",
                               unique(as.character(min(list.files(
                                 here::here("data","data_digitization","occurrence_data", 
                                            "occ_reference_data", "taxonomy","raw"))))))) 
    cowsay::say("old files removed", by="signbunny")
    
  }
    
## Visit...
  if(tax_ref_sys=="FPNW2"){
    
    utils::browseURL("https://www.pnwherbaria.org/florapnw/namechecker.php", browser = getOption("browser"))
    # Upload .txt file to: https://www.pnwherbaria.org/florapnw/namechecker.php
    utils::browseURL(here::here("data","data_digitization","occurrence_data", 
                                "occ_reference_data", "taxonomy", "raw"))
    
    # select "choose file" button
    # find file in folder data > data_digitization > occurrence_data > occ_reference_data > taxonomy > raw
    # press the "Check Names" button
    # download the file produced
    # place in...
    utils::browseURL(here::here("data","data_digitization","occurrence_data", 
                                "occ_reference_data", "taxonomy", "normalized"))
    
    # manually change the file name to "FPNW2_normalized_YYYY-MM-DD.txt"
    
  }else if(tax_ref_sys == "GBIF"){
    
    # find csv file in folder: 
    utils::browseURL(here::here("data","data_digitization","occurrence_data", 
                                "occ_reference_data", "taxonomy", "raw"))
    utils::browseURL("https://www.gbif.org/tools/species-lookup", browser = getOption("browser"))
    # Upload csv file to https://www.gbif.org/tools/species-lookup
    # select "match to backbone" button
    # Review the outputs - paying attention to the "matchType" column
    # if yellow or red, click edit field in the scienfificName column
    # select the desired accepted taxon - often the first one, 
    # often with authority L. 
    # repeat for all other entries with yellow or red match types
    # once all are exact or "edited" click "generate.csv" in the 
    # bottom corner
    # place in working directory:
    utils::browseURL(here::here("data","data_digitization","occurrence_data", 
                                "occ_reference_data", "taxonomy", "normalized"))
    
    # add the date that this table was generated: normalized_YYYY_MM_DD.csv
  }
    
## Reading in table of checked names, matching names and writing updated taxonomic info
  # to main data frame: 
  
  if(tax_ref_sys=="FPNW2"){
      
      normalized_names <- read.csv(
        here::here("data","data_digitization","occurrence_data",
                   "occ_reference_data",
                   "taxonomy","normalized",unique(as.character(max(list.files(
                     here::here("data","data_digitization","occurrence_data",
                                "occ_reference_data",
                                "taxonomy","normalized")))))),header=T, sep="\t", na.strings = "")
       
    ## matching and assigning names to main data frame:
    
    # initializing vectors:
    normalized_names$genus <- NA
    normalized_names$specificEpithet <- NA
    normalized_names$taxonRank <- NA
    normalized_names$intraspecificEpithet <- NA
    normalized_names$scientificNameAuthorship <- NA
    normalized_names$species <- NA
    normalized_names$scientificName <- NA
  
    # loop:
    for(i in 1:dim(normalized_names)[1]){  # for each row
      
      # assigning genus, specific epithet, intraspecific epithet from AcceptedScientificName
        # genus
        if (!is.na(strsplit(normalized_names$AcceptedScientificName[i], " ")[[1]][1])){
          normalized_names$genus[i] <- 
          strsplit(normalized_names$AcceptedScientificName[i], " ")[[1]][1]
          #a ssigning taxonomic rank as genus
          normalized_names$taxonRank[i] <- "GENUS"
          }
        
        # specific epithet
        if (!is.na(strsplit(normalized_names$AcceptedScientificName[i], " ")[[1]][2])){
          normalized_names$specificEpithet[i] <-
            strsplit(normalized_names$AcceptedScientificName[i], " ")[[1]][2]
          # assigning taxonomic rank as genus
          normalized_names$taxonRank[i] <- "SPECIES"
        }
            
          
        # intraspecific epithet
        if (!is.na(strsplit(normalized_names$AcceptedScientificName[i], " ")[[1]][4])){
            normalized_names$intraspecificEpithet[i] <- 
              strsplit(normalized_names$AcceptedScientificName[i], " ")[[1]][4]
            
            # if the intraspecific signifier is ssp. ...
            if (strsplit(normalized_names$AcceptedScientificName[i], " ")[[1]][3] == "ssp." |
                strsplit(normalized_names$AcceptedScientificName[i], " ")[[1]][3] == "subsp."){
                normalized_names$taxonRank[i] <- "SUBSPECIES"
            }else if (strsplit(normalized_names$AcceptedScientificName[i], " ")[[1]][3] == "var." ){
              normalized_names$taxonRank[i] <- "VARIETY"
              
            }else if (strsplit(normalized_names$AcceptedScientificName[i], " ")[[1]][3] == "form." |
                      strsplit(normalized_names$AcceptedScientificName[i], " ")[[1]] == "forma"){
              normalized_names$taxonRank[i] <- "FORMA"
              
            }
        }
      
        # assigning species author 
        # if intraspecific species author present, useses that over species author 
        if (!is.na(normalized_names$InfraspeciesAuthors[i])){
          normalized_names$scientificNameAuthorship[i] <- normalized_names$InfraspeciesAuthors[i]
        }else if (!is.na(normalized_names$SpeciesAuthors[i])){
          normalized_names$scientificNameAuthorship[i] <- normalized_names$SpeciesAuthors[i]
        }
      # assigning species (genus and specific epithet)
      if (!is.na(normalized_names$genus[i]) & ! is.na(normalized_names$specificEpithet[i])){
      normalized_names$species[i] <- paste(normalized_names$genus[i], normalized_names$specificEpithet[i])
      }
      
      # assigning scientificName
      if (!is.na(normalized_names$species[i]) & !is.na(normalized_names$scientificNameAuthorship[i])){
        normalized_names$scientificName[i] <- paste(normalized_names$species[i], normalized_names$scientificNameAuthorship[i])
      }else { # if no specific epithet, just assign genus
        normalized_names$scientificName[i] <- normalized_names$genus[i]
      }
      
    }  
        
      # renaming columns
      normalized_names <- normalized_names %>% 
      dplyr::rename(family = AcceptedFamily, 
                    sciName =ScientificName )
      
      # pasting and assigning a few other variables
      normalized_names$kingdom <- "Plantae"
      
    ## assigning names and fields in normalized names to occ_data
    occ_data <- left_join(occ_data, select(.data=normalized_names,
                                                  # removing these columns from join
                                                 c(-StateProvince, -Country, -FPNW2ndEdition, 
                                                   -FPNW1stEdition, -AcceptedScientificName, 
                                                   -SpeciesAuthors,
                                                   -InfraspeciesAuthors, -MisappliedTo.SeeAlso, 
                                                   -Comments)), by="sciName") %>% 
      select(-sciName) %>% # removing this, no longer needed 
      relocate(scientificName, .before=vSciName)
      
     
  } else if (tax_ref_sys=="GBIF"){
    
  # loading in taxonomy referenced file
    normalized_names <- read.csv(
      here::here("data","data_digitization","occurrence_data",
      "occ_reference_data",
      "taxonomy","normalized",unique(as.character(max(list.files(
      here::here("data","data_digitization","occurrence_data",
      "occ_reference_data",
      "taxonomy","normalized")))))))
      
    ## matching and assigning names to main data frame:

      # combining gbif taxon match data with occurrence data 
      cols <- names(normalized_names)[3:length(names(normalized_names))]
      
      # initializing empty columns to fill 
      for (i in 1:length(cols)){
        occ_data[,cols[i]] <-NA
      }
      
      # assigning matching names 
      for (i in 1:dim(occ_data)[1]){
        for (k in cols){
          occ_data[i, k] <- normalized_names[which(occ_data$sciName[i] ==
                                                     normalized_names$verbatimScientificName), k]
        }
        
      }
      
      # turning canonical Name into genus, specificEpithet and intraspecific epithet
      speciesSep <- as.data.frame(str_split_fixed(occ_data$canonicalName, " ", 3)) %>% 
        dplyr::rename(genus = V1, specificEpithet = V2, intraspecificEpithet = V3) %>% 
        select(-genus) # removing duplicate row of genus
      
        occ_data$specificEpithet <- speciesSep[,1]
        occ_data$intraspecificEpithet <- speciesSep[,2]
        occ_data <- occ_data %>% 
        # making sure that empty cells are NA
        mutate_all(na_if, "") %>% 
        # removing columns to avoid duplication 
        select(-sciName) %>% 
        # renaming columns
        dplyr::rename(scientificNameauthorship = authorship, 
        taxonRank = rank, 
        taxonomicStatus = status)
      
}

    
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

      write.csv(localities_to_georef, here::here("data","data_digitization","occurrence_data", 
                                  "occ_reference_data", "georeferencing", "raw",
                                  paste0("localities-to-georef_", 
                                         Sys.Date(), ".csv")), 
                row.names = F)
      # and removing old files to save storage
      if(length(list.files(here::here("data","data_digitization","occurrence_data", 
                                      "occ_reference_data", "georeferencing", "raw")))>2){
        file.remove(here::here("data","data_digitization","occurrence_data", 
                               "occ_reference_data", "georeferencing", "raw",
                               unique(as.character(min(list.files(
                                 here::here("data","data_digitization","occurrence_data", 
                                  "occ_reference_data", "georeferencing","raw")))))))   
      }

  ## visit GEOLocate batch processor: 
      utils::browseURL("https://www.geo-locate.org/web/WebFileGeoref.aspx", browser = getOption("browser"))
      # upload this file: 
      utils::browseURL(here::here("data","data_digitization","occurrence_data","occ_reference_data","georeferencing","raw"))
      # Follow protocol outlined in "post-entry-processing.Rmd"
      # place the completed sheet in this folder: 
      utils::browseURL(here::here("data","data_digitization","occurrence_data","occ_reference_data","georeferencing","done"))
      
      
  ## loading referenced occurrences 
   
  
      GEOlocate <- read.csv(
        here::here("data","data_digitization","occurrence_data",
                   "occ_reference_data",
                   "georeferencing","done",unique(as.character(max(list.files(
                     here::here("data","data_digitization","occurrence_data",
                                "occ_reference_data",
                                "georeferencing","done")))))))
  
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
    occ_data$vLat <- as.character(occ_data$vLat)
    occ_data$vLon <- as.character(occ_data$vLon)
    
 for (i in 1:dim(occ_data)[1]){  # for each row 
  if (!is.na(occ_data$vLat[i]) & !is.na(occ_data$vLon[i]) & is.na(occ_data$vUTM)[i] | 
      !is.na(occ_data$vLat[i]) & !is.na(occ_data$vLon[i]) & !is.na(occ_data$vUTM)[i]){ 
    # if there are verbatim lat and lon coordinates...
    # of if there are verbatim lat, lon and UTM coordinates... (use lat lon)
    
    if(length(strsplit(occ_data$vLat[i], " ")[[1]]) > 1){ 
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
      
      occ_data$decimalLatitude[i] <- as.numeric(occ_data$vLat[i])
      occ_data$decimalLongitude[i] <- as.numeric(occ_data$vLon[i])
      occ_data$coordinateUncertaintyInMeters[i] <- occ_data$vCoordUncM[i]
      occ_data$verbatimCoordinateSystem[i] <- "decimal degrees"
      occ_data$georeferenceSources[i] <- "Source" # indicates coordinates verbatim 
    }
    
  } else if (!is.na(occ_data$vUTM[i]) & is.na(occ_data$vLat[i]) & 
             is.na(occ_data$vLon[i])){
    # if only UTM coordinates provided...
    
    ## convert UTM coordinates into decimal degrees
    
      # seperating UTM into x, and y components
      UTM <- occ_data[i,] %>% separate(vUTM, c("zone","x","y"), " ") 
      # making dataframe with x y components
      points <- cbind(as.numeric(UTM$x), as.numeric(UTM$y)) 
      # making spatial points data frame
      v <- vect(points, crs="+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs") 
      # projecting points using assinged crs  
      z <- project(v,"+proj=longlat +datum=WGS84")  
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
      
      occ_data[i, c("decimalLatitude", "decimalLongitude", 
                    "coordinateUncertaintyInMeters")] <-  
        GEOlocate[which(occ_data$locality[i] == GEOlocate$locality &
                        occ_data$county[i] == GEOlocate$county),
                         c("geoLocLat", "geoLocLon", "geoLocPrecision")] 
      occ_data[i,"georeferenceProtocol"] <- "GEOLocate batch process"
      occ_data$georeferenceSources[i] <- "GEOLocate"
    
  } 
 }
    
    # making sure all Longitude is negative and entered properly
    for(i in 1:dim(occ_data)[1]){
      if(occ_data$decimalLongitude[i]>1){
        occ_data$decimalLongitude[i] <- occ_data$decimalLongitude[i]*-1
      }
    }
  
  points_to_map <- SpatialPoints(cbind((occ_data$decimalLongitude),
                                     occ_data$decimalLatitude), 
                               proj4string = CRS("+proj=longlat +datum=WGS84"))
  mapview(points_to_map)          
              
## 6) ASSIGNING ASSOCIATE ROWS and TAXA----
  
# temporarily make missing values in habitat and location remarks columns 
# "missing"
    occ_data[is.na(occ_data$habitat), "habitat"] <- "missing" 
    occ_data[is.na(occ_data$locationRemarks), "locationRemarks"] <- "missing" 
    
# for loop: finds matching occurrences and places their names in associated 
    # taxa column
    
for (i in 1:dim(occ_data)[1]){ # for every row
  
  # if there is another row that matches date, and place...
  if (length(occ_data$occurrenceID[
    which(occ_data$fulldate[i] == occ_data$fulldate & 
          occ_data$locality[i]==occ_data$locality &  
          occ_data$habitat[i]==occ_data$habitat &
          occ_data$locationRemarks[i]==occ_data$locationRemarks &
          occ_data$decimalLatitude[i]==occ_data$decimalLatitude &
          occ_data$decimalLongitude[i]==occ_data$decimalLongitude | 
          occ_data$vLat[i]==occ_data$vLat & occ_data$vLon[i] ==occ_data$vLon |
          occ_data$vUTM[i] == occ_data$vUTM &
          # and they have habitat and locality information
          !is.na(occ_data$habitat[i]) &
          !is.na(occ_data$locality[i])
          )]) > 1){
    
    nameVec <- 
      as.vector(occ_data[occ_data$fulldate[i] == occ_data$fulldate & 
      occ_data$locality[i]==occ_data$locality &  
      occ_data$habitat[i]==occ_data$habitat &
      occ_data$locationRemarks[i]==occ_data$locationRemarks &
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
              
              occ_data[i,"assCollTaxa"]<- paste(occ_data[i, "assCollTaxa"],
                                                nameVec[j],sep = "; ")
              
            }
          }
          
        # or if there is already a collected associated taxa listed...
        } else if (!is.na(occ_data$assCollTaxa[i])){   
          # for every match...
          for (j in 1:length(nameVec)){
              
              occ_data[i,"assCollTaxa"] <- paste(occ_data[i,"assCollTaxa"],
                                                 nameVec[j], sep = "; ")
                            
          }
       }
  }
}
    
# for loop: repeating the loop above but for record and collection numbers
  
for (i in 1:dim(occ_data)[1]){ # for every row
      
      # if there is another row that matches date, and place...
      if (length(occ_data$occurrenceID[
        which(occ_data$fulldate[i] == occ_data$fulldate & 
              occ_data$locality[i]==occ_data$locality &  
              occ_data$habitat[i]==occ_data$habitat &
              occ_data$locationRemarks[i]==occ_data$locationRemarks &
              occ_data$decimalLatitude[i]==occ_data$decimalLatitude &
              occ_data$decimalLongitude[i]==occ_data$decimalLongitude | 
              occ_data$vLat[i]==occ_data$vLat & occ_data$vLon[i] ==occ_data$vLon |
              occ_data$vUTM[i] == occ_data$vUTM&
              # and they have habitat and locality information
              !is.na(occ_data$habitat[i]) &
              !is.na(occ_data$locality[i])
              )]) > 1){
        
        numVec <- 
          as.vector(occ_data[occ_data$fulldate[i] == occ_data$fulldate & 
          occ_data$locality[i]==occ_data$locality &  
          occ_data$habitat[i]==occ_data$habitat &
          occ_data$locationRemarks[i]==occ_data$locationRemarks &
          occ_data$decimalLatitude[i]==occ_data$decimalLatitude &
          occ_data$decimalLongitude[i]==occ_data$decimalLongitude
                                      ,"occurrenceID"])
        
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
              
              occ_data[i,"assColl"]<- paste(occ_data[i, "assColl"],
                                            numVec[j],sep = "; ")
              
            }
          }
          
          # or if there is already a collected associated record num listed...
        } else if (!is.na(occ_data$assColl[i])){   
          # for every match...
          for (j in 1:length(numVec)){
            
            occ_data[i,"assColl"] <- paste(occ_data[i,"assColl"],
                                           numVec[j], sep = "; ")
            
          }
        }
      }
    } 
    
    
# converting "missing" habitat data back into NA
  occ_data[occ_data$habitat=="missing", "habitat"] <- NA
  occ_data[occ_data$locationRemarks=="missing", "locationRemarks"] <- NA
  
## 7) CONSERVATION STATUS ----
    
  ## reading downloaded BC Conservation Data centre ranks (BC CDC): 
  # https://a100.gov.bc.ca/pub/eswp/
    CDC <- read.csv(here::here("data","reference_data",
                               "CDC-resultsExport_2022-11-15.csv"))
    
  ## splitting scientific name into genus, species, var/ subspecies, 
  # and intraspecific epithet 
  
    CDC_cleaned <- CDC %>% 
      # separating species column into genus and species to obtain specific 
      # ephithet
      separate(col = Scientific.Name, 
               into = c("genus", "species", "abb", "intraspecificEpithet"),
               sep = " ", remove = FALSE) %>% 
     unite("fullName", genus, species, intraspecificEpithet,sep = " ", 
           na.rm=T, remove=F) %>% 
    # uniting genus and species again for ease of searching names
     unite("scientificName", genus, species, sep=" ", na.rm=T) 
    
  ## assigning CDC status (S5 to S1)
    
    # initializing vectors for for loop 
    occ_data$provincialStatus <- NA
    
    for (k in 1:dim(occ_data)[1]){ # for each occurrence observation
      
      # for observations that have species with intraspecific epithet (subspecies)...
      if (!is.na(occ_data$intraspecificEpithet[k])){ 
        
        # does the genus, species and intraspeicific epithet match one in the CDC?... 
        if(length(CDC_cleaned[which(occ_data$canonicalName[k]==CDC_cleaned$fullName), 
                              "Provincial"])==1){
          
          occ_data$provincialStatus[k]<- paste0("provincial status: ", CDC_cleaned[
          which(occ_data$canonicalName[k]==CDC_cleaned$fullName), "Provincial"])
        }
        
      } else { # if there is no intraspecific epithet listed... 
        
        # if there is match for genus and species name in CDC...
        if(length(CDC_cleaned[which(CDC_cleaned$scientificName == 
                              occ_data$canonicalName[k]), "Provincial"]) > 1){ 
          
          # for how ever many number of subspecies there are ...
          for (j in 1:length(CDC_cleaned[which(CDC_cleaned$scientificName == 
                                         occ_data$canonicalName[k]), 
                                         "Provincial"])){ 
            
            # do all of possible subspecies have same list code? 
            if (length(unique(CDC_cleaned[which(CDC_cleaned$scientificName == 
                                           occ_data$canonicalName[k]),
                                          "Provincial"])) == 1){ 
              
              # if so, then apply that list category to that row
              occ_data[k, "provincialStatus"] <- paste0("provincial status: ",
                CDC_cleaned[which(CDC_cleaned$scientificName == 
                              occ_data$canonicalName[k]), "Provincial"][1])
              
            } else { # if multiple subspecies with different list categories 
              # and we don't know what subspecies...
              
              occ_data[k, "provincialStatus"]  <-paste0("provincial status:", NA)
              
            } 
          }
          
        # if only one name that matches
        } else if (length(CDC_cleaned[which(CDC_cleaned$fullName == 
                                      occ_data$canonicalName[k]), 
                                      "Provincial"]) == 1){ 
          
          occ_data[k, "provincialStatus"] <- paste0("provincial status: ",
            CDC_cleaned[which(CDC_cleaned$fullName == 
                          occ_data$canonicalName[k]), "Provincial"])
          
        }
      }
    }
    
  ## assigning global status (G5 to G1)
    # initializing vectors for for loop 
    occ_data$globalStatus <- NA
    
    for (k in 1:dim(occ_data)[1]){ # for each occurrence observation
      
      # for observations that have species with intraspecific epithet (subspecies)...
      if (!is.na(occ_data$intraspecificEpithet[k])){ 
        # does the genus, species and intraspeicific epithet match one in the CDC? 
        if(length(CDC_cleaned[which(occ_data$canonicalName[k]==CDC_cleaned$fullName), 
                              "Global"])==1){
          
          occ_data$globalStatus[k]<- paste0("global status: ",
            CDC_cleaned[which(occ_data$canonicalName[k]==
                          CDC_cleaned$fullName), "Global"])
          
        }
        
      } else { # if there is no intraspecific epithet listed... 
        
        # if there is match for genus and species name in CDC...
        if(length(CDC_cleaned[which(CDC_cleaned$scientificName == 
                              occ_data$canonicalName[k]), "Global"]) > 1){ 
          
          # for how ever many number of subspecies there are ...
          for (j in 1:length(CDC_cleaned[which(CDC_cleaned$scientificName == 
                                         occ_data$canonicalName[k]), "Global"])){ 
            
            # do all of possible subspecies have same list code? 
            if (length(unique(
              CDC_cleaned[which(CDC_cleaned$scientificName == 
              occ_data$canonicalName[k]),"Global"])) == 1){ 
              
              # if so, then apply that list category to that row
              occ_data[k, "globalStatus"] <- paste0("global status: ",
                CDC_cleaned[which(CDC_cleaned$scientificName ==
                              occ_data$canonicalName[k]), "Global"][1])
              
            } else { # if multiple subspecies with different 
              #list categories and we don't know what subspecies...
              
              occ_data[k, "globalStatus"]  <- paste0("global status: ", NA) 
              
            } 
          }
          
          # if only one name that matches
        } else if (length(CDC_cleaned[which(CDC_cleaned$fullName ==
                                      occ_data$canonicalName[k]), "Global"]) == 1){ 
          
          occ_data[k, "globalStatus"] <- paste0("global status: ",
            CDC_cleaned[which(CDC_cleaned$fullName == 
                          occ_data$canonicalName[k]), "Global"])
          
        }
      }
    }
    
    
## 8) FINAL PROCESSING STEPS  ----
## remove extraneous columns  
    
    new_dwc_data <- occ_data %>% 
      # assigning statuses as dwc::dyamic properties
      unite("dynamicProperties", provincialStatus, 
            globalStatus, sep="; ", na.rm=T) %>% 
    
      # renaming columns to dwc terms 
      dplyr::rename(verbatimScientificName = vSciName, 
                    verbatimElevation = vElevM,
                    identificationQualifier = idQualifier,
                    occurrenceStatus = occStatus, 
                    associatedTaxa = assCollTaxa, 
                    associatedOccurrences = assColl, 
                    verbatimTaxonRank=vTaxonRank, 
                    verbatimLatitude = vLat, 
                    verbatimLongitude = vLon, 
                    verbatimCoordinates = vUTM, 
                    organismQuantity = orgQuantity, 
                    organismQuantityType = orgQtype, 
                    occurrenceRemarks = occRemarks, 
                    lifeStage = phenology, 
                    identificationBy= idBy,
                    eventDate = fulldate) %>% 

      # removing columns
      select(-archiveID, -dataEntryRemarks, -canonicalName, -confidence,
             -numPlantsCode)
    
## appending previously processed data 
    
  # if there are one or more files that have been previously processed...

    if(length(list.files(here::here("data", "data_digitization","occurrence_data",
                                    "darwin_core_data")))>=1){
      
    # read in the file with the latest date (with most observations) 
      old_dwc_data <- read.csv(here::here("data", "data_digitization","occurrence_data",
                      "darwin_core_data",
                       unique(as.character(max(list.files(here::here(
                      "data", "data_digitization","occurrence_data",
                        "darwin_core_data")))))))
      
    # append rows from current data table that contain old data and sort by date                                        
      new_total_dwc_data <- rbind(new_dwc_data, old_dwc_data) %>% arrange(eventDate)
      
    # renumbering occurrence ID
      new_total_dwc_data$occurrenceID <- paste0(AI,"O-",1:dim(new_total_dwc_data)[1])
    }else{
      new_total_dwc_data <-  new_dwc_data
  }
    
## saving csv files
  
    # write it to folder "darwin_core_data > all"
   write.csv(new_total_dwc_data, here::here("data", "data_digitization",
                                  "occurrence_data","darwin_core_data",
                                  paste0("darwin-core-occurrences_", 
                                         Sys.Date(), ".csv")), row.names = F)
    # and removing old files to save storage
    if(length(list.files(here::here("data", 
                                    "data_digitization","occurrence_data",
                                    "darwin_core_data")))>2){
      file.remove(unique(as.character(min(list.files(here::here("data", 
                                    "data_digitization","occurrence_data",
                                      "darwin_core_data"))))))   
      cowsay::say("old files removed", by="signbunny")
    }
    

    
