#######################################################################
###   Collection & Observation-Only Occurrence Data Aggregation     ###
###                         Emma Menchions                          ###
###                         March 4/23                              ###
#######################################################################

## OVERVIEW ----
# this script will take collection and occurrence only data, assign associated
# occurrences and taxa to the collection data, place this in the collected data folder,
# and then aggregate both types of data into one large Darwin Core formatted
# spreadsheet

# NOTE: to combine two sheets (collection data and occurrence data), the taxonomy
# reference system used in part 3 (darwin core conversion) on both types of data 
# must have been the SAME! 

## LOADING & INSTALLING PACKAGES ----

  # using groundhog to manage package versioning 
  #install.packages("groundhog")
  library(groundhog)
  
  set.groundhog.folder(here::here("packages"))
  date <- "2022-11-02"
  requiredPackages <-  c("assertr","dplyr","here", "lubridate",
                         "magrittr","mapview","purrr","ritis","stringr",
                         "stringi","sjmisc","taxize","terra",
                         "tidyverse","tidyr", "cowsay")
  
  for (pkg in requiredPackages) {
    groundhog.library(pkg, date)
  }

  ## removing all objects from workspace to avoid errors by carrying 
  # over similar variable names
  rm(list = ls())

## 1) USER INPUT ----

  AI <- "HJ" # AUTHOR INITIALS 

## 2) READING IN DATA ----

# reading in most recent round of DwC data...
  
  # for collection records
  coll_data <- read.csv(
      here::here("data","data_digitization",
                 "collection_data","darwin_core_data", 
                 unique(as.character(max(list.files(  
                   here::here("data","data_digitization",
                              "collection_data", 
                              "darwin_core_data")))))))

  # for occurrence records
  occ_data <- read.csv(
  here::here("data","data_digitization",
             "occurrence_data","darwin_core_data", unique(as.character(max(list.files(  
               here::here("data","data_digitization",
                          "occurrence_data", 
                          "darwin_core_data"))))))) %>% 

            # creating column of names which will be assigned as associate species names for collected occurrences
            # at same location
            unite("canonicalName", genus, specificEpithet, intraspecificEpithet, na.rm=T, sep= " ", remove=F)

## 3) Assigning associate observation-only rows to collected observations

## associatedOccurrences
# temporary adjustment
occ_data$associatedOccurrences[is.na(occ_data$associatedOccurrences)] <- "none"

for (i in 1:dim(coll_data)[1]){ # for every row of the collected occurrences sheet...
  for(j in 1:dim(occ_data)[1]){ # for every row of the obs only sheet
    # if a row contains its occurrenceID...
    if(str_contains(occ_data$associatedOccurrences[j], coll_data$occurrenceID[i])){
      
      # assign its canonicalName
      
      # if collection data row already has associated occurrences...
      if(!is.na(coll_data$associatedOccurrences[i])){
        # append it to other ID's there
        coll_data$associatedOccurrences[i] <- paste0(coll_data$associatedOccurrences[i], "; ", occ_data$occurrenceID[j])
      
      # if not...
      } else {
      # place new ID there
      coll_data$associatedOccurrences[i] <- occ_data$occurrenceID[j]

      }
    }
  }
}

  # converting NA values back into true NA values 
  occ_data$associatedOccurrences[occ_data$associatedOccurrences=="none"] <- NA
  coll_data$associatedOccurrences[coll_data$associatedOccurrences=="none"] <- NA
     
## associatedTaxa
# temporary adjustment
occ_data$associatedTaxa[is.na(occ_data$associatedTaxa)] <- "none"

for (i in 1:dim(coll_data)[1]){ # for every row of the collected occurrences sheet...
  for(j in 1:dim(occ_data)[1]){ # for every row of the obs only sheet
    # if a row contains its occurrenceID...
    if(str_contains(occ_data$associatedOccurrences[j], coll_data$occurrenceID[i])){
      
      # assign its canonicalName
      
      # if collection data row already has associated occurrences...
      if(!is.na(coll_data$associatedTaxa[i])){
        # append name to names already there
        coll_data$associatedTaxa[i] <- paste0(coll_data$associatedTaxa[i], "; ", occ_data$canonicalName[j])
        
      # if not...
      } else {
        # write new name there
        coll_data$associatedTaxa[i] <- occ_data$canonicalName[j]
        
      }
    }
  }
}

  # converting NA values back into true NA values 
  occ_data$associatedTaxa[occ_data$associatedTaxa=="none"] <- NA
  coll_data$associatedTaxa[coll_data$associatedTaxa=="none"] <- NA
  
  # removing the temporary variable canonicalName to adhere to DwC format
  occ_data <- occ_data %>% select(-canonicalName)

## writing files
  # save to darwin core formatted (with associated occurrences) for collection data
    write.csv(coll_data, here::here("data","data_digitization",
           "collection_data","darwin_core_data",
           paste0(AI,"-Final-DwC-coll-data_",Sys.Date(),".csv")), row.names=F)

## 4) Combining occurrences and collection observations
  aggregated_data <- rbind(coll_data, occ_data) %>% arrange(eventDate)


## writing files
  write.csv(aggregated_data, here::here("data","data_digitization",
                                "aggregated_data",
                                paste0(AI,"-DwC-occ-and-coll-data_",Sys.Date(),".csv")), row.names=F)

