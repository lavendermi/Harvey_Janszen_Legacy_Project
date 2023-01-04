#######################################################################
###       Processing Step 3: conversion from Data Entry Template    ###
###                      to Darwin Core Collections                 ###
###                         Emma Menchions                          ###
###                         Created Nov. 7/22                       ###
#######################################################################

## OVERVIEW ----
# this script will take collection and occurrence only data, assign associated
# occurrences and taxa to the collection data, place this in the collected data fodler,
# and then aggregate both types of data into one large Darwin Core formatted
# spreadsheet

## 1) LOADING & INSTALLING PACKAGES ----
# using groundhog to manage package versioning 
#install.packages("groundhog")
library(groundhog)

set.groundhog.folder(here::here("packages"))
date <- "2022-11-02"
requiredPackages <-  c("assertr","dplyr","here", "lubridate",
                       "magrittr","mapview","purrr","ritis",
                       "stringi","sjmisc","taxize","terra","tidyverse","tidyr")

for (pkg in requiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {groundhog.library(pkg, date)}
}
rm(requiredPackages)

## 2) READING IN DATA ----

coll_data <- read.csv(here::here("data","data_digitization",
                            "collection_data",
                            "field_note_data",
                            "darwin-core-collections_2023-01-03.csv"))

occ_data <- read.csv(here::here("data","data_digitization",
                                 "occurrence_data",
                             "darwin-core-occurrences_2023-01-03.csv")) %>% 
            # creating column 
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
        coll_data$associatedOccurrences[i] <- paste0(coll_data$associatedOccurrences[i], "; ", occ_data$occurrenceID[j])
      
      # if not...
      } else {
        
      coll_data$associatedOccurrences[i] <- occ_data$occurrenceID[j]

      }
    }
  }
}

occ_data$associatedOccurrences[occ_data$associatedOccurrences=="none"] <- NA
     
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
        coll_data$associatedTaxa[i] <- paste0(coll_data$associatedTaxa[i], "; ", occ_data$canonicalName[j])
        
      # if not...
      } else {
        
        coll_data$associatedTaxa[i] <- occ_data$canonicalName[j]
        
      }
    }
  }
}

occ_data$associatedOccurrences[occ_data$associatedOccurrences=="none"] <- NA
occ_data <- occ_data %>% select(-canonicalName)

## 4) Combining occurrences and collection observations
aggregated_data <- rbind(coll_data, occ_data) %>% arrange(eventDate)
