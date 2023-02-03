###########################################################
#######             Processing step Ib.             #######
#######       Consolidating checked and raw data    #######
#######           Emma Menchions 22-11-22           #######                       
###########################################################

## This script takes digitized herbarium label data and 
# compares it to that found in the rbcm database based 
# on accession numbers

# It is designed to be ITERATIVE so that you can check just new 
# rows for digitized herbarium label data as they are 
# entered if desired

# TO DO: 

# Species name matches 
# date of collection matches 
# locality matches

## LOADING PACKAGES ----
library(groundhog)

set.groundhog.folder(here::here("packages"))
date <- "2022-11-02"
requiredPackages <-  c("assertr","expss", "readxl","dplyr",
                       "here", "tidyverse","tidyr")


for (pkg in requiredPackages) {
  groundhog.library(pkg, date)
}

rm(requiredPackages)

## 1. LOADING DATA ----

  # read in the file with the latest date (with most observations) 
  label_data <- read.csv(here::here("data", "data_digitization","rbcm_data","label_data",
                                  "processed_data", unique(as.character(max(list.files(
                                    here::here("data", "data_digitization","rbcm_data","label_data",
                                               "processed_data"))))))) 

  rbcm_data <- read.csv(here::here("data", "existing_data","rbcm_HJ-specimens.csv")) %>% 
    rename(accessionNum=D)
 
  # Removing rows already compared for faster comparison
  # if already processed data, append new data to it 
  if(length(list.files(here::here("data", "data_digitization",
                                "rbcm_data","label_data",
                                "processed_data")))>=1){
  
  # FILL IN !!!!!*************
    
  # remove rows from current data table that contain old data                                              
    label_data <- label_data %>% anti_join(old_data)
    
    
  } 
  
##  Accession number: is it found in the database? ---
  
  # for every row, if it has a match, set it on a certain pathway
  # if it does not, make a new var to store those, and should be checked manually to see if there
  # by writing a sheet facillitating this ? 
  
  no_match <- data.frame(matrix(ncol = 14, nrow = 0))
  names(no_match) <- names(label_data)
  
  match <- data.frame(matrix(ncol = 14, nrow = 0))
  names(match) <- names(label_data)
  nums <- c()
  for (i in 1:dim(label_data)[1]){
    if(length(rbcm_data$accessionNum[rbcm_data$accessionNum == label_data$accessionNum[i]])>0){
      match[i,] <- label_data[i,] %>% filter_all(any_vars(!is.na(.)))
    }else{
      no_match[i,] <- label_data[i,]
      
    }
  }
  
  # removing na rows 
  no_match <- no_match %>% filter_all(any_vars(!is.na(.)))
  match <- match %>% filter_all(any_vars(!is.na(.)))
  
  # for these suspected missing matches, are there any of the same species collected on same date?
  no_match$potential_match <- c()
  for (i in 1:dim(no_match[i])){
    
    # if there is a match in genus, species, and collection date or genus, species and locality ...
    if(length(rbcm_data$accessionNum[which(rbcm_data$CollectionDate == no_match$CollectionDate[i] & 
                      rbcm_data$Genus == no_match$Genus[i] & 
                      rbcm_data$Species == no_match$Species[i])])>0){
      # 
      no_match$potential_match[i] <- rbcm_data$accessionNum[
        which(rbcm_data$CollectionDate == no_match$CollectionDate[i] & 
        rbcm_data$Genus == no_match$Genus[i] & 
        rbcm_data$Species == no_match$Species[i] | 
        rbcm_data$Species == no_match$Species[i] & 
        rbcm_data$Genus == no_match$Genus[i] &
        rbcm_data$LocationName == no_match$LocationName[i]
          )]
    }
  }

  # writing csv file for herbarium sheets suspected with no matching
  # accession number in the rbcm database
  if(dim(no_match)[1]>0){
  write.csv(no_match, here::here("data","data_digitization", "rbcm_data", "label_data",
                                 "review","suspected-not-in-database", paste0("missing_",
                                                                      Sys.Date(), ".csv")),
            row.names = F)
  }

  
  
## 2. HAVE ALL ROWS IN CHECK DATA BEEN REVIEWED? ----
  checked_data %>% 
  assert(in_set("C", "R", "c","r"), checkStatus) 
  
## 3. CONSOLIDATNG & REMOVING ROWS ----
  
  # removing rows which are empty
  raw_data <- raw_data %>%  
    dplyr::filter(!is.na(vName) & 
    !is.na(vSciName) &
    !is.na(sciName)) %>% 
    dplyr::filter(., rowSums(is.na(.)) != ncol(.)) 
  
  checked_data <- checked_data %>% 
    dplyr::filter(!is.na(vName) & 
                    !is.na(vSciName) &
                    !is.na(sciName)) %>% 
    dplyr::filter(., rowSums(is.na(.)) != ncol(.)) 
  
  for (i in 1:dim(checked_data)[1]){  # for each row in the checked frame
      if (checked_data$toDelete[i] == "Y" | checked_data$toDelete[i] == "y"){
        for (j in 1:dim(raw_data)[1]){
          if(checked_data$vName[i] == raw_data$vName[j]){
              raw_data[j,] <- NA
              raw_data[j,"vName"] <- "missing"
              
              
          }
        }
      } else {
      raw_data[ # if its archiveID, pageNum and numPage match that in raw data
      which(checked_data$pageNum[i] == raw_data$pageNum &
      checked_data$numPage[i] == raw_data$numPage),] <- 
        # assign raw data column to that row of checked data
        checked_data[i,3:dim(checked_data)[2]]
    }
  }
  
  raw_data[raw_data$vName=="missing","vName"] <- NA
  
## 4. WRITING CHECKED FILE FOR NEXT PROCESSING STEP ----
  
  processed_data_1 <- raw_data %>% # removing rows that contain all NA values
                      relocate(., dataEntryRemarks, .after= idBy) %>% 
                    dplyr::filter(., rowSums(is.na(.)) != ncol(.)) 
  
  # saving to data_cleaning folder to prepare for Processing step 2 (cleaning)
  write.csv(processed_data_1, 
            here::here("data", "data_digitization", 
            "collection_data","3_data_cleaning",paste0("HJ",J),
            paste0("HJ", J, "-processed-step-1_",Sys.Date(),".csv")),
            row.names = F)
  
  # and removing old files to save storage
  if(length(list.files(here::here("data", 
                                  "data_digitization","collection_data",
                                  "3_data_cleaning", paste0("HJ",J))))>2){
    file.remove(unique(as.character(min(list.files(here::here("data", 
                                                              "data_digitization","collection_data",
                                                              "3_data_cleaning", paste0("HJ",J)))))))   
  }
            
## Keeping record of which rows have been compared already 
  write.csv(comp_rows )
  
  