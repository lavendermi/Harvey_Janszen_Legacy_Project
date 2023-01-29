###########################################################
#######             Label Data Processing           #######
#######           Emma Menchions 2023-01-29         #######                       
###########################################################

# TO DO: 

# goal --> to make sure (link via accession numbers) 
# 1) record number on label, matches record number on sheet 
# Species name matches 
# date of collection matches 
# locality matches

# checking...
# all accession numbers have species name
# all accession numbers have record number between certain range


# with labels ...
# Add V0 to front of accession number
# Species name checking 
# locality checking
# convert date to rbcm format

## LOADING PACKAGES ----
library(groundhog)

set.groundhog.folder(here::here("packages"))
date <- "2022-11-02"
requiredPackages <-  c("assertr","readxl","dplyr","here", "tidyverse","tidyr", "stringi")

for (pkg in requiredPackages) {
  groundhog.library(pkg, date)
}

rm(requiredPackages)

## 1. LOADING IN RAW DATA ----

  total_data <- read_excel(here::here("data","data_digitization", 
          "rbcm_data","label_data","raw_data", "RBCM-label-entry.xlsx"))
  
  ## finding new rows entered 
  
  # if there is one or more files that have been previously processed...
  if(length(list.files(here::here("data", "data_digitization",
                                  "rbcm_data",
                                  "processed_data")))>=1){
    
    # read in the file with the latest date (with most observations) 
    old_data <- read.csv(here::here("data", "data_digitization","collection_data",
                                    "processed_data", unique(as.character(max(list.files(
      here::here("data", "data_digitization","rbcm_data",
                 "processed_data"))))))) 
    
    # converting accession number in processed data into same format as raw data
    # to match previously processed rows
    
    ## fill in!!!!!!!!!!.........

    # remove rows from current data table that contain old data      ***EDIT   *****                                      
    new_data <- total_data %>% anti_join(old_data)
    
  } else {
    new_data <- total_data
  }

## CHECKING & FORMATTING----

## Accession number

  # checking that all rows have number
  new_data %>% 
  group_by(accessionNum) %>% 
  assert(not_na, accessionNum) 

  # converting it to format in rbcm database
  new_data$accessionNum <- paste0("V0", new_data$accessionNum)
  
## Genus
  # should be capitalized, only contain letters, and not NA
  letters_only <- function(x) !grepl("^[^A-Za-z]+[[:space:]]+", x)
  genusCapitalized <- function(x) grepl("^[[:upper:]]", x)
  
    new_data %>% 
    chain_start %>%
    group_by(genus) %>% 
    assert(letters_only, genus) %>% # checking only letters from A to Z
    assert(not_na, genus) %>%  # checking that all rows have number
    assert(genusCapitalized, genus) %>% 
    chain_end
    
    new_data <- new_data %>% 
      dplyr::rename(Genus= genus)
  
## Species
  # should be not capitalized, only contain letters, and not NA
  specEpiphetNotCap <- function(x) !grepl("^[[:upper:]]", x)
  
    new_data %>% 
    chain_start %>%
    group_by(species) %>% 
    assert(letters_only, species) %>% # checking only letters from A to Z
    assert(not_na, species) %>%  # checking that all rows have number
    assert(specEpiphetNotCap, species) %>% 
    chain_end 
      
    new_data <- new_data %>% 
    dplyr::rename(Species= species)
    
## Subspecies
  # should be not capitalized, only contain letters
    
    new_data %>% 
    chain_start %>%
    group_by(ssp) %>% 
    assert(letters_only, ssp) %>% # checking only letters from A to Z
    assert(specEpiphetNotCap, ssp) %>% 
    chain_end 
    
    new_data <- new_data %>% 
      dplyr::rename(Subspecies= ssp)
    
## Variety
  # should be not capitalized, only contain letters
    
    new_data %>% 
      chain_start %>%
      group_by(var) %>% 
      assert(letters_only, var) %>% # checking only letters from A to Z
      assert(specEpiphetNotCap, var) %>% 
      chain_end 
    
    new_data <- new_data %>% 
      dplyr::rename(Variety= var)
    
## Forma
  # should be not capitalized, only contain letters
    
    new_data %>% 
      chain_start %>%
      group_by(form) %>% 
      assert(letters_only, form) %>% # checking only letters from A to Z
      assert(specEpiphetNotCap, form) %>% 
      chain_end 
    
    new_data <- new_data %>% 
      dplyr::rename(Forma= form)
    
## Locality 
    # must not be NA
    new_data %>% 
      assert(not_na, locality)
    
  # Creating LocationName and LocationDescription columns from "Locality" using Gazeteer ----
  # downloaded from https://catalogue.data.gov.bc.ca/dataset/bc-geographical-names
    BCplaces <- read.csv(here::here(
    "data","reference_data", "BCGW_7113060B_1672719865150_14800","GNSGGRPHCL.csv"))
    
    # initializing vectors
    placeNames <- c()
    new_data$LocationName <- NA
    new_data$LocationDescription <- NA
    new_data[is.na(new_data$locality), "locality"]<- "missing"
    
    for (i in 1:dim(new_data)[1]){ # for each row...
      # split up locality string into different placename segments...
      placeNames[i]<- strsplit(new_data$locality[i], "; ")
      placeNames[[i]] <- str_to_title(placeNames[[i]])
      
      # for every segment...
      for (j in 1:length(placeNames[[i]])){
        # if there is a match in gazeteer...
        if (length(BCplaces$GEO_NAME[placeNames[[i]][j] == BCplaces$GEO_NAME])>=1){
          
          # and if there has not already been another locationName
          # listed by a previous name segment...
          if(is.na(new_data$LocationName[i])){
            # assign the LocationName as this name segment
            new_data$LocationName[i] <- 
              BCplaces$GEO_NAME[placeNames[[i]][j]==BCplaces$GEO_NAME]
            
          } else { 
            # if there is already a LocationName, paste it on to that name
            new_data$LocationName[i] <- 
              paste0(new_data$LocationName[i],"; ", 
                     BCplaces$GEO_NAME[placeNames[[i]][j]==BCplaces$GEO_NAME])
          }
          
          # if there is no match in the gazeteer...
        } else if (length(BCplaces$GEO_NAME[placeNames[j] == BCplaces$GEO_NAME])<1){
          # assign it to the location description section
          new_data$LocationDescription[i] <- placeNames[[i]][j]
          
        }
      }
    }
      
    # removing old locality column
    new_data <- new_data %>%  select(-locality)
    new_data$LocationDescription <- tolower(new_data$LocationDescription)
    
## Date
    # must not be NA
    # must not be NA
    new_data %>% 
      group_by(date) %>% 
      assert(not_na, date)
    
    # renaming to rbcm database name
    new_data <- new_data %>% rename(CollectionDate = date)
    
## collNum
  # must not be NA, must be between 1 and 3000
    new_data %>% 
      chain_start %>%
      group_by(collNum) %>% 
      assert(not_na, collNum) %>% 
      # for records with only numeric collection numbers, are
      # they between 1 and 3000? 
      filter(str_detect(collNum, "^[:digit:]+$")) %>% 
      assert(in_set(1:3000),collNum) %>% 
      chain_end
    
  # making sure all letters (if present) are upper case
    new_data$collNum <- toupper(new_data$collNum) 
    
  # renaming to rbcm database name
    new_data <- new_data %>% 
    rename(CollectorsFieldNumber = collNum)
  
  
    
  

  