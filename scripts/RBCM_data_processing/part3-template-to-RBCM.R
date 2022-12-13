#######################################################################
###       Processing Step 3: conversion from Data Entry Template    ###
###                      to RBCM Collections data format             ###
###                         Emma Menchions                          ###
###                         Created Nov. 7/22                       ###
#######################################################################

## overview


## 1) LOADING & INSTALLING PACKAGES ----
# using groundhog to manage package versioning 
#install.packages("groundhog")
library(groundhog)

date <- "2022-11-02"
requiredPackages <-  c("assertr","dplyr","here", "lubridate","magrittr","purrr","ritis",
                       "stringi","taxize","terra","tidyverse","tidyr")

for (pkg in requiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {groundhog.library(pkg, date)}
}
rm(requiredPackages)

## 2) READING IN DATA ----

index <- c(8) # USER INPUT 

for (i in index){
  if (i == index[1]){
    data <- read_excel(
      here::here("data","digitized_data",
                 "collection_data", 
                 "field_note_data",
                 "raw_data", 
                 paste0("HJ-",index[1], "-collection-entry.xlsx"))) 
  } else{ 
    data <- rbind(data, read_excel(
      here::here("data","digitized_data",
                 "collection_data", 
                 "field_note_data",
                 "raw_data", 
                 paste0("HJ-",index[1], "-collection-entry.xlsx"))))
  }
}

## Converting columns to have similar names to RBCM ---

## 

# convert SciName after taxonomy check 
data <- data %>% dplyr::rename(sciName <- )

# use taxize to match common name? 

# overwrite locality, coordinates and habitat if there is nothing there in their database? 



