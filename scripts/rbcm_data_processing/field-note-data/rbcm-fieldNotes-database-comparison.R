


## 1) LOADING & INSTALLING PACKAGES ----
# using groundhog to manage package versioning 

install.packages("groundhog")
library(groundhog)

set.groundhog.folder(here::here("packages"))
date <- "2022-11-02"

requiredPackages <-  c("assertr","dplyr","here", "lubridate","magrittr","purrr","ritis",
                       "stringi","taxize","terra","tidyverse","tidyr")

for (pkg in requiredPackages) {
  groundhog.library(pkg, date)
}

rm(requiredPackages)

## 2) READING IN DATA ----

J <- 7 # journal number

# loading in most recent file
field_notes <- read.csv(
  here::here("data","data_digitization",
             "rbcm_data","field_note_data", 
             paste0("HJ",J), unique(
               as.character(max(list.files(here::here("data","data_digitization",
                                                      "rbcm_data","field_note_data",
                                                       paste0("HJ",J))))))))

rbcm_database <- read.csv(here::here("data","existing_data","rbcm_HJ-specimens.csv")) %>% 
  na_if("")

   

# only retaining rows that have recordNumber in rbcm_database
rows_to_comp <- field_notes[0,]
rejected<-field_notes[0,]

for (i in 1:dim(field_notes)[1]){ # for every row
  # if there is a matching record number in field notes and databse
  if(length(rbcm_database$CollectorsFieldNumber[
    which(field_notes$CollectorsFieldNumber[i]==rbcm_database$CollectorsFieldNumber)])==1){
    
    rows_to_comp[i,] <- field_notes[i,]
    
  } else if (length(rbcm_database$CollectorsFieldNumber[
    which(field_notes$CollectorsFieldNumber[i]==rbcm_database$CollectorsFieldNumber)])==0){
    rejected[i,] <- field_notes[i,] 
  }
}

# removing rows that contain all NA values
  rejected <- rejected %>% 
  dplyr::filter(., rowSums(is.na(.)) != ncol(.)) %>% 
  
  temp_rbcm_data
  seperate(ScienfiticName, c("name","","y"), " ") 

  
# finding matches with potential unnumbered collections in RBCM database 
  # matching names 
  # separating Scientific name into name and authority and discarding authority
  for (i in 1:dim(rbcm_database)[1]){
    if(!is.na(rbcm_database$Species[i]) & is.na(rbcm_database$Subspecies[i]) & is.na(rbcm_database$Variety[i]) & is.na(rbcm_database$Forma[i])){
      # retain genus and species
      name <- str_split(rbcm_database$ScientificName[i],pattern=" ")[[1]][1:2]
      rbcm_database$ScientificName[i] <- paste(name[1],name[2])
      
    }else if (!is.na(rbcm_database$Species[i])& !is.na(rbcm_database$Subspecies[i]) | !is.na(rbcm_database$Variety[i]) | !is.na(rbcm_database$Forma[i])){
      # retain genus, species, intraspeicifc epithet
      name <- str_split(rbcm_database$ScientificName[i],pattern=" ")[[1]][1:2]
      rbcm_database$ScientificName[i] <- paste(name[1],name[2])
      
      if(!is.na(rbcm_database$Subspecies[i])){
        rbcm_database$ScientificName[i] <- paste(rbcm_database$ScientificName[i],rbcm_database$Subspecies[i])
      } else if (!is.na(rbcm_database$Variety[i])){
        rbcm_database$Variety[i] <- paste(rbcm_database$ScientificName[i],rbcm_database$Variety[i])
      } else if (!is.na(rbcm_database$Forma[i])){
        rbcm_database$Forma[i] <- paste(rbcm_database$ScientificName[i],rbcm_database$Forma[i])
        
      }
      
    }else if (!is.na(rbcm_database$Species[i])){ # retain genus
      rbcm_database$ScientificName[i] <- str_split(rbcm_database$ScientificName[i],pattern=" ")[[1]][1]
    }
  }
  
  rbcm_unnumbered <- rbcm_database %>% 
    filter(CollectorsFieldNumber == "s.n" | is.na(CollectorsFieldNumber))
  # are there matches for name, date and first part of location description? 
  
  
  potential_field_note_entries <- rbcm[0,]
  for (i in 1:dim(rejected)[1]){
   if(length(rbcm_unnumbered[which(rejected$ScientificName[i]==rbcm_unnumbered$ScientificName & rejected$CollectionDate[i] == rbcm_unnumbered$CollectionDate)])>=1){
     potential_field_note_entries[i,] <- 
     
     
  
  
  
  
