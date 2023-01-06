############################################################################
###         Conversion of Field Notes for from Darwin Core               ###
###           Collection data from Darwin Core to the                    ###
###              format of the Royal BC Museum Herbarium                 ###
###                         Emma Menchions                               ###
###                         Created Jan3/23                              ###
############################################################################


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

dwc_data <- read.csv(
  here::here("data","data_digitization",
             "collection_data","darwin_core_data", 
             paste0("HJ",J), unique(
               as.character(max(list.files(here::here("data","data_digitization",
                                                      "collection_data",
                                                      "darwin_core_data", paste0("HJ",J))))))))

rbcm_database <- read.csv(here::here("data","existing_data","rbcm_HJ-specimens.csv"))






# only retaining rows that have recordNumber in rbcm_database
dwc_data <- dwc_data %>% 
  dplyr::rename(CollectorsFieldNumber= recordNumber) 
RBCM_format <- dwc_data[0,]
rejected<-dwc_data[0,]

for (i in 1:dim(dwc_data)[1]){ # for every row
  # if there is a matching record number in field notes and databse
  if(length(rbcm_database$CollectorsFieldNumber[
    which(dwc_data$CollectorsFieldNumber[i]==rbcm_database$CollectorsFieldNumber)])==1){
    
    RBCM_format[i,] <- dwc_data[i,]
    
  } else if (length(rbcm_database$CollectorsFieldNumber[
    which(dwc_data$CollectorsFieldNumber[i]==rbcm_database$CollectorsFieldNumber)])==0){
    rejected[i,] <- dwc_data[i,]
  }
  
}