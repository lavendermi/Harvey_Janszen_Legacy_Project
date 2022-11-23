###########################################################
#######                 Checking entered            ####### 
#######     occurrence data in data entry template  #######
#######                 Emma Menchions 22-11-22     #######                       
###########################################################

# The goal of this script is to identify places where there 
# is missing data, and when notes have been made to check
# and validate the data that was entered 

## LOADING PACKAGES ----
library(groundhog)

date <- "2022-11-02"
requiredPackages <-  c("assertr","readxl","dplyr","here", "tidyverse","tidyr")

for (pkg in requiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {groundhog.library(pkg, date)}
}
rm(requiredPackages)

## READING IN DATA ----
  data <- read_excel(
    here::here("data","digitized_data",
               "HJ-occ-entry-template_22-11-11.xlsx"))

## 1) check where there are dataEntryRemarks, keep these columns

## 2) Check where there is missing sciName and vSci Name

## 3) Check where there is low confidence in taxon name 

## 4) If time, places where medium confidence in taxon name 