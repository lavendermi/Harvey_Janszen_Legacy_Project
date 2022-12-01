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
    here::here("data","digitized_data", "raw_data",
               "HJ-7-occ-entry.xlsx"))

  # renaming column names for easier recognition 
  data <- data %>%dplyr::rename(archiveID="[archiveID]", pageNum = "[pageNum]", 
                                numPage = "[numPage]", vName = "[vName]",
                                vSciName= "[vSciName]", sciName = "[sciName]",
                                conf="[conf]",date="[date]",
                                locality="[locality]", country = "[country]",
                                stateProvince = "[stateProvince]", 
                                island ="[island]")

## 1. Extract rows where... ----
# a) vName, vSciName, or sciName are empty
# b) there are dataEntry remarks
# c) there is no date
# d) there is no locality, country, stateProvince
# e) rows with low confidence in taxon name deciphering

to_check_high_priority <- data %>% 
  dplyr::filter(is.na(vName) | is.na(vSciName) | is.na(sciName) | 
                  !is.na(dataEntryRemarks) |
                  is.na(date)| 
                  is.na(locality) | conf=="l") %>% 
    dplyr::filter(., rowSums(is.na(.)) != ncol(.)) # removing rows that contain all NA values

  ## saving file 
  write.csv(to_check_high_priority, here::here("data","digitized_data","data_checking", paste0("occ-data-to-check_HIGH-PRIORITY_",
                                                                Sys.Date(),".csv")))
## If time add rows where there is medium confidence in taxon name deciphering

  to_check <- rbind(to_check_high_priority, data[data$conf=="m",]) 
  to_check <- to_check %>% dplyr::filter(., rowSums(is.na(.)) != ncol(.))

  ## saving file 
  write.csv(to_check, here::here("data","digitized_data","data_checking", paste0("occ-data-to-check_ALL_",Sys.Date(),".csv")))
