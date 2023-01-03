###########################################################
#######             Processing step Ia.             #######
#######       Checking entered collection data      #######
#######           Emma Menchions 22-11-22           #######                       
###########################################################

# The goal of this script is to....

# 1. identify occurrence rows flagged by individual during 
# data entry either by:
# a) remarking something in the dataEntryRemarks
# b) Not entering all taxon name information
# c) low confidence in taxon name interpretation 
# d) uncertainty in location and and no locality entered

# 2. Create a new spreadsheet of these collections so they 
# this uncertainty can be checked one row at a time

## LOADING PACKAGES ----
library(groundhog)

set.groundhog.folder(here::here("packages"))
date <- "2022-11-02"
requiredPackages <-  c("assertr","readxl","dplyr","here", "tidyverse","tidyr")

for (pkg in requiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {groundhog.library(pkg, date)}
}
rm(requiredPackages)

## 1. LOADING IN DATA ----
 
  # loading raw data 
  J <-7 # USER INPUT

  data <- read_excel(here::here("data","data_digitization", 
          "collections_data","raw_data", 
          paste0("HJ-",J,"-","occ-entry.xlsx")))

  # renaming column names for easier recognition 
  data <- data %>% dplyr::rename(pageNum = "[pageNum]", 
                                numPage = "[numPage]", 
                                vName = "[vName]",
                                vSciName= "[vSciName]", 
                                sciName = "[sciName]",
                                conf="[conf]",date="[date]",
                                locality="[locality]", 
                                country = "[country]",
                                stateProvince = "[stateProvince]", 
                                island ="[island]")
  
## 2. EXTRACTING ROWS & WRITING SHEET FOR CHECKING ----
  
# Extract rows where... 
# a) vName, vSciName, or sciName are empty
# b) there are dataEntry remarks
# c) there is no date
# d) there is no locality, country, stateProvince
# e) rows with lower confidence in taxon name deciphering

  Q <- "hp" # "all" = will also include rows with 
            # medium confidence in taxon name decipher
  
             # "hp" = high priority - will only write file 
             # with these conditions and taxon 
             # name with LOW confidence only
  
  if (Q == "hp"){

    to_check <- data %>% 
      dplyr::filter(is.na(vName) | is.na(vSciName) | is.na(sciName) | 
                      !is.na(dataEntryRemarks) |
                      is.na(date)| 
                      is.na(locality) | conf=="l") %>% 
      # removing rows that contain all NA values
        dplyr::filter(., rowSums(is.na(.)) != ncol(.)) %>%  
      # adding new column to which can be used to keep track of 
      # data checking progress
        mutate("checkStatus"= "", 
               .before=pageNum) %>% 
        mutate("toDelete"= "", # adding column to indicate which entries
           # too poor or uncertain to include
           .before=pageNum) %>% 
        relocate(., dataEntryRemarks, .after= toDelete)
    
    
      ## saving file 
      write.csv(to_check, 
                here::here("data","data_digitization","occurrence_data", 
                "data_checking", 
                 paste0("HJ-",J,"_","occ-data-to-check_HIGH-PRIORITY_",
                 Sys.Date(),".csv")), row.names = F)
      
    } else if (Q=="all"){
      
      to_check <- data %>% 
        dplyr::filter(is.na(vName) | is.na(vSciName) |
                        is.na(sciName) | 
                        !is.na(dataEntryRemarks) |
                        is.na(date)| 
                        is.na(locality) | conf=="l" | 
                        conf == "m") %>% 
        # removing rows that contain all NA values
        dplyr::filter(., rowSums(is.na(.)) != ncol(.)) %>%  
        # adding new column to which can be used to keep 
        # track of data checking progress
        mutate("checkStatus"= "",
               .before=pageNum) %>% 
        mutate("toDelete"= "", # adding column to indicate which entries
               # too poor or uncertain to include
             .before=pageNum) %>% 
       relocate(., dataEntryRemarks, .after= toDelete)
        
      
      ## saving file 
      write.csv(to_check, 
                here::here("data","data_digitization","occurrence_data",
                           "data_checking", 
                           paste0("HJ-",J,"_","occ-data-to-check_ALL_",
                           Sys.Date(),".csv")), row.names = F)
    } 
  
## 3. ADDRESS THE ISSUES IN THESE ROWS in CSV FILE ----
  # add data or change that data in these rows within the new
  # .csv file created
  # once they are all addressed... use script part1-b_checking 
  # to consolidate and remove rows that can't be filled 
  # in with all of the minimum required data
  

  