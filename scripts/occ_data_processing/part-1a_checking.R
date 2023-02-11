###########################################################
#######             Processing step Ia.             #######
#######       Checking entered occurrence data      #######
#######           Emma Menchions 22-11-22           #######                       
###########################################################

# The goal of this script is to....

# 1. identify occurrence rows flagged by individual during 
# data entry either by:
# a) remarking something in the dataEntryRemarks
# b) Not entering all taxon name information
# c) low confidence in taxon name interpretation 
# d) uncertainty in location and and no locality entered

# 2. Create a new spreadsheet of these observations so they 
# this uncertainty can be checked one observation at a time

## LOADING PACKAGES ----
library(groundhog)

set.groundhog.folder(here::here("packages"))
date <- "2022-11-02"
requiredPackages <-  c("assertr","readxl","dplyr","here", 
                       "tidyverse","tidyr", "cowsay", "multicolor",
                       "jsonlite")

for (pkg in requiredPackages) {
  groundhog.library(pkg, date)
}

rm(requiredPackages)

## 1. LOADING IN RAW DATA ----
 
  
  J <- 27 # Journal number (only ONE at a time) --> USER INPUT !
  
  # loading raw data
  total_data <- read_excel(here::here("data","data_digitization", 
                           "occurrence_data","1_raw_data", 
                            paste0("HJ-",J,"-","occ-entry.xlsx"))) %>% 
    dplyr::rename(pageNum = "[pageNum]", 
                  numPage = "[numPage]", 
                  vName = "[vName]",
                  vSciName= "[vSciName]", 
                  sciName = "[sciName]",
                  conf="[conf]",date="[date]",
                  locality="[locality]", 
                  country = "[country]",
                  stateProvince = "[stateProvince]", 
                  island ="[island]")
  
  ## finding new rows entered 
  
  # if there is one or more files that have been previously processed...
  if(length(list.files(here::here("data", "data_digitization",
                                  "occurrence_data", "prev_proccessed", 
                                  "template_format",
                                paste0("HJ",J))))>=1){
    
    # read in the file with the latest date (with most observations) 
    old_data <- read.csv(here::here("data", "data_digitization","occurrence_data",
                                    "prev_proccessed","template_format", 
                                    paste0("HJ",J), unique(as.character(max(list.files(
      here::here("data", "data_digitization","occurrence_data",
                 "prev_proccessed","template_format", paste0("HJ",J))))))))  
    
    # remove rows from current data table that contain old data                                              
    new_data <- total_data %>% anti_join(old_data)
    
    cowsay::say("previously proccessed rows removed :)", by="signbunny")
    
  } else {
    new_data <- total_data
    cowsay::say("all rows new and kept :)", by="signbunny")
  }
  
## writing sheet of total columns reviewed/ processed
  write.csv(total_data, here::here("data", "data_digitization","occurrence_data",
                                 "prev_proccessed", "template_format",paste0("HJ",J), 
                                 paste0("HJ-", J, "_rows-reviewed_",
                                        Sys.Date(),".csv")), row.names=F)
  
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

    to_check <- new_data %>% 
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
                here::here("data",
                  "data_digitization","occurrence_data", 
                "2_data_checking", paste0("HJ",J),
                 paste0("HJ-",J,"_","occ-data-to-check_HIGH-PRIORITY_",
                 Sys.Date(),".csv")), row.names = F)
      
    } else if (Q=="all"){
      
      to_check <- new_data %>% 
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
                           "2_data_checking", paste0("HJ",J),
                           paste0("HJ-",J,"_","occ-data-to-check_ALL_",
                           Sys.Date(),".csv")), row.names = F)
    } 
  
  # and removing old files to save storage
  if(length(list.files(here::here("data", 
                                  "data_digitization","occurrence_data",
                                  "2_data_checking", paste0("HJ",J))))>4){
    file.remove(unique(as.character(min(list.files(here::here("data", 
                                                              "data_digitization","occurrence_data",
                                                              "2_data_checking", paste0("HJ",J)))))))  
    cowsay::say("old files removed :)", by="signbunny")
  }
  
  
## 3. ADDRESS THE ISSUES IN THESE ROWS in CSV FILE ----
  # add data or change that data in these rows within the new
  # .csv file created
  # once they are all addressed... use script part1-b_checking 
  # to consolidate and remove rows that can't be filled 
  # in with all of the minimum required data
  

  