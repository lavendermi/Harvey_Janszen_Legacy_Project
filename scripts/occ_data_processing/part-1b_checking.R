###########################################################
#######             Processing step Ib.             #######
#######       Consolidating checked and raw data    #######
#######           Emma Menchions 22-11-22           #######                       
###########################################################

# Combine the filled-out checked data sheet with the raw
# data to form a data file that passes the first data check 
# processing step #1

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

  J <-5 # journal number (only ONE at a time) (USER INPUT) ! 
  
  # loading data
  
  raw_data <- read_excel(here::here("data","data_digitization",
                                    "occurrence_data", 
                                    "1_raw_data", paste0("HJ-",J,
                                     "-","occ-entry.xlsx"))) %>% 
    dplyr::rename(
      pageNum = "[pageNum]", 
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
                                      "prev_proccessed","template_format", paste0("HJ",J), unique(as.character(max(list.files(
                                        here::here("data", "data_digitization","occurrence_data",
                                                   "prev_proccessed","template_format", paste0("HJ",J))))))))  
      
      # remove rows from current data table that contain old data                                              
     raw_data <- raw_data %>% anti_join(old_data) %>% relocate(., dataEntryRemarks, .before= pageNum)

    }
  
  # most recent checked data entry
  checked_data <- read.csv(
    here::here("data","data_digitization",
    "occurrence_data","2_data_checking", 
    paste0("HJ",J), 
    as.character(unique(max(list.files(here::here("data","data_digitization",
                                "occurrence_data",
                                 "2_data_checking", paste0("HJ",J))))))))
  
## 2. HAVE ALL ROWS IN CHECK DATA BEEN REVIEWED? ----
  # (if no error message appears, then everything is fine)
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
            "occurrence_data","3_data_cleaning",
            paste0("HJ",J),
            paste0("HJ", J, "-processed-step-1_",Sys.Date(),".csv")),
            row.names = F)
  
  # and removing old files to save storage
  if(length(list.files(here::here("data", 
                                  "data_digitization","occurrence_data",
                                  "3_data_cleaning", paste0("HJ",J))))>2){
    file.remove(unique(as.character(min(list.files(here::here("data", 
                                                              "data_digitization","occurrence_data",
                                                              "3_data_cleaning", paste0("HJ",J)))))))   
  }
  
            
            
  