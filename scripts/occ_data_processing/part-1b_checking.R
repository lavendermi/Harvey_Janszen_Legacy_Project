###########################################################
#######             Processing step Ib.             #######
#######       Consolidating checked and raw data    #######
#######           Emma Menchions 22-11-22           #######                       
###########################################################

## OVERVIEW ----
# will append changes and reviews made in part 1a 
# to the raw data and write it as a new file in 
# the folder "3_data_cleaning" for use in the next script (part-2)

## LOADING PACKAGES ----
library(groundhog)

set.groundhog.folder(here::here("packages"))
date <- "2022-11-02"
requiredPackages <-  c("assertr","expss", "readxl","dplyr",
                       "here", "tidyverse","tidyr", "cowsay", 
                       "multicolor", "jsonlite", "Rfast")

for (pkg in requiredPackages) {
  groundhog.library(pkg, date)
}

rm(requiredPackages)

## USER INPUT ----

J <- c(5,7,8,9,27) # JOURNAL NUMBERS (only ONE at a time ! )
AI <- "HJ" # AUTHOR INITIALS 

## 1. LOADING DATA ----

  ## loading most recent checked data entry sheet from part 1a
  checked_data <- read.csv(
  here::here("data","data_digitization",
             "occurrence_data","2_data_checking",
             as.character(unique(max(list.files(here::here("data","data_digitization",
                                                           "occurrence_data",
                                                           "2_data_checking")))))))
# initializing variable for loop
raw_data <- data.frame()

## loading raw data
for (i in 1:length(J)){
  # if there is a raw data file for a given journal number
  if (length(list.files(here::here(
    "data","data_digitization",
    "occurrence_data", 
    "1_raw_data")))>0){
    
    if (dim(raw_data)[1] == 0){ # and if it is the first journal number in the sequence
      # read in data to a new variable from that file
      
      raw_data <- read_excel(here::here("data","data_digitization", 
                                          "occurrence_data","1_raw_data", 
                                          paste0(AI,"-",J[i],"-","occ-entry.xlsx"))) %>% 
        add_column(archiveID = J[i]) # adding journal number
    } else{ # if it is the second + sheet being read in, append it to data already read in
      
      temp_var <- read_excel(here::here("data","data_digitization", 
                                        "occurrence_data","1_raw_data", 
                                        paste0(AI,"-",J[i],"-","occ-entry.xlsx"))) %>% 
        add_column(archiveID=J[i]) # adding journal number
      raw_data <- rbind(raw_data,temp_var ) 
      
      cowsay::say("raw data from multiple journals appended", by="signbunny")
    }
  }
}

  # removing brackets from column names in template
  raw_data <- raw_data %>% 
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
    
  ## finding rows previously processed and removing these from raw data 
    
    # if there are one or more files that have been previously processed...
    if(length(list.files(here::here("data", "data_digitization",
                                    "occurrence_data", "prev_proccessed")))>1){
      
      # read in the file with the second latest date
      old_data <- read.csv(here::here("data", "data_digitization","occurrence_data",
                                      "prev_proccessed",
                                        unique(as.character(dplyr::nth(sort(list.files(
                                        here::here("data", "data_digitization","occurrence_data",
                                                   "prev_proccessed"))),
                                          length(list.files(
                                          here::here("data", "data_digitization","occurrence_data",
                                                     "prev_proccessed")))-1)
                                        ))))  
    
      # forcing columns from new and old dataframes to be read as 
      # the same classes so they can be joined
      
      # obtaining classes of raw data frame
      classes_raw_data <- lapply(raw_data,class)
      classes_raw_data <- bind_rows(classes_raw_data)
      classes_raw_data <- unlist(classes_raw_data)
      
      # applying to old_data frame
      old_data <- Map('class<-', old_data, classes_raw_data) %>% bind_rows()
      
      # removing rows from current data table that contain old data                                              
     raw_data <- raw_data %>% 
       anti_join(old_data) %>% 
       relocate(., dataEntryRemarks, .before= pageNum)
     
     # message
     cowsay::say("old data removed!", by="signbunny")
     
    }
  
  
## 2. HAVE ALL ROWS IN CHECKED DATA BEEN REVIEWED? ----
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
    
    # if the "toDelete" column has "Y" or "y", 
      if (checked_data$toDelete[i] == "Y" | checked_data$toDelete[i] == "y"){
        
        # delete that observation by defining row as NA 
        # and defining the vName as missing
        for (j in 1:dim(raw_data)[1]){
          if(checked_data$vName[i] == raw_data$vName[j]){ 
              raw_data[j,] <- NA
              raw_data[j,"vName"] <- "missing"
              
          }
        }
      } else { # if a given row in the checked data does not have a "Y" in the "toDelete" column...
        
      # if its pageNum, and numPage match that in raw data
      raw_data[ 
      which(checked_data$pageNum[i] == raw_data$pageNum &
      checked_data$numPage[i] == raw_data$numPage),] <- 
      # assign raw data column to that row of checked data
        checked_data[i,3:dim(checked_data)[2]]
    }
  }
  
  # setting vName which had to "toDelete" in the checked data that was assigned as "missing" 
  # in the loop (to make the loop work), assigning them as NA now so that these whole rows can
  # be removed later
  raw_data[raw_data$vName=="missing","vName"] <- NA
  
## 4. WRITING CHECKED FILE FOR NEXT PROCESSING STEP ----
  
  processed_data_1 <- raw_data %>% # removing rows that contain all NA values (toDelete entries from previous step)
                      relocate(., dataEntryRemarks, .after= idBy) %>% # relocating dataEntryRemarks column for easier reading of csv file
                      dplyr::filter(., rowSums(is.na(.)) != ncol(.))  # another check to ensure no NA rows retained
  
  # saving to data_cleaning folder to prepare for Processing step 2 (cleaning)
  write.csv(processed_data_1, 
            here::here("data", "data_digitization", 
            "occurrence_data","3_data_cleaning",
            paste0(AI, "-processed-step-1_",Sys.Date(),".csv")),
            row.names = F)
  
  # and removing old files to save storage
  if(length(list.files(here::here("data", 
                                  "data_digitization","occurrence_data",
                                  "3_data_cleaning")))>2){
    file.remove(unique(as.character(min(list.files(here::here("data", 
                                                              "data_digitization",
                                                              "occurrence_data",
                                                              "3_data_cleaning", 
                                                              ))))))
    cowsay::say("old file removed!", by="signbunny")
  }
  
            
            
  