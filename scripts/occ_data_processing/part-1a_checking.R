###########################################################
#######             Processing step Ia.             #######
#######       Checking entered occurrence data      #######
#######           Emma Menchions 22-11-22           #######                       
###########################################################

## OVERVIEW ----
# The goal of this script is to....

# 1. identify occurrence rows flagged by individual during 
# data entry either by:
# a) remarking something in the dataEntryRemarks
# b) Not entering all taxon name information
# c) low confidence in taxon name interpretation 
# d) uncertainty in location and and no locality string entered

# 2. Create a new spreadsheet of these observations so that rows
# with these uncertainties can be reviewed, infromation added
# or can be deleted if uncertainties can't be resolved

## LOADING PACKAGES ----

  # using the ground hog package for package management
  install.packages("groundhog")
  library(groundhog)

  # installs pacakages to local folder in working directory "packages"
  set.groundhog.folder(here::here("packages"))
  
  # installs package versions that were used when building this script
  date <- "2022-11-02"
  
  # packages required
  requiredPackages <-  c("assertr","readxl","dplyr","here", 
                         "tidyverse","tidyr", "cowsay", "multicolor",
                         "jsonlite")
  
  # loading and installing
  for (pkg in requiredPackages) {
    groundhog.library(pkg, date)
  }
  
  # removing vars
  rm(requiredPackages)
  
## USER INPUT ----
  
  J <- c(5,7,8,9,27) # JOURNAL NUMBERS (only ONE at a time ! )
  AI <- "HJ" # AUTHOR INITIALS 

## 1. LOADING IN RAW DATA ----
  
  # initializing variable for loop
  total_data <- data.frame()
  
  ## loading raw data
  for (i in 1:length(J)){
    # if there is a raw data file for a given journal number
    if (length(list.files(here::here(
      "data","data_digitization",
      "occurrence_data", 
      "1_raw_data")))>0){
      
      if (dim(total_data)[1] == 0){ # and if it is the first journal number in the sequence
        # read in data to a new variable from that file
        
        total_data <- read_excel(here::here("data","data_digitization", 
                                            "occurrence_data","1_raw_data", 
                                            paste0(AI,"-",J[i],"-","occ-entry.xlsx"))) %>% 
          add_column(archiveID = J[i]) # adding journal number
        
      } else{ # if it is the second + sheet being read in, append it to data already read in
        
        temp_var <- read_excel(here::here("data","data_digitization", 
                                          "occurrence_data","1_raw_data", 
                                          paste0(AI,"-",J[i],"-","occ-entry.xlsx"))) %>% 
                      add_column(archiveID = J[i]) # adding journal number
        
        total_data <- rbind(total_data, temp_var)
        
        cowsay::say("raw data from multiple journals appended", by="signbunny")
      }
    }
  }
  
  # removing brackets from column names in template
  total_data <- total_data %>% 
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
                                  "occurrence_data", "prev_proccessed")))>=1){
    
    # read in the file with the second latest date (with most observations) 
    old_data <- read.csv(here::here("data", "data_digitization","occurrence_data",
                                    "prev_proccessed",
                                    unique(as.character(max(list.files(
      here::here("data", "data_digitization","occurrence_data",
                 "prev_proccessed")))))))  
    
    # forcing columns from new and old dataframes to be read as 
    # the same classes so they can be joined
    
    # obtaining classes of raw data frame
    classes_total_data <- lapply(total_data,class)
    classes_total_data <- bind_rows(classes_total_data)
    classes_total_data <- unlist(classes_total_data)
    
    # applying to old_data frame
    old_data <- Map('class<-', old_data, classes_total_data) %>% bind_rows()
    
    # remove rows from current data table that contain old data                                              
    new_data <- total_data %>% anti_join(old_data)
    
    cowsay::say("previously proccessed rows removed :)", by="signbunny")
    
  } else {
    new_data <- total_data
    cowsay::say("all rows are new so they were kept :)", by="signbunny")
  }
  
## writing sheet of total columns reviewed/ processed
  
  # finding the copy number - starts at 1 if no other files on same date, appended to end of file name
  X <- length(which(grepl(Sys.Date(),list.files(here::here("data", "data_digitization",
                                   "occurrence_data", "prev_proccessed")), fixed=TRUE) ==T))+1
  
  write.csv(total_data, here::here("data", "data_digitization","occurrence_data",
                                 "prev_proccessed",
                                 paste0(AI,"-", "_rows-reviewed_",
                                        Sys.Date(),"_",X,".csv")), row.names=F)
  
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
                "2_data_checking",
                 paste0(AI,"-","occ-data-to-check_HIGH-PRIORITY_",
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
                           "2_data_checking",
                           paste0(AI,"-","occ-data-to-check_ALL_",
                           Sys.Date(),".csv")), row.names = F)
    } 
  
  # and removing old files to save storage
  if(length(list.files(here::here("data", 
                                  "data_digitization","occurrence_data",
                                  "2_data_checking")))>4){
    file.remove(unique(as.character(min(list.files(here::here("data", 
                                                              "data_digitization","occurrence_data",
                                                              "2_data_checking"))))))  
    cowsay::say("old files removed!", by="signbunny")
  }
  
  if(length(list.files(here::here("data", "data_digitization",
                                  "occurrence_data", "prev_proccessed")))>4){
    file.remove(unique(as.character(min(list.files(here::here("data", "data_digitization",
                                                              "occurrence_data", "prev_proccessed", 
                                                              ))))))  
    cowsay::say("old files removed!", by="signbunny")
  }
  
  
## 3. ADDRESS THE ISSUES IN THESE ROWS in CSV FILE ----
  
  # run this to open file at location: 
  utils::browseURL(here::here("data", 
                              "data_digitization","occurrence_data",
                              "2_data_checking"))
  
  # add data or change that data in these rows within the new .csv file created
  # once they are all addressed... use script part1-b_checking 
 

  