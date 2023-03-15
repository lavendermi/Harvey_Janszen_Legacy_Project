#### Digitization & Processing Progress
## Created by: Emma Menchions
## Date: March 4/23
## Goal: 1) This script will check the raw data sheets in both..
##          data > data_digitization > occurrence_data > 1_raw_data AND
##          data > data_digitization > collection_data > 1_raw_data
##          Output: the number of pages digitized from each journal so far
##
##       2) It will check the data processing outputs in both...
##          data > data_digitization > occurrence_data > darwin_core_data AND
##          data > data_digitization > collection _data > darwin_core_data 
##          Output: number of pages digitized AND processed from each journal so far
##          
##       total output = a CSV file in the folder "digitization-progress" with the date
## 
## Run this script every time you want to update progress

## LOADING PACKAGES ----
library(groundhog)
set.groundhog.folder(here::here("packages"))
date <- "2022-11-02"

requiredPackages <-  c("tidyverse", "dplyr", "readxl", "stringr")

for (pkg in requiredPackages) {
  groundhog.library(pkg, date)
}

rm(requiredPackages)
  
## USER INPUTS----

  J <- c(5,7,8,9,27) # JOURNAL NUMBERS (vector)
  AI <- "HJ" # AUTHOR INITIALS 
  journalPages <- c(116,159,209,206,28) # vector of number of pages in each journal in same order as J 
  
## Digitization progress ----

    ## collection data
  
      coll_pages <- c() # initializing vector to store max page numbers
  
      # loop that loads data and reads the maximum page number entered for each journal
      for (i in 1:length(J)){
        coll_data <- read_excel(
        here::here("data","data_digitization",
                 "collection_data","1_raw_data",paste0(AI,"-",J[i],"-coll-entry.xlsx"))) %>% 
          dplyr::rename(pageNum="[pageNum]")
        
          # ensuring that pageNum is read as numeric
          coll_data$pageNum <- as.numeric(coll_data$pageNum)
          
        # if more than one page digitized...
        if (dim(coll_data)[1]>0){
          # assign as maximum page number for row entered
          coll_pages[i] <- length(unique(coll_data$pageNum)) 
        }else{
          # otherwise, assign as 0 pages digitized
          coll_pages[i] <- 0
        
        } 
      }
  
    ## occurrence data
    
    occ_pages <- c() # initializing vector to store max page numbers
    
    # loop that loads data and reads the maximum page number entered for each journal
    for (i in 1:length(J)){
      occ_data <- read_excel(
        here::here("data","data_digitization",
                   "occurrence_data","1_raw_data",paste0(AI,"-",J[i],"-occ-entry.xlsx"))) %>% 
        dplyr::rename(pageNum="[pageNum]")
      
        # ensuring that pageNum is read as numeric
        occ_data$pageNum <- as.numeric(occ_data$pageNum)
        
      # if more than one page digitized...
      if (dim(occ_data)[1]>0){
        # assign as maximum page number for row entered
        occ_pages[i] <- length(unique(occ_data$pageNum)) 
      }else{
        # otherwise, assign as 0 pages digitized
        occ_pages[i] <- 0
        
      } 
    }
    
  ## calculating total progress as % of total pages of each journal
    coll_dig_progress <- round((coll_pages/journalPages)*100, 1)
    occ_dig_progress<- round((occ_pages/journalPages)*100, 1)
    
## Processing progress ----

    ## collection data
    coll_data <- read.csv(
      here::here("data","data_digitization",
                 "collection_data","darwin_core_data", unique(as.character(max(list.files(  
                   here::here("data","data_digitization",
                              "collection_data", 
                              "darwin_core_data")))))))
    
    coll_proc_pages <- vector("list", length(J)) # initializing storage list
    
    if (dim(coll_data)[1]>0){
    for (i in 1:dim(coll_data)[1]){ # for each row 
      for (k in 1:length(J)){ # and for each journal number
        # if when you split up the record number into its components and the first component
        # matches with the journal number/name...
        if(str_split(coll_data$recordNum[i], pattern="-")[[1]][1]==paste0(AI,J[k])){
          # then assign the 2nd element (pageNum) from that string split
          coll_proc_pages[[k]][i] <- str_split(coll_data$recordNum[i], pattern="-")[[1]][2]
        }else{
          coll_proc_pages[[k]][i] <- 0
        }
      }
    }
    }else{
      coll_proc_pages <- rep(0,length(J))
    }
    
    coll_proc <- c() # initializing storage vector
    for (i in 1:length(coll_proc_pages)){ # for each journal in the list
      
      # assign the number of unique pages for each journal processed
      coll_proc[i] <- length(unique(as.numeric(coll_proc_pages[[i]][which(as.numeric(coll_proc_pages[[i]])>0)])))
    }
    
  ## occurrence data
    
    # loading data
    occ_data <- read.csv(
    here::here("data","data_digitization",
             "occurrence_data","darwin_core_data", unique(as.character(max(list.files(  
               here::here("data","data_digitization",
                          "occurrence_data", 
                          "darwin_core_data")))))))
    
    occ_proc_pages <- vector("list", length(J)) # initalizing storage list
    
    # loop 
    if (dim(occ_data)[1]>0){
    for (i in 1:dim(occ_data)[1]){ # for each row 
      for (k in 1:length(J)){ # and for each journal number
        # if when you split up the record number into its components and the first component
        # matches with the journal number/name...
        if(str_split(occ_data$recordNum[i], pattern="-")[[1]][1]==paste0(AI,J[k])){
          # then assign the 2nd element (pageNum) from that string split
          occ_proc_pages[[k]][i] <- str_split(occ_data$recordNum[i], pattern="-")[[1]][2]
        }else{
          occ_proc_pages[[k]][i] <- 0
        }
      }
    }
    }else{
      occ_proc_pages <- rep(0, length(J))
    }
      
    
    occ_proc <- c() # initializing storage vector
    for (i in 1:length(occ_proc_pages)){ # for each journal in the list
    
      # assign the number of unique pages for each journal processed
      occ_proc[i] <- length(unique(as.numeric(occ_proc_pages[[i]][which(as.numeric(occ_proc_pages[[i]])>0)])))
    }
      
    
    ## calculating total progress as % of total pages of each journal
    coll_proc_progress <- round((coll_proc/journalPages)*100, 1)
    occ_proc_progress<- round((occ_proc/journalPages)*100, 1)
    
    
##  calculating total between collection and occurrence data ----
    
    ## digitization progress
    #initalizing vectors 
    total_dig_pages <- c()
    total_dig_progress <- c()
    
    for (i in 1:length(J)){
    total_dig_pages[i] <- occ_pages[i] + coll_pages[i]
    total_dig_progress[i] <-  (occ_pages[i] + coll_pages[i])/(2*journalPages[i])*100
    
    }
    
    ## digitization progress
    #initalizing vectors 
    total_proc_pages <- c()
    total_proc_progress <- c()
    
    for (i in 1:length(J)){
      total_proc_pages[i] <- occ_proc[i] + coll_proc[i]
      total_proc_progress[i] <-  (occ_proc[i] + coll_proc[i])/(2*journalPages[i])*100
      
    }
    
    

## Constructing a data table to store digitization progress summary ----
    
    summary <- data.frame(J, journalPages, coll_pages, coll_dig_progress, occ_dig_progress, occ_pages, 
                          coll_proc, coll_proc_progress, occ_proc, occ_proc_progress, total_dig_pages,
                          total_dig_progress, total_proc_pages, total_proc_progress) %>% 
      dplyr::rename(journalNumber = J, coll_digitized_pages = coll_pages, coll_digitization_perc = coll_dig_progress,
                    occ_digitization_perc= occ_dig_progress, occ_digitized_pages = occ_pages, 
                    coll_processed_pages=coll_proc, coll_processed_perc = coll_proc_progress, 
                    occ_processed_pages=occ_proc, occ_processed_perc=occ_proc_progress, 
                    total_digitized_pages = total_dig_pages,
                    total_digitization_perc = total_dig_progress,
                    total_processed_pages = total_proc_pages,
                    total_processed_perc = total_proc_progress)
      
    ## witing csv file 
    write.csv(summary,here::here("digitization-progress", paste0("digitization-progress_",Sys.Date(),".csv")))
  
