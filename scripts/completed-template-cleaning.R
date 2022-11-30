###########################################################
#######               Cleaning entered              ####### 
#######     occurrence data in data entry template  #######
#######                 Emma Menchions 22-11-20     #######                       
###########################################################

# this script will go through each column in order that it appears on the 
# HJ occurrence collection digitization template and perform a series
# of checks for data entry errors 

# Things to do: 
# maybe have data CHECKING component then data CLEANING component? (as seperate scripts?)
# pagenum: ADD FOR OTHER JOURNALS AS YOU GO 
# - column 2
# check that occurrenceID only occurs once 

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

J <-8 # journal number (1-31)
data <- read_excel(here::here("data","digitized_data", paste0("HJ-",J,"-","occ-entry-template_22-11-11.xlsx")))

places <- read.csv(here::here("data", "reference_data","islands-and-districts_22-11-22.csv"))

# checking classes of columns: 
  data %>% 
    chain_start %>%
    verify(., has_class("archiveID","pageNum", "numPage", "date", "vElevM","vLat", "vLon",
                     "vCoordUncM","numPlantsCode" , class="numeric")) %>% 
    verify(., has_class("vName","vSciName", "sciName", "conf","vTaxonRank", "occStatus",
                     "locality","country", "stateProvince", "county","islandGroup",
                     "island", "habitat", "vElevRef","vUTM", "orgQuantity",
                     "orgQtype", "occRemarks", "phenology", "recordedBy", "idBy", 
                     "assOcc", "assTaxa", "dataEntryRemarks", class="character")) %>% 
    chain_end 
  
# TASK 1: check for NA's in certain columns and data entry remarks!  ----
  
  
## Column 1: archiveID ----
# constraint: a number between 1 and 31 (corresponding to range of archives from HJ)

  data %>% 
    chain_start %>%
    assert(in_set(1:31), archiveID) %>% # checking if integer from 1 - 31
    assert(not_na, archiveID) %>%  # checking that all rows have archiveID
    chain_end 

## Column 2: pageNum
  # constraint: for HJ-8 journal = numeric between 1 and 208
  # for HJ-27 = numeric between 1 and 28 
  # for HJ-7 = numeric between 1 and 159
  # for HJ-9 = numeric between 1 and 206

  if(J==7){
    data %>% 
    assert(in_set(1:159),data$pageNum[i])
  }else if(J==8){
    data %>% 
    assert(in_set(1:208),data$pageNum[i])
  }else if(J==9){
    data %>% 
    assert(in_set(1:206),data$pageNum[i]) 
  }else if(J==27){
    data %>% 
    chain_start %>% 
    assert(in_set(1:28),data$pageNum[i]) 
  }

## Column 3: numPage----
  # constraint: between 1 and 100 (arbitrary threshold - unlikely to be more than 100 observaitons on page)
  data %>% 
    chain_start %>%
    assert(in_set(1:100), numPage) %>% # checking if integer from 1 - 31
    assert(not_na, numPage) %>%  # checking that all rows have number
    chain_end 
  
## Column 4: vName ----
  # constraint - must be character and only contain letters not numbers 
  # from https://stackoverflow.com/questions/43195519/check-if-string-contains-only-numbers-or-only-characters-r
  letters_only <- function(x) !grepl("^[^A-Za-z]+[[:space:]]+", x)
  
  data %>% 
    chain_start %>%
    assert(letters_only, vName) %>%  # %>% # checking if integer from 1 - 31
    assert(not_na, vName) %>%  # checking that all rows have number
    chain_end(error_fun = error_df_return)
  
## Column 5: vSciName ----
  genusCapitalized <- function(x) !grepl("^[[:upper:]]", x)
  
  data %>% 
    chain_start %>%
    assert(letters_only, vSciName) %>%  # %>% # checking if integer from 1 - 31
    assert(not_na, vSciName) %>%  # checking that all rows have number
    assert(genusCapitalized, vSciName) %>% 
    chain_end(error_fun = error_df_return)
  
## Column 6: conf ----
  confvalues <- c("l","m","h")
  
  data %>% 
    assert(in_set(confvalues),conf)
  
## Column 7: sciName ---- 
  data %>% 
    chain_start %>%
    assert(letters_only, sciName) %>%  # %>% # checking if integer from 1 - 31
    assert(not_na, sciName) %>%  # checking that all rows have value
    assert(genusCapitalized, sciName) %>% 
    chain_end(error_fun = error_df_return)
  
## Column 8: vTaxonRank ---- 
  ranks <- c("species", "genus", "family", "order", "class", "phylum", "kindgom")
         
  data %>% 
    chain_start %>% 
    assert(not_na,vTaxonRank) %>%  # checking that all rows have value
    assert(in_set(ranks),vTaxonRank) %>% 
    chain_end(error_fun = error_df_return)
  
  # COME BACK AND FIX THIS
  rank_match_name <- function(name, rank) {
  for (i in 1:length(rank)){
  if (length(strsplit(name, " ")[[1]])==1){
    if (rank == "genus" | rank =="family"){
      return(TRUE)
    } else if (rank != "genus" | rank !="family"){
      return(FALSE)
    }
  } else if (length(strsplit(name, " ")[[1]])==2){
    if (rank == "species"){
      return(TRUE)
    } else if (rank != "species"){
      return(FALSE)
    }
  }
  }
  }
  
 
  data[rank_match_name(data$vSciName, data$vTaxonRank)] 

  
## Column 9: occStatus ----
  occstat <- c("present","absent")
  data %>% 
    chain_start %>% 
    assert(not_na,occStatus) %>%  # checking that all rows have value
    assert(in_set(occstat),occStatus) %>% 
    chain_end(error_fun = error_df_return)
  
## Column 10: date ---- 
  # must be within dates of journals 
  data %>% 
    chain_start %>% 
    assert(not_na,date) %>%  # checking that all rows have value
    assert(within_bounds(19680101, 20210510), date) %>% 
    chain_end(error_fun = error_df_return)
  
## Column 11: locality ---- 
  # constraint: is read as character vector, every observation has it 
  data %>% 
    assert(not_na,locality)  # checking that all rows have value
  
## Column 12: country ---- 
  data %>% 
    chain_start %>% 
    assert(not_na,country) %>% 
    assert(in_set("Canada", "United States"),country) %>% 
    chain_end(error_fun = error_df_return)
  
## Column 13: stateProvince ----
  
  data %>% 
    chain_start %>% 
    assert(not_na,stateProvince) %>% 
    assert(in_set(c("British Columbia", "Washington")),stateProvince) %>% 
    chain_end(error_fun = error_df_return)
  
  
## Column 14: county ---- 
  data %>% 
    chain_start %>% 
    assert(not_na,county) %>% 
    assert(in_set(places$district),county) %>% 
    chain_end(error_fun = error_df_return)
  
## Column 15: islandGroup ---- 
  
  data %>% 
    chain_start %>% 
    assert(not_na,islandGroup) %>% 
    assert(in_set(places$islandGroup),islandGroup) %>% 
    chain_end(error_fun = error_df_return)

## Column 16: island ---- 
  data %>% 
    chain_start %>% 
    assert(not_na,island) %>% 
    assert(in_set(places$island),island) %>% 
    chain_end(error_fun = error_df_return)
  
## Column 17: habitat ---- 
  # constraint: read as character
  
## Column 18: vElevM ---- 
  # constraint: read as numeric 
  
## Column 19: vElevRef ---- 
  # constraint: read as character
  
## Column 20: locationRemarks ----
  # constraint: read as character
  
## Column 21, 22: vLat & vLon ---- 
  
  # loading function to convert to decimal degrees
  source(here::here("scripts","functions", "angle2dec.R")) 
  
  # converting degrees to decimal degrees
  for (i in 1:dim(data)[1]){
    if(length(tstrspli(data$vLat[i], " ")[[1]])>1){ # if data in dms degrees
      data$vLat[i] <- angle2dec(data$vLat[i]) # convert to decimal degrees
      data$vLon[i] <- angle2dec(data$vLon[i])
    }
  }
  
  data$vLon <- as.numeric(data$vLon) # converting to numeric so assert function works
  data$vLon <- as.numeric(data$vLon) 
  
  # check within certain range (drawn from arbitrary boundary box around 
  # South BC, North Washignton Area)
  
  data %>% 
    chain_start %>% 
    assert(within_bounds(47,51),vLat) %>% 
    assert(within_bounds(-129,-121),vLon) %>% 
    chain_end(error_fun = error_df_return)
  
## Column 23: vUTM ---- 
  # constraint: must be bounded by same limits as latitude 
  
  
  
## Column 24: vCoorUncM ---- 
  # constraint: must be numeric 
  
## Column 25: numPlantsCode ---- 
  # constraint: must be between 0 and 5 
  data %>% 
    assert(in_set(0:5),numPlantsCode)

## Column 26: orgQuantity ---- 
## Column 27: orgQtype ---- 
## Column 28: occRemarks ---- 
## Column 29: phenology ---- 
## Column 30: recordedBy ----
## Column 31: idBy ---- 
## Column 32: assOcc ---- 
## Column 33: assTaxa ---- 
## Column 34: dataEntryRemarks ---- 
  
## putting it all together 
  
#1) identify rows with missing info for vName, vSciName, sciName, and entries in data entry remarks
errors <- 

# check that all rows have crucial info

  # recordedby 
  #velev - only numeric (meters implied)
  # v elev reference as character sting
  # vlat as decimal or 3 segments - and within certain range that would match where he collected with some buffer though
  # vUTM - the right number of elements and with 10U and starting with 3 or 4 and 4 5 or 6 --> look at map to check this 
  # coord uncertainty - only numeric (meters implied)