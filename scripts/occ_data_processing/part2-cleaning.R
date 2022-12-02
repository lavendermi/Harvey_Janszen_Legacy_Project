###########################################################
#######               Cleaning entered              ####### 
#######     occurrence data in data entry template  #######
#######                 Emma Menchions 22-11-20     #######                       
###########################################################

# this script will go through each column in order that it 
# appears on the HJ occurrence collection digitization 
# template and perform a series of checks for data entry errors 
# or will it go one book at a time? 


## Things to do: 
# change ordering of qualifiers
# check that species only occurs once at certain event! - if not then randomly remove one from dataset 
# add check to see specific epiphet lower case in vsciname, sciname, assCollTaxa 
#pagenum: ADD FOR OTHER JOURNALS AS YOU GO 
# - column 2
# check that occurrenceID only occurs once 
# should it go through one book at a time or all? 



## LOADING PACKAGES ----
library(groundhog)

date <- "2022-11-02"
requiredPackages <-  c("assertr","dplyr","here", "tidyverse","tidyr")

for (pkg in requiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {groundhog.library(pkg, date)}
}
rm(requiredPackages)

## READING IN DATA ----

  # loading data that has passed the first processing step 

  J <-7 # journal number (USER INPUT)

  # ENTER FILE NAME (USER INPUT)
  filename <- "HJ7-processed-step-1_2022-12-01.csv" 
  
  data <- read.csv(here::here("data","digitized_data",
                                "occurrence_data",
                                "data_cleaning", 
                                filename))
  
  # loading places metadata
  places <- read.csv(here::here("data", 
                                "reference_data",
                                "islands-and-districts_22-11-29.csv"))

  # checking classes of columns: 
  classes <- data %>% 
    chain_start %>%
    verify(., has_class("archiveID","pageNum", "numPage", "date", "vElevM","vLat", "vLon",
                     "vCoordUncM","numPlantsCode" , class="numeric")) %>% 
    verify(., has_class("vName","vSciName", "sciName", "conf","vTaxonRank", "occStatus",
                     "locality","country", "stateProvince", "county","islandGroup",
                     "island", "habitat", "vElevRef","vUTM", "orgQuantity",
                     "orgQtype", "occRemarks", "phenology", "recordedBy", "idBy", 
                     "assColl", "assCollTaxa", "dataEntryRemarks", class="character")) %>% 
    chain_end(error_fun = error_df_return)
  
# TASK 1: checking for repeated taxon observations for given 
# sampling location and time ----
  
  
## TASK 2: checking individual columns for constraints ----
  
## archiveID 
  # constraint: a number between 1 and 31 (corresponding to range of archives from HJ)

  data %>% 
    chain_start %>%
    assert(in_set(1:31), archiveID) %>% # checking if integer from 1 - 31
    assert(not_na, archiveID) %>%  # checking that all rows have archiveID
    chain_end 

## pageNum
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

## numPage 
  # constraint: between 1 and 100 (arbitrary threshold - unlikely to be more than 100 observaitons on page)
  data %>% 
    chain_start %>%
    assert(in_set(1:100), numPage) %>% # checking if integer from 1 - 31
    assert(not_na, numPage) %>%  # checking that all rows have number
    chain_end 
  
## vName 
  # constraint - must be character and only contain letters not numbers 
  # from https://stackoverflow.com/questions/43195519/check-if-string-contains-only-numbers-or-only-characters-r
  letters_only <- function(x) !grepl("^[^A-Za-z]+[[:space:]]+", x)
  
  data %>% 
    chain_start %>%
    assert(letters_only, vName) %>%  # %>% # checking if integer from 1 - 31
    assert(not_na, vName) %>%  # checking that all rows have number
    chain_end
  
## vSciName 
  
  # function that can check if the genus name is capitalized 
  # will be deployed also in sciName column section 
  genusCapitalized <- function(x) grepl("^[[:upper:]]", x)
  
  data %>% 
    chain_start %>%
    assert(letters_only, vSciName) %>%  # %>% # checking if integer from 1 - 31
    assert(not_na, vSciName) %>%  # checking that all rows have number
    assert(genusCapitalized, vSciName) %>% 
    chain_end
  
## conf 
  confvalues <- c("l","m","h")
  
  data %>% 
    assert(in_set(confvalues),conf)
  
## sciName 
  data %>% 
    chain_start %>%
    assert(letters_only, sciName) %>%  # %>% # checking if integer from 1 - 31
    assert(not_na, sciName) %>%  # checking that all rows have value
    assert(genusCapitalized, sciName) %>% 
    chain_end
  
## date 
  # must be within dates of journals 
  data %>% 
    chain_start %>% 
    assert(not_na,date) %>%  # checking that all rows have value
    assert(within_bounds(19680101, 20210510), date) %>% 
    chain_end
  
## locality  
  # constraint: is read as character vector, every observation has it 
  data %>% 
    assert(not_na,locality)  # checking that all rows have value
  
## country 
  data %>% 
    chain_start %>% 
    assert(not_na,country) %>% 
    assert(in_set("Canada", "United States"),country) %>% 
    chain_end
  
## stateProvince 
  
  data %>% 
    chain_start %>% 
    assert(not_na,stateProvince) %>% 
    assert(in_set(c("British Columbia", "Washington")),stateProvince) %>% 
    chain_end
## island 
  data %>% 
    chain_start %>% 
    assert(not_na,island) %>% 
    assert(in_set(places$island),island) %>% 
    chain_end
  
## county 
  data %>% 
    chain_start %>% 
    assert(in_set(places$district),county) %>% 
    chain_end

## occStatus 
  occstat <- c("present","absent")
  data %>% 
    chain_start %>% 
    assert(in_set(occstat),occStatus) %>% 
    chain_end

## habitat 
  # constraint: read as character

## locationRemarks 
  # constraint: read as character
  
## vTaxonRank 
  ranks <- c("species", "genus", "family", "order", "class", "phylum", "kindgom")
  
  data %>% 
    chain_start %>% 
    assert(in_set(ranks),vTaxonRank) %>% # checking that rows that 
    # have a value assigned is within these categories
    chain_end

## vElevM 
  # constraint: read as numeric 

  
## vLat & vLon 
  
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
  
## vUTM 
  # constraint: must be bounded by same limits as latitude 
  
  
  
## vCoorUncM  
  # constraint: must be numeric 
  
## assCollOcc 
  
## assCollTaxa 
  # all genus names in 
  data %>% 
    dplyr::select(assCollTaxa) %>% 
    na.omit() %>% 
    chain_start %>%
    assert(letters_only, assCollTaxa) 
    assert(genusCapitalized, assCollTaxa)
    chain_end
  
## numPlantsCode 
  # constraint: must be between 0 and 5 
  data %>% 
    assert(in_set(0:5),numPlantsCode)

## orgQuantity 
## orgQtype
## occRemarks 
## phenology
## recordedBy 
## idBy 
## dataEntryRemarks 
  
## TASK 3? putting it all together 
  
#1) identify rows with missing info for vName, vSciName, sciName, and entries in data entry remarks
errors <- 

# check that all rows have crucial info

  # recordedby 
  #velev - only numeric (meters implied)
  # v elev reference as character sting
  # vlat as decimal or 3 segments - and within certain range that would match where he collected with some buffer though
  # vUTM - the right number of elements and with 10U and starting with 3 or 4 and 4 5 or 6 --> look at map to check this 
  # coord uncertainty - only numeric (meters implied)