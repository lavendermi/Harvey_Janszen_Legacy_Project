###########################################################
#######         Checking & cleaning entered         ####### 
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
# fix reading classes of columns 
# check that occurrenceID only occurs once 

#landings_data[!complete.cases(landings_data),]

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

data <- read_excel(here::here("data","digitized_data","HJ-occ-entry-template_22-11-11.xlsx"))
places <- read.csv(here::here("data", "islands-and-districts_22-11-22.csv"))

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

  for (i in 1:dim(data)[1]){
      if (data$archiveID[i] ==7){
      assert(in_set(1:159),data$pageNum[i]) 
      } else if (data$archiveID[i] == 8){
        assert(in_set(1:208),data$pageNum[i]) 
      } else if (data$archiveID[i] == 9){
        assert(in_set(1:206),data$pageNum[i]) 
      } else if (data$archiveID[i] == 27){
        assert(in_set(1:28),data$pageNum[i]) 
      }
  }

## Column 3: numPage----
## Column 4: vName ----
## Column 5: vSciName ----
## Column 6: conf ----
  
  confvalues <- c("l","m","h")
  
  data %>% 
    assert(in_set(confvalues),conf)
  
## Column 7: sciName ---- 
## Column 8: vTaxonRank ---- 
  
  
## Column 9: occStatus ----
  occstat <- c("present","absent")
  
  data %>% 
    assert(in_set(occstat),occStatus)
  
## Column 10: date ---- 
## Column 11: locality ---- 
## Column 12: country ---- 
  possCountries <- c("Canada", "United States")
  
  data %>% 
    assert(in_set(possCountries),country)
  
## Column 13: stateProvince ----
  possProvince <- c("British Columbia", "Washington")
  
  data %>% 
    assert(in_set(possProvince),stateProvince)
  
## Column 14: county ---- 
  data %>% 
    assert(in_set(places$district),county)
  
## Column 15: islandGroup ---- 
  
  data %>% 
    assert(in_set(places$islandGroup),islandGroup)

## Column 16: island ---- 
  data %>% 
    assert(in_set(places$island),island)
  
## Column 17: habitat ---- 
## Column 18: vElevM ---- 
## Column 19: vElevRef ---- 
## Column 20: locationRemarks ---- 
## Column 21: vLat ---- 
## Column 22: vLon ---- 
## Column 23: vUTM ---- 
## Column 24: vCoorUncM ---- 
## Column 25: numPlantsCode ---- 
## Column 26: orgQuantity ---- 
## Column 27: orgQtype ---- 
## Column 28: occRemarks ---- 
## Column 29: phenology ---- 
## Column 30: recordedBy ----
## Column 31: idBy ---- 
## Column 32: assOcc ---- 
## Column 33: assTaxa ---- 
## Column 34: dataEntryRemarks ---- 
  

#1) identify rows with missing info for vName, vSciName, sciName, and entries in data entry remarks

# check that UTM has right number of digits 

# check that all rows have crucial info
  # vTaxonRank - from acceptable set of strings - and if name only 1 word then genus etc 
  # occStatus - either present or absent
  # date - within certain date range and numeric
  # locality
  # coutnry - check if spelled properly
  # stateProvince - check if spelled properly
  # county - check if spelled properly
  # recordedby 
  #velev - only numeric (meters implied)
  # v elev reference as character sting
  # vlat as decimal or 3 segments - and within certain range that would match where he collected with some buffer though
  # vUTM - the right number of elements and with 10U and starting with 3 or 4 and 4 5 or 6 --> look at map to check this 
  # coord uncertainty - only numeric (meters implied)