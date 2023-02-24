###########################################################
#######               Cleaning entered              ####### 
#######     occurrence data in data entry template  #######
#######                 Emma Menchions 22-11-20     #######                       
###########################################################

## OVERVIEW ----
# this script will check for inconspicuous errors in data entry
# such as missing values, incorrect capitalization for species names
# incorrect values for page and archive numbers
# and many other checks

# it uses the assert R package to check whether these factors are true

# these code chunks using assert() or verify() will spit out an error message
# in the console if the data does not pass the check 

# these errors must be manually fixed and saved in excel:

# once they are fixed, the script should be run again until the point that it last stopped

# ensure that it now passes that point and continues

# repeat for all error messages

# if no error message appears, the test has been passed 

## LOADING PACKAGES ----
library(groundhog)

set.groundhog.folder(here::here("packages"))
date <- "2022-11-02"
requiredPackages <-  c("assertr","dplyr","here", "terra","tidyverse",
                       "tidyr", "cowsay")

for (pkg in requiredPackages) {
  groundhog.library(pkg, date)
}

rm(requiredPackages)

## USER INPUT ----

J <- c(5,7,8,9,27) # JOURNAL NUMBERS 
AI <- "HJ" # AUTHOR INITIALS 

## READING IN DATA ----

  # **use this code chunk to retun to the data and edit errors
  # doesn't need to be run on first time around **
  utils::browseURL(here::here("data", 
                            "data_digitization","occurrence_data",
                            "3_data_cleaning"))

  # reading in most recent checked data
  data <- read.csv(
    here::here("data","data_digitization",
    "occurrence_data","3_data_cleaning",unique(
     as.character(max(list.files(here::here("data","data_digitization",
     "occurrence_data",
     "3_data_cleaning")))))))
  
  # loading places metadata
  places <- read.csv(here::here("data", 
                                "reference_data",
                                "islands-and-districts_22-11-29.csv"))
  
# TASK 1: removing repeated taxon observations for given sampling location and time ----
  
  data <- data %>% 
    group_by(sciName, date, locality, habitat, 
             locationRemarks) %>% 
      distinct(.keep_all =T)
  
## TASK 2: checking individual columns for constraints ----
  
## pageNum
  # constraints: 1) for HJ-8 journal = numeric between 1 and 208
  # for HJ-27 = numeric between 1 and 28 
  # for HJ-7 = numeric between 1 and 159
  # for HJ-9 = numeric between 1 and 206
  # for HJ-5 = numeric between 1 and 116
  # 2) read as an integer
  # 3) all rows must have entry

  if(J==7){
    data %>% 
    group_by(pageNum) %>% 
    chain_start %>% 
    verify(., has_class("pageNum", class="integer")) %>% 
    assert(., in_set(1:159), pageNum) %>% 
    chain_end 
    
  }else if(J==8){
    data %>% 
    group_by(pageNum) %>% 
    chain_start %>% 
    verify(., has_class("pageNum", class="integer")) %>% 
    assert(in_set(1:208),pageNum) %>% 
    chain_end 
    
  }else if(J==9){
    data %>% 
    group_by(pageNum) %>% 
    chain_start %>%
    verify(., has_class("pageNum", class="integer")) %>% 
    assert(in_set(1:206),pageNum)  %>% 
    chain_end 
    
  }else if(J==27){
    data %>% 
    group_by(pageNum) %>% 
    chain_start %>% 
    verify(., has_class("pageNum", class="integer")) %>% 
    assert(in_set(1:28),pageNum) %>% 
    chain_end 
    
  }else if(J==5){
    data %>% 
      group_by(pageNum) %>% 
      chain_start %>% 
      verify(., has_class("pageNum", class="integer")) %>% 
      assert(in_set(1:116),pageNum) %>% 
      chain_end 
  }

## numPage 
  # constraints: 1) between 1 and 100 (arbitrary threshold - 
  # unlikely to be more than 100 observaitons on page)
  # 2) read as an integer
  # 3) all rows must have entry
  
  data %>% 
    chain_start %>%
    group_by(numPage) %>% 
    verify(., has_class("numPage", class="integer")) %>% 
    assert(in_set(1:100), numPage) %>% # checking if integer from 1 - 31
    assert(not_na, numPage) %>%  # checking that all rows have number
    chain_end 
  
## vName 
  # constraints: 1) must be character and only contain letters not numbers 
  # 2) every row has one 
  
  # This function converts all first letters of name to capital 
  data$vName<-sub("(.)", "\\U\\1",data$vName,perl=TRUE)
  
  # from https://stackoverflow.com/questions/43195519/check-if-string-contains-only-numbers-or-only-characters-r
  letters_only <- function(x) !grepl("^[^A-Za-z]+[[:space:]]+", x)
  
  data %>% 
    group_by(vName) %>% 
    chain_start %>%
    assert(letters_only, vName) %>%  # checking only letters from A to Z
    assert(not_na, vName) %>%  # checking that all rows have number
    chain_end
  
## vSciName 
  ## constraints: 1) letters only 
  # 2) every row has one
  # 3) genus name capitalized
  # 4) specific epithet not capitalized 
  # 5) column read as character 
  
  # This function converts all first letters of name to capital 
  data$vSciName<-sub("(.)", "\\U\\1",data$vSciName,perl=TRUE)
  
  # functions that can check if the genus name is capitalized 
  # and specific epithet not capitalized
  genusCapitalized <- function(x) grepl("^[[:upper:]]", x)
  specEpiphetNotCap <- function(x) !grepl("^[[:upper:]]", str_split(x, pattern="n")[[1]][2])
  
  data %>% 
    chain_start %>%
    group_by(vSciName) %>% 
    verify(., has_class("vSciName", class="character")) %>% 
    assert(letters_only, vSciName) %>% # checking only letters from A to Z
    assert(not_na, vSciName) %>%  # checking that all rows have number
    assert(genusCapitalized, vSciName) %>% 
    assert(specEpiphetNotCap, vSciName) %>% 
    chain_end
  
  
## conf 
  # constraints: 1) in the set "l","m","h" 
  # 2) read as character
  # 3) every row has one 
  
  confvalues <- c("l","m","h")
  
  data %>% 
    group_by(conf) %>% 
    chain_start %>% 
    verify(., has_class("vSciName", class="character")) %>% 
    assert(in_set(confvalues),conf) %>%  # checking that in set
    assert(not_na, vSciName) %>%  # checking that all rows have number
    chain_end
  
## sciName 
  ## constraints: 1) letters only 
  # 2) every row has one
  # 3) genus name capitalized
  # 4) specific epithet not capitalized 
  # 5) read as character 
  
  # This function converts all first letters of name to capital 
  data$sciName<-sub("(.)", "\\U\\1",data$sciName,perl=TRUE)
  
  data %>% 
    group_by(sciName) %>% 
    chain_start %>%
    verify(., has_class("sciName", class="character")) %>% 
    assert(letters_only, sciName) %>%  # checking all letters 
    assert(not_na, sciName) %>%  # checking that all rows have value
    assert(genusCapitalized, sciName) %>% # genus capitalized? 
    assert(specEpiphetNotCap, vSciName) %>% # species not capitalized? 
    chain_end
  
## date 
  # constraints: 1) must be within dates of journals 
  # 2) read as integer 
  # 3) every row must have one 
  data %>% 
    group_by(date) %>% 
    chain_start %>% 
    verify(., has_class("date", class="integer")) %>% 
    assert(not_na,date) %>%  # checking that all rows have value
    assert(within_bounds(19680101, 20210510), date) %>% 
    chain_end
  
## locality  
  # constraints: 1) every observation has one
  # 2) column read as character 
  # 3) every row has one 
  
  data %>% 
    group_by(locality) %>% 
    chain_start %>% 
    verify(., has_class("locality", class="character")) %>% 
    assert(not_na,locality) %>%  # checking that all rows have value
    chain_end
  
## country 
  # constraints: 1) Canada or USA
  # 2) column read as character
  # 3) every row has one 

  data %>% 
    group_by(country) %>% 
    chain_start %>% 
    verify(., has_class("country", class="character")) %>% 
    assert(not_na,country) %>% # every obs has one
    assert(in_set("Canada", "United States"),country) %>% 
    chain_end
  
## stateProvince 
  # constraints: 1) either BC or Washington
  # 2) every row has one 
  # 3) read as character
  data %>% 
    group_by(stateProvince) %>% 
    chain_start %>% 
    verify(., has_class("stateProvince", class="character")) %>% 
    assert(not_na,stateProvince) %>% # every obs has one
    assert(in_set(c("British Columbia", "Washington")),stateProvince) %>% 
    chain_end
  
## island 
  # constraints: 1) if it is on an island
  # it is on one of the islands in the Norther, Southern
  # Gulf Islands, San Juans, Discovery Islands, Howe Sound, or 
  # sunshine coast (or Vancouver island) 
  # not every obs has to have it 
  # 2) read as character
  
  # if has non-NA elements...
  if(length(data$island[is.na(data$island)])
     <length(data$island)){
    data %>% 
      group_by(island) %>% 
      chain_start %>% 
      #column is read as character 
      verify(., has_class("island", class="character")) %>% 
      assert(in_set(places$island),island) %>% 
      chain_end
    
  }
  
## idQualifier
  # if has non-NA elements...
  if(length(data$idQualifier[is.na(data$idQualifier)])
     <length(data$idQualifier)){
    data %>% 
      #column is read as character 
      verify(., has_class("idQualifier", class="character"))
  }
  
## county 
  # constraints: 1) if listed, it should be 
  # listed as a county in the list of places metadata
  # and spelt correctly 
  # not every observation has to have one at this point, 
  # this will later be assigned based on the "island" field
  # 2) read as character
  
  # if has non-NA elements...
  if(length(data$county[is.na(data$county)])
     <length(data$county)){
    data %>% 
      group_by(county) %>% 
      chain_start %>% 
      #column is read as character 
      verify(., has_class("county", class="character")) %>% 
      assert(in_set(places$district),county) %>% 
      chain_end
  }
  
## occStatus 
  # constraints: 1) either "absent" or "present
  # not every row has to have one yet, will be assigned later
  # 2) read as character
  
  occstat <- c("present","absent")
  
  # if there are non-Na elements...
  if(length(data$occStatus[is.na(data$occStatus)])
     <length(data$occStatus)){
    # check that... 
    data %>%
      group_by(occStatus) %>% 
      chain_start %>% 
      #column is read as character 
      verify(., has_class("occStatus", class="character")) %>% 
      # characters are within set 
      assert(in_set(occstat),occStatus)  %>% 
      chain_end
  }
    
## habitat 
  # constraint: 1) read as character
  
  # if has non-NA elements...
  if(length(data$habitat[is.na(data$habitat)])
     <length(data$habitat)){
      data %>% 
      group_by(habitat) %>% 
      verify(., has_class("habitat", class="character"))
    }
  
## locationRemarks 
  # constraint: 1) read as character
  
  # if has non-NA elements...
  if(length(data$locationRemarks[is.na(data$locationRemarks)])
     <length(data$locationRemarks)){
    data %>% 
      group_by(locationRemarks) %>% 
      # is column is read as character 
      verify(., has_class("locationRemarks", class="character"))
  }
    
## vTaxonRank 
  # constraints: 1) in set of ranks
  # 2) read as character
  
  ranks <- c("species", "genus", "family", "order", 
             "class", "phylum", "kindgom")
  
  # if there are non-Na elements...
  if(length(data$vTaxonRank[is.na(data$vTaxonRank)]) < 
     length(data$vTaxonRank)){
    # check that... 
    data %>% 
      group_by(vTaxonRank) %>% 
      chain_start %>% 
      #column is read as character 
      verify(., has_class("vTaxonRank", class="character")) %>% 
      # characters are within set 
      assert(in_set(ranks),vTaxonRank)  %>% 
      chain_end
  }

## vLat & vLon 
  # constraint: 1) within rough bounding box
  # of where HJ collected from 
  
  # if there are non-Na elements...
  if(length(data$vLat[is.na(data$vLat)])<length(data$vLat)){
    
    # loading function to convert to decimal degrees
    source(here::here("scripts","functions", "angle2dec.R")) 
    
      # converting degrees to decimal degrees
      for (i in 1:dim(data)[1]){
        if(!is.na(data$vLat[i]) & !is.na(data$vLon[i])){
          if(length(strsplit(data$vLat[i], " ")[[1]])>1){ # if data in dms degrees
            data$vLat[i] <- angle2dec(data$vLat[i]) # convert to decimal degrees
            data$vLon[i] <- angle2dec(data$vLon[i])
          }
        }
      }
  
    data$vLat <- as.numeric(data$vLat) # converting to numeric so assert function works
    data$vLon <- as.numeric(data$vLon)*-1
    
  # check within certain range (drawn from arbitrary
  # boundary box around 
  # South BC, North Washignton Area)
  
  data %>% 
    group_by(vLat) %>% 
    assert(within_bounds(47,51,allow.na=T),vLat)
  data %>% 
    group_by(vLon) %>% 
    assert(within_bounds(-129,-121,allow.na=T),vLon)
  } 
  
## vUTM 
  # constraint: 1) must be bounded by same limits as latitude and longitude
 
  # if there are non-Na elements...
  if(length(data$vUTM[is.na(data$vUTM)])<length(data$vUTM)){
    
    projection <- "+proj=utm +zone=10 +datum=WGS84  +units=m" # assumed 
   
    UTM <- data %>% separate(vUTM, c("zone","x","y"), " ") # seperating UTM into x, and y components
    points <- cbind(as.numeric(UTM$x), as.numeric(UTM$y)) # making dataframe with x y components
    
    # assert that UTM coordinates have right number of digits
    six_digits <- function(x) if(!is.na(x) & x !=6) return(FALSE) # custom predicate
    right_zone <- function(x) if(!is.na(x) & x !="10U") return(FALSE) # custom predicate
    
      UTM %>% 
        chain_start %>% 
        assert(right_zone, zone) %>% 
        assert(six_digits,x) %>% 
        assert(six_digits,y)
        
    # convert UTM coordinates into decimal degrees
    v <- terra::vect(points, crs=projection) # making spatial points data frame
    z <- terra::project(v, "+proj=longlat +datum=WGS84")  # projecting points using assigned crs  
    lonlat <- as.data.frame((geom(z)[, c("x", "y")])) # extracting lat lon from spatial points frame
    
    # assigning lat and lon from UTM to these rows 
    data$lat <- as.numeric(lonlat$y)
    data$lon <- as.numeric(lonlat$x) 
   
    # check within certain range (drawn from arbitrary
    # boundary box around 
    # South BC, North Washington Area)
    
    data %>% 
      group_by(lat) %>% 
      assert(within_bounds(47,51, allow.na=T),lat)
      
    data %>% 
      group_by(lon) %>% 
      assert(within_bounds(-129,-121,allow.na=T),lon)  
      
      # removing these columns - conversion will be re-done in next script (part 3)
      # where columns are added/ altered to keep things consistent 
      data <- data %>% select(-c("lat", "lon"))
    
  }
  
## assColl
  # constraints: 1) if assCollTaxa is non-Na, this column should be non-NA
  # 2) read as character
  paired_col_not_empty <- function(x) if(x==" ") return(FALSE)
  
  # if there are non-Na elements...
  if(length(data$assColl[is.na(data$assColl)])
     <length(data$assColl)){
    
      # checking class 
      data %>% 
        verify(., has_class("assColl", class="character"))
    
      # checking that all rows with data entered in assCollTaxa have 
      # something enetered in the assCollTaxa column
    for (i in 1:dim(data)[1]){
        if (is.na(data$assCollTaxa[i]) & is.na(data$assColl[i])){
          match <- T 
        } else if (!is.na(data$assCollTaxa[i]) & !is.na(data$assColl[i])){
          match <- T
        } else if (!is.na(data$assCollTaxa[i]) & is.na(data$assColl[i])){
          match <- F 
        } else if (is.na(data$assCollTaxa[i]) & !is.na(data$assColl[i])){
          match <- F 
        }
    }
  } 
      
  
## assCollTaxa 
  # constraints: 1) if assCollOcc is non-Na, this column should be non-NA
  # 2) all genus names capitalized
  # 3) all specific epithets not capitalized
  # 4) read as character
    
  # first make separate data frame where each row is an associated species name with associated index ID
  # it was originally attached to 
  
  # creating index id for the original dataframe 
  data$index <- 1:dim(data)[1]
  
  namesFrame <- data %>% 
    select(assCollTaxa, index) %>% 
  separate_rows(assCollTaxa,sep=", ")
  
  genusCapitalized <- function(x) if(!is.na(x)){grepl("^[[:upper:]]", x)}
  
  # if there are non-Na elements...
  if(length(namesFrame$assCollTaxa[is.na(namesFrame$assCollTaxa)])<length(namesFrame$assCollTaxa)){
  namesFrame %>% 
    chain_start %>%
    assert(letters_only, assCollTaxa) %>% 
    assert(genusCapitalized, assCollTaxa)
  }
  
## numPlantsCode 
  # constraint: 1) must be between 0 and 5 
  # 2) read as integer
  
  # if there are non-Na elements...
  if(length(data$numPlantsCode[is.na(data$numPlantsCode)])<length(data$numPlantsCode)){
  data %>% 
    group_by(numPlantsCode) %>% 
    dplyr::select("numPlantsCode") %>% 
    assert(in_set(0:5, allow.na=T),numPlantsCode)
  }

  
## orgQuantityType
  # if has non-NA elements...
  if(length(data$orgQtype[is.na(data$orgQtype)])
     <length(data$orgQtype)){
    data %>% 
      #column is read as character 
      verify(., has_class("orgQtype", class="character"))
  }
  
## phenology
  # if has non-NA elements...
  if(length(data$phenology[is.na(data$phenology)])
     <length(data$phenology)){
    data %>% 
      #column is read as character 
      verify(., has_class("phenology", class="character"))
  }
  
## recordedBy
  # if has non-NA elements...
  if(length(data$recordedBy[is.na(data$recordedBy)])
     <length(data$recordedBy)){
    data %>% 
      #column is read as character 
      verify(., has_class("recordedBy", class="character"))
  }
  
## idBy
  # if has non-NA elements...
  if(length(data$idBy[is.na(data$idBy)])
     <length(data$idBy)){
    data %>% 
      #column is read as character 
      verify(., has_class("idBy", class="character"))
  }
  
## Writing cleaned sheet ---
  

write.csv(data_cleaned, here::here("data","data_digitization",
                                   "occurrence_data",
                                   "4_clean_data",
                                   paste0(AI, "_clean-occurrences_",
                                          Sys.Date(),".csv")), row.names = F)
# and removing old files to save storage
if(length(list.files(here::here("data", 
                                "data_digitization","occurrence_data",
                                "4_clean_data")))>2){
  file.remove(unique(as.character(min(list.files(here::here("data", 
                                                            "data_digitization","occurrence_data",
                                                            "4_clean_data"))))))  
  cowsay::say("old files removed!", by="signbunny")
}



