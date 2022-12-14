###########################################################
#######               Cleaning entered              ####### 
#######     occurrence data in data entry template  #######
#######                 Emma Menchions 22-11-20     #######                       
###########################################################

# this script will go through each column and perform a 
# series of checks for data entry errors for one journal 
# at a time 

## Things to do: 

# add check to see specific epihet lower case in assCollTaxa 
# function for UTM conversion that works for both part 2 and 3? 
# only limit islands to "Galiano Island" must have the term island for georeferencing 

#pagenum: ADD FOR OTHER JOURNALS AS YOU GO 
# should it go through one book at a time or all? 

## LOADING PACKAGES ----
library(groundhog)

date <- "2022-11-02"
requiredPackages <-  c("assertr","dplyr","here", "terra","tidyverse",
                       "tidyr")

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
  filename <- "HJ7-processed-step-1_2022-12-13.csv" 
  
  data <- read.csv(here::here("data","digitized_data",
                                "occurrence_data",
                                "data_cleaning", 
                                filename))
  
  # loading places metadata
  places <- read.csv(here::here("data", 
                                "reference_data",
                                "islands-and-districts_22-11-29.csv"))
  
# TASK 1: checking for repeated taxon observations for given sampling location and time ----
  
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

## vElevM 
  # constraint: 1) read as numeric 
  
  # if there are non-Na elements...
  if(length(data$vElevM[is.na(data$vElevM)])
     <length(data$vElevM)){
    data %>% 
      verify(., has_class("vElevM", class="numeric"))
  } 
  
## vLat & vLon 
  # constraint: 1) within rough bounding box
  # of where HJ collected from 
  
  # if there are non-Na elements...
  if(length(data$vLat[is.na(data$vLat)])
     <length(data$vLat)){
    
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
    
  # check within certain range (drawn from arbitrary
  # boundary box around 
  # South BC, North Washignton Area)
  
  data %>% 
    chain_start %>% 
    assert(within_bounds(47,51),vLat) %>% 
    assert(within_bounds(-129,-121),vLon) %>% 
    chain_end(error_fun = error_df_return)
  } 
  
## vUTM 
  # constraint: 1) must be bounded by same limits as latitude and longitude
 
  # if there are non-Na elements...
  if(length(data$vUTM[is.na(data$vUTM)])
     <length(data$vUTM)){
    projection <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs" # assumed 
    
    # convert UTM coordinates into decimal degrees
    UTM <- data %>% separate(vUTM, c("zone","x","y"), " ") # seperating UTM into x, and y components
    points <- cbind(as.numeric(UTM$x), as.numeric(UTM$y)) # making dataframe with x y components
    v <- vect(points, crs=projection) # making spatial points data frame
    z <- project(v, projection)  # projecting points using assinged crs  
    lonlat <- as.data.frame(t((geom(z)[, c("x", "y")]))) # extracting lat lon from spatial points frame
    
    # assigning lat and lon from UTM to these rows 
    data$lat <- as.numeric(lonlat$y)
    data$lon <- as.numeric(lonlat$x) 
   
    # check within certain range (drawn from arbitrary
    # boundary box around 
    # South BC, North Washington Area)
    
    data %>% 
      chain_start %>% 
      assert(within_bounds(47,51),lat) %>% 
      assert(within_bounds(-129,-121),lon) %>% 
      chain_end
    
      # removing these columns for consistency
      data <- data %>% select(-c("lat", "lon"))
    
  }
  
## vCoorUncM  
  # constraint: must be numeric 
  
  # if there are non-Na elements...
  if(length(data$vLat[is.na(data$vLat)])
     <length(data$vLat)){
    data %>% 
      verify(., has_class("vCoorUncM", class="numeric"))
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
    
  # if there are non-Na elements...
  if(length(data$assCollTaxa[is.na(data$assCollTaxa)])
       <length(data$assCollTaxa)){
  data %>% 
    chain_start %>%
    assert(letters_only, assCollTaxa) 
    assert(genusCapitalized, assCollTaxa) # this function NEEDS TO BE UPDATED FOR THE MUTLIPOLE ELEMENTS NORMALLY 
    
    chain_end
  }
  
## numPlantsCode 
  # constraint: 1) must be between 0 and 5 
  # 2) read as integer
  
  # if there are non-Na elements...
  if(length(data$numPlantsCode[is.na(data$numPlantsCode)])
     <length(data$numPlantsCode)){
  data %>% 
    chain_start %>% 
    assert(in_set(0:5),numPlantsCode) %>% 
    verify(., has_class("vCoorUncM", class="numeric")) %>% 
    chain_end
  }

## orgQuantity
  # if has non-NA elements...
  if(length(data$orgQuantity[is.na(data$orgQuantity)])
     <length(data$orgQuantity)){
    data %>% 
      #column is read as character 
      verify(., has_class("orgQuantity", class="character"))
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
  
# adding archiveID row
data_cleaned <- data %>% 
mutate("archiveID"= J,.before=pageNum)

write.csv(data_cleaned, here::here("data","digitized_data",
                                   "occurrence_data",
                                   "clean_data", paste0("HJ-",J, "_clean-occurrences.csv")), row.names = F)
