############################################################################
###         Conversion of Field Notes for from Darwin Core               ###
###           Collection data from Darwin Core to the                    ###
###              format of the Royal BC Museum Herbarium                 ###
###                         Emma Menchions                               ###
###                         Created Jan3/23                              ###
############################################################################

# Requirements: data has been processed with the post entry procotcol

# Objective: takes dwc-formatted collections and converts them to RBCM format
# for comparison

## 1) LOADING & INSTALLING PACKAGES ----
# using groundhog to manage package versioning 

install.packages("groundhog")
library(groundhog)

set.groundhog.folder(here::here("packages"))
date <- "2022-11-02"

requiredPackages <-  c("assertr","dplyr","here", "lubridate","magrittr","purrr","ritis",
                       "stringi","taxize","terra","tidyverse","tidyr")

for (pkg in requiredPackages) {
  groundhog.library(pkg, date)
}

rm(requiredPackages)

## 2) READING IN DATA ----

J <- 7 # journal number
  
dwc_data <- read.csv(
            here::here("data","data_digitization",
            "collection_data","darwin_core_data", 
            paste0("HJ",J), unique(
            as.character(max(list.files(here::here("data","data_digitization",
            "collection_data",
            "darwin_core_data", paste0("HJ",J))))))))

rbcm_database <- read.csv(here::here("data","existing_data","rbcm_HJ-specimens.csv"))

## 3) Converting columns to have similar names to RBCM ----
  
RBCM_format <- dwc_data %>% 
  ## A. RBCM Accession number - blank
  mutate("D"= "",.before=verbatimScientificName) %>% 
  
  ## B. scientificName --> ScientificName
  dplyr::rename(ScientificName = verbatimScientificName) %>% 
  
  ## C. identificationQualifier --> QualifiedScientificName + authority 
  mutate(QualifiedScientificName = 
           case_when(is.na(identificationQualifier) ~ "NA", 
           !is.na(identificationQualifier) ~ 
             str_c(identificationQualifier,scientificNameauthorship, sep=" "))) %>% 
  mutate_at(c("QualifiedScientificName"), ~na_if(., "NA")) %>% 
  
  ## D. Common name --> blank
  mutate("CommonName"= "",.before=family) %>% 

  ## E. family --> family
  dplyr::rename(Family = family) %>% 

  ## F. genus --> Genus 
  dplyr::rename(Genus = genus) %>% 

  ## G. specificEpithet --> Species
  dplyr::rename(Species = specificEpithet) %>% 

  ## H. scientificNameAuthorship --> SpeciesAuthority 
  # does this differ if intraspecific epithet
  dplyr::rename(SpeciesAuthority = scientificNameauthorship) %>% 
  
  ## I. intraspecificEpithet --> Subspecies 
  # if taxonRank = subspecies
  mutate("Subspecies"=case_when(taxonRank == "SUBSPECIES" ~ intraspecificEpithet)) %>% 
  
  ## J. blank --> SubspeciesAuthority
  mutate("SubspeciesAuthority"= "",.after=intraspecificEpithet) %>% 

  ## K. intraspecificEpithet -->Variety
  # if taxonRank = variety
  mutate("Variety"=case_when(taxonRank == "VARIETY" ~ intraspecificEpithet)) %>% 
    
  ## L. blank --> VarietyAuthority
  mutate("VarietyAuthority"= "",.after=SubspeciesAuthority) %>% 
    
  ## M. intraspecificEpithet --> Forma
  # if taxonRank = forma
  mutate("Forma"=case_when(taxonRank == "FORMA" ~ intraspecificEpithet)) %>% 
    
  ## N. blank --> FormaAuthority
  mutate("FormaAuthority"= "",.after=VarietyAuthority) %>% 

  ## O. idBy --> Identifier
  dplyr::rename(Identifier = identificationBy) %>% 
  
  ## P. blank --> IDDate
  mutate("IDDate"= "",.after=Identifier) %>% 
  
  ## Q. blank --> IDNotes
  mutate("IDNotes"= "",.after=Identifier) %>% 
  
  ## R. blank --> CDC (system generated)
  # or fill in with dynamic properties
  mutate("CDC"= "",.after=Identifier) %>% 
  
  ## S. datasetName --> MuseumCollection
  # Harvey Janszen Collection
  dplyr::rename(MuseumCollection = datasetName) %>% 
  
  ## T. recordedBy --> Collector
  dplyr::rename(Collector = recordedBy) %>% 

  ## U. recordNumber --> CollectorsFieldNumber
  dplyr::rename(CollectorsFieldNumber= recordNumber) %>% 

  ## V. eventDate --> CollectionDate
  dplyr::rename(CollectionDate = eventDate) %>% 
  
  ## W. stateProvince --> Prov_State 
  dplyr::rename(Prov_State = stateProvince) %>% 

  ## X. county --> District !!!!!!!!!!!!!!!
  # convert SGI and NGI to "Gulf Islands" 
  # anything on Vancouver Island to Vancouver Island
  # Lower Fraser Valley
  dplyr::rename(District = county) %>% 

  ## Y. locality --> LocationName!!!!!!!!!!!!!!!!
  # will need to re-adapt entry...
  # consider gazeteer matches- can I download match automatically? 
  mutate("LocationName"= "",.after=Collector) %>% 
  
  ## Z. locality --> LocationDescription !!!!!!!!!!!!!!!!
  mutate("LocationDescription"= "",.after=Collector) %>% 
  
  ## AA. decimalLatitude or verbatimLatitude ? --> Latitude
  dplyr::rename(Latitude = verbatimLatitude) %>% 

  ## AB. decimalLongitude or verbatimLongitude ? --> Longitude
  dplyr::rename(Longitude = verbatimLongitude) %>% 

  ## AC, AD, AE verbatimCoordinates (UTM) --> UTMZone + UTMNorthing + UTMEasting
  separate(verbatimCoordinates, into = c("UTMZone", "UTMNorthing", 
  "UTMEasting"), sep = " ") %>% 

  ## AF. verbatimElevation --> Elevation (+ unit)
  mutate("unit"= "m",.after=verbatimElevation) %>% 
  unite("Elevation", verbatimElevation, unit, sep = " ", 
              na.rm=T, remove=T) %>% 

  ## AG. blank --> Park_ER_IR
  mutate("Park_ER_IR"= "",.after=Elevation) %>% 
  
## AH. habitat --> HabitatRemarks
  # remarks separated with ; 
  dplyr::rename(HabitatRemarks = habitat) %>% 
  mutate(HabitatRemarks=
           str_replace_all(string=HabitatRemarks, 
                           pattern= ",", replacement =";")) %>% 

## AI. occurrenceRemarks + lifeStage + organismQuantity 
  # (and plant coding)/ individual count --> SpecimenNotes
  
  unite("SpecimenNotes", occurrenceRemarks, lifeStage, sep = " ", 
        na.rm=T, remove=F) %>% 
  
## AJ. organismQuantity --> SpeciesAbundance
  dplyr::rename(SpeciesAbundance = organismQuantity) %>% 

  dplyr::select(D, ScientificName, QualifiedScientificName,CommonName, Family,
                Genus, Species, SpeciesAuthority, Subspecies, SubspeciesAuthority, 
                Variety, VarietyAuthority, Forma, FormaAuthority, Identifier, IDDate,
                IDNotes, CDC, MuseumCollection, Collector, CollectorsFieldNumber,
                CollectionDate, Prov_State, District, LocationName,LocationDescription,
                Latitude, Longitude,UTMZone,UTMNorthing, UTMEasting, 
                Elevation, Park_ER_IR,
                HabitatRemarks, SpecimenNotes, SpeciesAbundance,
                locality, locationRemarks) %>% 
  
  # Replacing commas in locality
  mutate(locality=
           case_when(grepl(",", locality) ~ 
                    str_replace_all(string=locality, pattern= ", ",
                    replacement ="; "),!grepl(",", locality) ~locality)) %>% 
  mutate_all(na_if, "")

# replacing erraneous "m"s with NA
RBCM_format$Elevation[RBCM_format$Elevation=="m"] <- NA
RBCM_format$QualifiedScientificName[RBCM_format$QualifiedScientificName=="NA"] <- NA

## 4) Creating LocationName and LocationDescription columns from "Locality" using Gazeteer ----
# downloaded from https://catalogue.data.gov.bc.ca/dataset/bc-geographical-names
BCplaces <- read.csv(here::here(
  "data","reference_data", "BCGW_7113060B_1672719865150_14800","GNSGGRPHCL.csv"))

# initializing vectors
placeNames <- c()
RBCM_format$LocationName <- NA
RBCM_format$LocationDescription <- NA
RBCM_format[is.na(RBCM_format$locality), "locality"]<- "missing"
RBCM_format[is.na(RBCM_format$locationRemarks), "locationRemarks"]<- "missing"
RBCM_format$locationRemarks <- tolower(RBCM_format$locationRemarks)

  for (i in 1:dim(RBCM_format)[1]){ # for each row...
      # split up locality string into different placename segments...
      placeNames[i]<- strsplit(RBCM_format$locality[i], "; ")
      placeNames[[i]] <- str_to_title(placeNames[[i]])
      
      # for every segment...
      for (j in 1:length(placeNames[[i]])){
        # if there is a match in gazeteer...
        if (length(BCplaces$GEO_NAME[placeNames[[i]][j] == BCplaces$GEO_NAME])>=1){
          
          # and if there has not already been another locationName
          # listed by a previous name segment...
          if(is.na(RBCM_format$LocationName[i])){
            # assign the LocationName as this name segment
            RBCM_format$LocationName[i] <- 
              BCplaces$GEO_NAME[placeNames[[i]][j]==BCplaces$GEO_NAME]
            
          } else { 
            # if there is already a LocationName, paste it on to that name
            RBCM_format$LocationName[i] <- 
              paste0(RBCM_format$LocationName[i],"; ", 
                     BCplaces$GEO_NAME[placeNames[[i]][j]==BCplaces$GEO_NAME])
          }
        
        # if there is no match in the gazeteer...
        } else if (length(BCplaces$GEO_NAME[placeNames[j] == BCplaces$GEO_NAME])<1){
          # assign it to the location description section
          RBCM_format$LocationDescription[i] <- placeNames[[i]][j]
          
        }
      }
    
      # additionally, if the assigned location description if different from
      # the location remarks...
      if(!is.na(RBCM_format$LocationDescription[i]) & tolower(RBCM_format$LocationDescription[i])
         != RBCM_format$locationRemarks[i] & RBCM_format$locationRemarks[i] != "missing"){
        # add the location remakrs to the location description with ";"
        RBCM_format$LocationDescription[i] <- paste0(RBCM_format$LocationDescription[i], 
                                                     "; ",RBCM_format$locationRemarks[i])
      
        # or if there is no location description but there are location remarks
        # , assign the location remarks to it
      } else if (is.na(RBCM_format$LocationDescription[i]) & RBCM_format$locationRemarks[i]!= "missing"){
        RBCM_format$LocationDescription[i] <- RBCM_format$locationRemarks[i]
      }
  }
 
  # removing unecessary columns
  RBCM_format <- RBCM_format %>%  select(-locality, -locationRemarks)
  RBCM_format$LocationDescription <- tolower(RBCM_format$LocationDescription)
  
## 5) Saving file ----
  write.csv(RBCM_format,
            here::here("data", "data_digitization","rbcm_data",
                       "field_note_data", paste0("HJ",J),
                       paste0("HJ", J,"_rbcm-format-field-notes_",Sys.Date(),".csv")), row.names = F)
  

