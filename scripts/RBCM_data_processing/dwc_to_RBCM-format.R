#######################################################################
###       Processing Step 3: conversion from Data Entry Template    ###
###                      to RBCM Collections data format             ###
###                         Emma Menchions                          ###
###                         Created Nov. 7/22                       ###
#######################################################################

# takes the dwc collection sheet and converts it to RBCM format

## 1) LOADING & INSTALLING PACKAGES ----
# using groundhog to manage package versioning 
#install.packages("groundhog")
library(groundhog)

date <- "2022-11-02"
requiredPackages <-  c("assertr","dplyr","here", "lubridate","magrittr","purrr","ritis",
                       "stringi","taxize","terra","tidyverse","tidyr")

for (pkg in requiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {groundhog.library(pkg, date)}
}
rm(requiredPackages)

## 2) READING IN DATA ----

# using occurrence data in dwc format to test

## 3) Converting columns to have similar names to RBCM ---


RBCM_format <- dwc_data %>% # change variable to dwc_format
  ## A. RBCM Accession number - blank
  mutate("D"= "",.before=verbatimScientificName) %>% 
  
  ## B. scientificName --> ScientificName
  dplyr::rename(ScientificName = scientificName) %>% 
  
  ## C. identificationQualifier --> QualifiedScientificName + authority 
  unite("QualifiedScientificName", identificationQualifier, scientificNameauthorship, sep = " ", 
      na.rm=T, remove=F) %>% 
  
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

  ## U. blank --> CollectorsFieldNumber
  mutate("CollectorsFieldNumber"= "",.after=Collector) %>% 
  
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

  dplyr::rename(LocationName = locality) %>% 
  
  ## Z. locality --> LocationDescription !!!!!!!!!!!!!!!!
  dplyr::rename(LocationDescription = locationRemarks) %>% 
  
  ## AA. decimalLatitude or verbatimLatitude ? --> Latitude
  dplyr::rename(Latitude = verbatimLatitude) %>% 

  ## AB. decimalLongitude or verbatimLongitude ? --> Longitude
  dplyr::rename(Longitude = verbatimLongitude) %>% 

  ## AC, AD, AE verbatimUTM --> UTMZone + UTMNorthing + UTMEasting
  #separate(verbatimUTM, into = c("UTMZone", "UTMNorthing", "UTMEasting"), sep = " ") %>% 

  ## AF. verbatimElevation --> Elevation (+ unit)
  mutate("unit"= "m",.after=verbatimElevtion) %>% 
  unite("Elevation", verbatimElevtion, unit, sep = " ", 
              na.rm=T, remove=T) %>% 

  ## AG. blank --> Park_ER_IR
  mutate("Park_ER_IR"= "",.after=Elevation) %>% 
  
## AH. habitat --> HabitatRemarks
  # remarks separated with ; 
  dplyr::rename(HabitatRemarks = habitat) %>% 
  mutate(HabitatRemarks=str_replace_all(string=HabitatRemarks, pattern= ",", replacement =";")) %>% 

## AI. occurrenceRemarks + lifeStage + organismQuantity (and plant coding)/ individual count --> SpecimenNotes
  unite("SpecimenNotes", occurrenceRemarks, lifeStage, organismQuanity, sep = " ", 
        na.rm=T, remove=F) %>% 
  
## AJ. organismQuantity --> SpeciesAbundance
  dplyr::rename(SpeciesAbundance = organismQuanity) %>% 

  dplyr::select(D, ScientificName, QualifiedScientificName,CommonName, Family,
                Genus, Species, SpeciesAuthority, Subspecies, SubspeciesAuthority, 
                Variety, VarietyAuthority, Forma, FormaAuthority, Identifier, IDDate,
                IDNotes, CDC, MuseumCollection, Collector, CollectorsFieldNumber,
                CollectionDate, Prov_State, District, LocationName,LocationDescription,
                Latitude, Longitude, 
                #UTMZone,
                #UTMNorthing, 
                #UTMEasting, 
                Elevation, Park_ER_IR,
                HabitatRemarks, SpecimenNotes, SpeciesAbundance)

RBCM_format$Elevation[RBCM_format$Elevation=="m"] <- NA


