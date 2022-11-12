#######
# this script will take occurrence data from the data entry template and place it in darwin core terms

#install.packages("groundhog")
library(groundhog)

## LOADING & INSTALLING PACKAGES ----
date <- "2022-11-02"
requiredPackages <-  c("readxl","here", "tidyverse","tidyr","Rfast","dplyr","purrr")

for (pkg in requiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {groundhog.library(pkg, date)}
}


## READING IN DATA ----
# change to read.csv after - coversion into csv should be the last step once template completed
template <- read_excel(here::here("data","digitized_data","HJ-occ-entry-template_22-11-11.xlsx"))

# splitting occurrenceID into record and catalogue numbers

template$recordNumber <- sapply(strsplit(template$occID, "-"), "[", 3)
template$catalogueNumber <- sapply(strsplit(template$occID, "-"), "[", 2)

# datasetName
template$datasetName <- "Harvey Janszen Collections"

# basisOfRecord 
tempalte$basisOfRecord <- "Human Observation"

# splitting verbatim date into year, month, day
# use lubridate

#taxonomy 
#using taxize

# Georeferencing

#organismQuantity 
