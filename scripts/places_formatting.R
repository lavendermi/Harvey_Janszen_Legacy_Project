# removing island from places table and making new rows without "island"

places <- read.csv(here::here("data", "reference_data","islands-and-districts_22-11-22.csv"))
removed <- data.frame(str_remove(places$island, " Island"), places[,2:3]) 
colnames(removed)[1] <- "island"

placesFormatted <- rbind(places, removed)

write.csv(placesFormatted,here::here("data","reference_data","islands-and-districts_22-11-29.csv"))

          