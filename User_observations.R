#Obtener las observaciones de un usuario 

#iNat username
user <- "gonzalobravopatagonia"

#Load required packages
library(httr)
library(rlist)
library(reactable)
library(dplyr)

#API set-up
x <- 1
resp <- GET(paste("https://api.inaturalist.org/v1/observations/species_counts?user_id=", user, "&page=", as.character(x), "&hrank=species", sep = ""))
parsed <- content(resp, as = "parsed")

#Retreving data from the API
while(x < (parsed$total_results/500)+1){
  resp <- GET(paste("https://api.inaturalist.org/v1/observations/species_counts?user_id=", user, "&page=", as.character(x), "&hrank=species", sep = ""))
  parsed <- content(resp, as = "parsed")
  modJSON <- parsed$results %>%
    list.select(taxon$name, count, taxon$observations_count)
  if(x == 1){
    data <- list.stack(modJSON)
  }
  if(x > 1){
    dataz <- list.stack(modJSON)
    data <- rbind(data, dataz)
  }
  x <- x+1
}

colnames(data) <- c("scientific_name","Obsbyuser","ObsiNat")
