###project: Forage Fish Resilience
###author(s): MW
###goal(s): pulling in hurricane information 
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, splitstackshape, purrr, zoo, pracma)

### set simple workflow functions ---
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

hurricanes <- read_csv('local-data/hurricane-info-raw.csv') |> 
      filter(site %in% c('Cedar_Key', 'Charlotte_Harbor', 'Jacksonville',
                         'Northern_Indian_River', 'Tampa_Bay', 'Southern_Indian_River', "Apalachicola_Bay")) |> 
      filter(hurDate >= '1996-01-01') |> 
      filter(!is.na(DistClosest_km))
      
unique(hurricanes$site)
unique(hurricanes$SID)
