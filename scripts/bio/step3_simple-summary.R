###project: Forage Fish Resilience
###author(s): MW
###goal(s): summarizing data to make smaller
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

### read in data and bind -----

dat <- read_rds('local-data/fim-master.RDS')
glimpse(dat)

species_list <- read_xlsx('local-data/species-list.xlsx')
glimpse(species_list)