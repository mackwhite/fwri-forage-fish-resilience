###project: Forage Fish Resilience
###author(s): MW, JR
###goal(s): visualize abundance of individuals through time
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, stringr, splitstackshape, purrr, zoo, pracma)

### set simple workflow functions ---
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

### read in necessary data ---
dat <- read_rds('local-data/fim-master.RDS')
glimpse(dat)


### visualize time series of forage fish biomass -----