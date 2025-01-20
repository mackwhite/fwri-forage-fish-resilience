###project: Forage Fish Resilience
###author(s): MW
###goal(s): Relating all data tables
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, splitstackshape, ropensci/taxize, stringr)

dat <- read_rds('local-data/fim-master.RDS')
glimpse(dat)

species_list <- read_xlsx('local-data/species-list.xlsx')
glimpse(species_list)

test <- dat |> slice_head(n=1000)


### see lines starting on 329 for previous synthesis code using taxize
