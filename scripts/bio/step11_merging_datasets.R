###project: Forage Fish Resilience
###author(s): MW
###goal(s): merge final datasets
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, FSA, readr, forcats, multcompView, readxl, dplyr, 
                 splitstackshape, purrr, zoo, pracma, vegan, e1071, codyn, lubridate, forecast)

### set simple workflow functions ---
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

### set color pallete for visualizations ---
estuary_palette = c("Apalachicola Bay"="#e6d7b9",
                    "Charlotte Harbor"="#a7c4a0",
                    "Cedar Key"="#64a988",
                    'Northern Indian River'="#89c8d9",
                    "Northeast Florida"="#f2c6b4",
                    'Tampa Bay'='#dba5a4',
                    "Southern Indian River"="#bfaed9")

heat_wave_palette = c("Moderate"="lightgrey",
                      "Significant"="pink",
                      "Severe"="#BF616A",
                      "Extreme"="darkred")

cold_snap_palette = c("Moderate"="lightgrey",
                      "Significant"="#88C0D0",
                      "Severe"="#4682B4",
                      "Extreme"="darkblue")

### read in data ----
sampling_event <- read_csv('local-data/key-datasets/discrete-community-timeseries.csv')
annual <- read_csv('local-data/key-datasets/annual-community-timeseries.csv')
stability <- read_csv('local-data/key-datasets/community-biomass-stability.csv')
dynamics <- read_csv('local-data/forage-fish-dynamics.csv')
estuary <- read_csv("local-data/archive/for-joins/bay-to-estuary.csv") |> janitor::clean_names()
site_info <- read_csv("local-data/annual_site_information_of_interest.csv")
site_info_por <- read_csv('local-data/site_information_of_interest.csv')

### sampling event ----
glimpse(sampling_event)
glimpse(site_info)
site <- site_info |> select(bay,zone,grid,year,lat,long)
discrete_final <- sampling_event |> 
      left_join(estuary) |> 
      left_join(site_info)

### annual time step ----
glimpse(annual)
annual_final <- annual |> left_join(estuary)

### stability ----
glimpse(stability)
glimpse(dynamics)

stability_final <- stability |> 
      rename(lat = mean_lat,
             long = mean_long) |> 
      left_join(dynamics)

### read out final datasets ---

write_csv(discrete_final, "local-data/key-datasets/discrete_foragefish_final.csv")
write_csv(annual_final, "local-data/key-datasets/annual_foragefish_final.csv")
write_csv(stability_final, "local-data/key-datasets/stability_foragefish_final.csv")
