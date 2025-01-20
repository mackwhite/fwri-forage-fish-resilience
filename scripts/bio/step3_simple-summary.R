###project: Forage Fish Resilience
###author(s): MW
###goal(s): summarizing data to make smaller
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
nacheck(dat)

sci_name_nas <- dat |> filter(is.na(Scientificname))

### get to know the data a little bit -----
unique(dat$Bay)
# [1] "AP" "CH" "CK" "IR" "JX" "TB" "TQ"

### check out the distribution of gear types ---
unique(dat$Gear)
#  [1] "20"  "160" "23"  "10"  "11"  "12"  "13"  "106" "22"  "107" "180" "5"   "436"
# [14] "438"

all_gear_summary <- dat |> select(Reference, Gear, GearDetails) |> 
      distinct() |> group_by(Gear, GearDetails) |> summarize(obs = n()) |> 
      arrange(desc(obs))|> print()
rm(all_gear_summary)

est_gear_summary <- dat |> select(Bay, Reference, Gear, GearDetails) |> 
      distinct() |> group_by(Bay, Gear, GearDetails) |> summarize(obs = n()) |> 
      arrange(desc(obs))|> print(n=100)
rm(est_gear_summary)

### check out the distribution of zones ---
est_zone_summary <- dat |> select(Bay, Reference, Zone) |> 
      distinct() |> group_by(Bay, Zone) |> summarize(obs = n()) |> 
      arrange(desc(Bay))
rm(est_zone_summary)

est_grid_summary <- dat |> select(Bay, Reference, Grid) |> 
      distinct() |> group_by(Bay, Grid) |> summarize(obs = n()) |> 
      arrange(desc(obs))
rm(est_zone_summary)

### clean environment before next step ---
keep <- c("dat", "nacheck")
rm(list = setdiff(ls(), keep))

### look into biotic data ---
all_fish_counts <- dat |> select(Commonname, Scientificname, N) |> 
      group_by(Commonname) |> summarize(counts = sum(N)) |> 
      arrange(desc(counts))

est_fish_counts <- dat |> 
      select(Bay, Commonname, Scientificname, N) |> 
      group_by(Bay, Commonname) |> 
      summarize(counts = sum(N)) |> 
      ungroup() |> 
      group_by(Bay) |> 
      mutate(total_counts = sum(counts),  # Total catches per Bay
             proportion = counts / total_counts) |>  # Proportion by species
      ungroup() |> 
      arrange(Bay, desc(counts))

bays <- est_fish_counts |> group_split(Bay)
bay_codes <- unique(est_fish_counts$Bay)

for (i in seq_along(bay_codes)) {
      assign(paste("fish counts", bay_codes[i]), bays[[i]])
}

### Apalachicola Bay
##1: Spot
##2: Pinfish
##3: Menhadens
##4: Bay Anchovy
##5: Atlantic Croaker

### Cedar Key
##1: Spot
##2: Bay Anchovy
##3: Pinfish
##4: Silver Perch
##5: Striped Mullet

### Tampa Bay
##1: Bay Anchovy
##2: Pinfish
##3: New World Silversides
##4: Spot
##5: Eucinostomus

### Charlotte Harbor
##1: Bay Anchovy
##2: Pinfish
##3: Eucinostomus
##4: Rainwater Killifish
##5: New World Silversides

### Southern Indian River
##1: Bay Anchovy
##2: Pinfish
##3: Irish Pompano
##4: White Mullet
##5: Menhadens

### Northern Indian River
##1: Bay Anchovy
##2: Rainwater Killifish
##3: Pinfish
##4: Striped Mullet
##5: Spot

### Northeast Florida
##1: Spot
##2: Bay Anchovy
##3: Striped Mullet
##4: Menhadens
##5: Atlantic Silverside

### data cleaning steps ----
## filter out NAs from Scientific Name [will also handle NAs in in 'N' column]
## filter out common names with shark or shrimp or crab as first step in getting down to fish
## summarize length for each species by reference (include max, min, and sd)

### clean environment before next step ---
keep <- c("dat", "nacheck")
rm(list = setdiff(ls(), keep))

species <- dat |> select(Commonname, Scientificname) |> distinct()

## removing crabs, fishes, and sharks only reduced number of taxa by 60
### so decided to keep here and label for future
species_groups <- species |> 
      mutate(group = case_when(
            str_detect(Commonname, regex("shrimp", ignore_case = TRUE)) ~ "shrimps",
            str_detect(Commonname, regex("shark", ignore_case = TRUE)) ~ "sharks",
            str_detect(Commonname, regex("crab", ignore_case = TRUE)) ~ "crabs",
            TRUE ~ "fishes"))

test_dat <- dat |> slice_tail(n=1000)
dat <- dat |> 
      filter(!is.na(Scientificname)) |> 
      group_by(Reference, Sampling_Date, Bay, Gear, GearDetails, Rep, Latitude, Longitude, Zone, Subzone,
               Grid, Depth, Scientificname, Commonname, N) |>
      summarize(MeanLength = case_when(
                  !is.na(Length) ~ mean(Length, na.rm = TRUE),
                  TRUE ~ NA_real_),
                MaxLength = case_when(
                  !is.na(Length) ~ max(Length, na.rm = TRUE),
                  TRUE ~ NA_real_),
                MinLength = case_when(
                      !is.na(Length) ~ min(Length, na.rm = TRUE),
                      TRUE ~ NA_real_),
                SDLength = case_when(
                      !is.na(Length) ~ sd(Length, na.rm = TRUE),
                      TRUE ~ NA_real_)) |> 
      ungroup()
write_rds(dat, "local-data/fim-master-short.RDS")

dat <- dat |> distinct()

### Things to Ask Ted -----
## get a handle on NAs in Subzone & Grid
## list of "forage fish" taxa?
