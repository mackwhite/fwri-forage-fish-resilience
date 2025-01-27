###project: Forage Fish Resilience
###author(s): MW
###goal(s): pulling from fishbase
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, stringr, splitstackshape, purrr, 
                 zoo, pracma, rfishbase)

### set simple workflow functions ---
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

### read in necessary data ---

spp <- read_xlsx('local-data/forage_fish_trait_list.xlsx') |> 
      janitor::clean_names() |> 
      select(scientific_name, common_name, family, genus, specific_epithet)

ests <- rfishbase::estimate(spp$scientific_name) |> 
      janitor::clean_names() |> 
      rename(scientific_name = species,
             tl_max = max_length_tl,
             sl_max = max_length_sl,
             l_inf = linf,
             ppr_min = pred_prey_ratio_min,
             ppr_max = pred_prey_ratio_max) |> 
      select(scientific_name, feeding_path, troph, a, b, k, l_inf, depth_max, depth_min,
             ppr_min, ppr_max, temp_pref_min, temp_pref_mean, temp_pref_max,
             tl_max, sl_max)
      
spp_ests <- spp |> 
      left_join(ests, by = "scientific_name") |> 
      group_by(genus) |> 
      mutate(across(where(is.numeric),
            ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) |> 
      ungroup() |> 
      group_by(family) |> 
      mutate(across(where(is.numeric),
                    ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) |> 
      ungroup() |> 
      mutate(across(where(is.numeric),
                    ~ifelse(is.nan(.), NA, .)))

writexl::write_xlsx(spp_ests, "local-data/forage_fish_trait_list_v2.xlsx")
