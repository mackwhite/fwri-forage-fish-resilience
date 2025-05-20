###project: Forage Fish Resilience
###author(s): MW
###goal(s): generating evenness metrics
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readr, readxl, dplyr, splitstackshape, purrr, zoo, pracma, vegan, e1071)

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

### read in data and bind -----
df <- read_rds("local-data/key-datasets/forage_fish_master.RDS")
glimpse(df)
df_summ <- read_csv('local-data/key-datasets/discrete-community-timeseries.csv')
estuary <- read_csv("local-data/archive/for-joins/bay-to-estuary.csv") |> 
      janitor::clean_names()

### quick site-level data for future analyses ----
site_info <- df |> 
      filter(gear_details == "21.3-m Seine") |> 
      select(year, bay, zone, grid, latitude, longitude, depth, temp_c, cond, ph, sal_ppt, do2) |> 
      distinct() |> 
      group_by(bay, zone, grid, year) |> 
      summarize(
            lat = mean(latitude, na.rm = TRUE),
            long = mean(longitude, na.rm = TRUE),
            obs_temp = mean(temp_c, na.rm = TRUE),
            obs_depth = mean(depth, na.rm = TRUE),
            obs_cond = mean(cond, na.rm = TRUE),
            obs_ph = mean(ph, na.rm = TRUE),
            obs_sal = mean(sal_ppt, na.rm = TRUE),
            obs_do2 = mean(do2, na.rm = TRUE),
            site_metric_n = n()
      ) |> 
      ungroup()
write_csv(site_info, "local-data/annual_site_information_of_interest.csv")

df_total <- df |>
      left_join(estuary, by = "bay") 

df_wide <- df_total %>%
      group_by(year, month, day, bay, estuary, gear_details, rep, zone, subzone, grid, scientific_name) %>%
      summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = scientific_name, values_from = n, values_fill = 0)  # Convert to wide format

# Remove grouping
df_wide <- as.data.frame(df_wide)

# Select only species abundance columns for `diversity()` function
abundance_matrix <- df_wide %>% select(-year, -month, -day, -bay, -estuary, -gear_details, -rep, -zone, -subzone, -grid)

df_wide$H <- diversity(abundance_matrix, index = "shannon")
df_wide$S <- rowSums(abundance_matrix > 0)

df_final <- df_total %>%
      left_join(df_wide %>% select(year, month, day, bay, estuary, gear_details, rep, zone, subzone, grid, H, S), 
                by = c("year", "month", "day", "bay", "estuary", "gear_details", "rep", "zone", "subzone", "grid")) |> 
      select(year, month, day, bay, estuary, gear_details, rep, zone, subzone, grid, H, S) |> 
      distinct() |> 
      mutate(rep = as.numeric(rep))

df_all <- df_summ |>
      left_join(df_final, by = c("year", "month", "day", "bay", "estuary", "gear_details", "rep", "zone", "subzone", "grid"))
glimpse(df_all)
nacheck(df_all)

df_for_joins <- df_all |> select(year, month, day, bay, estuary, gear_details, rep, zone, subzone, grid, H, S)
write_csv(df_for_joins, "local-data/evenness_metrics_for_join_in_stepseven.csv")

test <- df_all |> 
      select(species_richness, S, H)

test |> ggplot(aes(x=species_richness, y=H)) +
      geom_point() +
      geom_abline()


