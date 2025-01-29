###project: Forage Fish Resilience
###author(s): MW
###goal(s): generating figures for midterm report 
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, corrplot, 
                 splitstackshape, purrr, zoo, pracma, vegan, e1071,
                 MuMIn, sjPlot, lme4, corrplot, glmmTMB,
                 performance, ggeffects, ggpubr, parameters, ggstats, 
                 brms, mixedup, lterpalettefinder)

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
df <- read_csv("local-data/stability-model-data-012025.csv") |> 
      mutate(grid = case_when(
            is.na(grid) ~ 9999,
            TRUE ~ grid
      ))
glimpse(df)
nacheck(df)
df_nan <- df |> filter(if_any(everything(), is.nan))

dat_scaled <- df |> 
      mutate(across(comm_bm_mean:comm_bm_stability,\(x) scale(x, center = TRUE))) |> 
      group_by(bay, estuary, zone) |> 
      ## this is a function syntax
      mutate(across(comm_species_richness:comm_temp_max,\(x) scale(x, center = TRUE))) |>
      ungroup() |> 
      select(-flag)

glimpse(dat_scaled)

mod_df <- dat_scaled
corr <- mod_df |> 
      select(comm_bm_stability:comm_temp_max)

corr_matrix <- cor(corr, use = 'complete.obs')
corrplot(corr_matrix, method = "number", type = "lower", tl.col = "black", tl.srt = 45)

mod_df <- mod_df |> na.omit()

### first run ---
global <- glmmTMB(comm_bm_stability ~ comm_species_richness + comm_max_size +
                        comm_k + comm_depth_min + comm_depth_max + comm_temp_mean + 
                        comm_temp_min + comm_temp_max + (1|estuary),
                  mod_df,
                  na.action = "na.fail",
                  family = gaussian(),
                  REML = FALSE
)

keep <- c("global", "mod_df") 
rm(list = setdiff(ls(), keep))

model_set <- dredge(global) |> 
      filter(delta < 2)