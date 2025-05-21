###project: Forage Fish Resilience
###author(s): MW
###goal(s): calculating synchrony and turnover
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
df <- read_rds("local-data/key-datasets/forage_fish_master.RDS")
estuary <- read_csv("local-data/archive/for-joins/bay-to-estuary.csv") |> janitor::clean_names()
site_info <- read_csv("local-data/annual_site_information_of_interest.csv")

dat <- df |> 
      left_join(estuary, by = "bay") |> 
      left_join(site_info, join_by("year", "zone", "grid", "bay")) |> 
      filter(gear_details == "21.3-m Seine") |> 
      mutate(year_month = paste(year, month, sep = ""),
             year_month = as.numeric(year_month),
             est_zone = paste(estuary, zone, sep = "_"),
             est_zone_grid = paste(est_zone, grid, sep = "_"),
             site = paste(est_zone_grid, rep, sep = "_"),
             biomass = ((n*mean_weight_g)/140),
             abundance = (n/140)) |> 
      group_by(site) |> 
      mutate(obs = n()) |> 
      ungroup() |> 
      filter(obs>24) |> 
      select(-obs) |> 
      filter(year >= 1996) |> 
      mutate(simple_date = make_date(year = year, month = month, day = 1))
glimpse(dat)

### detrend time series for biomass and abundance ---
### list metrics to detrend ---
metrics <- c("biomass", "abundance")

### create de-trending function ---
detrend_all <- function(df, metrics) {
      df <- df |> arrange(simple_date)
      
      if (nrow(df) < 24) {
            for (m in metrics) {
                  df[[paste0("detrended_", m)]] <- df[[m]]
            }
            return(df)
      }
      
      for (m in metrics) {
            # Interpolate NA values
            x <- na.approx(df[[m]], rule = 2, na.rm = FALSE)
            
            # Convert to time series
            ts_data <- ts(x, frequency = 12, start = c(min(df$year), min(df$month)))
            
            # STL decomposition
            fit <- stl(ts_data, s.window = "periodic")
            
            # Save trend
            df[[paste0("detrended_", m)]] <- as.numeric(fit$time.series[, "trend"])
      }
      return(df)
}

ss_detrended <- dat |> 
      group_by(estuary, zone, grid) |> 
      group_split() |> 
      lapply(detrend_all, metrics = metrics) |> 
      bind_rows()
glimpse(ss_detrended)

### set up dataset for calculations ---

dat1 <- ss_detrended |> 
      select(site, year, year_month, scientific_name, detrended_biomass, detrended_abundance) |> 
      distinct() |> 
      group_by(site, year, year_month, scientific_name) |> 
      mutate(count = n()) |> 
      ungroup() |> 
      group_by(site, year, year_month, scientific_name) |> 
      summarize(detrended_biomass = mean(detrended_biomass, na.rm = TRUE),
                detrended_abundance = mean(detrended_abundance, na.rm = TRUE)) |> 
      ungroup() |> 
      group_by(site, year, scientific_name) |> 
      summarize(detrended_biomass = mean(detrended_biomass, na.rm = TRUE),
                detrended_abundance = mean(detrended_abundance, na.rm = TRUE)) |> 
      ungroup() |> 
      mutate(detrended_biomass = detrended_biomass +9,
             detrended_abundance = detrended_abundance + 61) |> 
      na.omit()

site_vector <- unique(dat1$site)
df_temp <- data.frame(matrix(ncol = 5, nrow = length(site_vector)))
df_temp[, 1] <- site_vector
names(df_temp) <- c("vector", "biomass_turnover", "biomass_synchrony", 
                    "abundance_turnover", "abundance_synchrony")

for (i in seq_along(site_vector)) {
      temp <- dat1 |> filter(site == site_vector[i])
      
      # Turnover: biomass
      df_temp[i, 2] <- tryCatch({
            biomass_turnover <- turnover(
                  df = temp, time.var = "year",
                  abundance.var = "detrended_biomass",
                  species.var = "scientific_name",
                  metric = "total"
            )
            mean(biomass_turnover[[1]], na.rm = TRUE)
      }, error = function(e) NA_real_)
      
      # Synchrony: biomass
      df_temp[i, 3] <- tryCatch({
            synchrony(
                  df = temp, time.var = "year",
                  abundance.var = "detrended_biomass",
                  species.var = "scientific_name",
                  metric = "Loreau",
                  replicate.var = NA
            )
      }, error = function(e) NA_real_)
      
      # Turnover: abundance
      df_temp[i, 4] <- tryCatch({
            abundance_turnover <- turnover(
                  df = temp, time.var = "year",
                  abundance.var = "detrended_abundance",
                  species.var = "scientific_name",
                  metric = "total"
            )
            mean(abundance_turnover[[1]], na.rm = TRUE)
      }, error = function(e) NA_real_)
      
      # Synchrony: abundance
      df_temp[i, 5] <- tryCatch({
            synchrony(
                  df = temp, time.var = "year",
                  abundance.var = "detrended_abundance",
                  species.var = "scientific_name",
                  metric = "Loreau",
                  replicate.var = NA
            )
      }, error = function(e) NA_real_)
}

df_temp_final <- df_temp |> 
      separate(vector, into = c("estuary", "zone", "grid", "rep"), sep = "_", remove = FALSE) |> 
      group_by(estuary, zone, grid) |> 
      summarize(across(biomass_turnover:abundance_synchrony, ~mean(.x, na.rm = TRUE)))

write_csv(df_temp_final, "local-data/forage-fish-dynamics.csv")      
