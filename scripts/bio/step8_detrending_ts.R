###project: Forage Fish Resilience
###author(s): MW
###goal(s): seasonal detrend of time series
###date(s): February 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readr, tsibble, zoo, forecast, strucchange,
                 performance, ggeffects, lsmeans, emmeans, ggpubr, visreg)

### set simple workflow functions ---
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

df <- read_csv("local-data/key-datasets/discrete-community-timeseries.csv") 

### separate by gear and summarize monthly by zone ----

small_seine_grid_df <- df |> 
      filter(gear_details == "21.3-m Seine") |> 
      # group_by(year, month, day, estuary, zone) |> 
      group_by(year, month, estuary, zone, grid) |> 
      summarize(bm_m2 = mean(total_biomass_m2), 
                abund = mean(total_n),
                max_size = mean(max_size),
                k = mean(mean_k),
                gen_time = mean(mean_generation_time), 
                max_depth = mean(mean_depth_max), 
                min_depth = mean(mean_depth_min), 
                pref_temp_min = mean(mean_temp_pref_min), 
                pref_temp_mean = mean(mean_tem_pref_mean), 
                pref_temp_max = mean(mean_temp_pref_max), 
                trophic_level = mean(mean_troph), 
                benthic_prop = mean(benthic_n), 
                pelagic_prop = mean(pelagic_n),
                species_richness = mean(species_richness),
                species_evenness = mean(species_evenness)) |> 
      ungroup() |> 
      mutate(simple_date = make_date(year = year, month = month, day = 1)) |> 
      mutate(est_zone = paste(estuary, zone, sep = "_"))
glimpse(small_seine_grid_df)

### list metrics to detrend ---
metrics <- c("bm_m2", "abund", "max_size", "k", "gen_time", 
             "max_depth", "min_depth", "pref_temp_min", 
             "pref_temp_mean", "pref_temp_max", "trophic_level", 
             "benthic_prop", "pelagic_prop", "species_richness", "species_evenness")

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
            x <- na.approx(df[[m]], na.rm = FALSE)
            
            # Convert to time series
            ts_data <- ts(x, frequency = 12, start = c(min(df$year), min(df$month)))
            
            # STL decomposition
            fit <- stl(ts_data, s.window = "periodic")
            
            # Save trend
            df[[paste0("detrended_", m)]] <- as.numeric(fit$time.series[, "trend"])
      }
      return(df)
}

### create season signature function ---
seasonality_all <- function(df, metrics) {
      df <- df |> arrange(simple_date)
      
      if (nrow(df) < 24) {
            for (m in metrics) {
                  df[[paste0("seasonal_", m)]] <- NA
            }
            return(df)
      }
      
      for (m in metrics) {
            ts_data <- ts(df[[m]], frequency = 12, start = c(min(df$year), min(df$month)))
            fit <- stl(ts_data, s.window = "periodic")
            df[[paste0("seasonal_", m)]] <- as.numeric(fit$time.series[, "seasonal"])
      }
      return(df)
}

### apply functions and evaluate output ---
ss_detrended <- small_seine_grid_df |> 
      group_by(estuary, zone, grid) |> 
      group_split() |> 
      lapply(detrend_all, metrics = metrics) |> 
      bind_rows()

ss_seasonal <- ss_detrended |> 
      group_by(estuary, zone, grid) |> 
      group_split() |> 
      lapply(seasonality_all, metrics = metrics) |> 
      bind_rows()

glimpse(ss_seasonal)
glimpse(ss_detrended)

### read out dataset as a csv file ---
write_csv(ss_seasonal, "local-data/detrended-smallseine-ts.csv")
