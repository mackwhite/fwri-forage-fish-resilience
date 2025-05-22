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

bio <- read_rds('local-data/key-datasets/forage_fish_master.RDS')
evenness <- read_csv('local-data/evenness_metrics_for_join_in_step_eight.csv') |> 
      mutate(rep = as.character(rep))

bio_even <- bio |> left_join(evenness) |> filter(gear_details == "21.3-m Seine")
rm(bio)

bio_even_grid <- bio_even |> 
      group_by(year, month, estuary, bay, zone, grid, scientific_name, common_name) |> 
      summarize(
            abund = mean(n, na.rm = TRUE),
            area = mean(area_m2, na.rm = TRUE),
            troph = mean(troph, na.rm = TRUE),
            k = mean(k, na.rm = TRUE),
            depth_max = mean(depth_max, na.rm = TRUE),
            depth_min = mean(depth_min, na.rm = TRUE),
            temp_min = mean(temp_pref_min, na.rm = TRUE),
            temp_max = mean(temp_pref_max, na.rm = TRUE),
            tl_max = mean(tl_max, na.rm = TRUE),
            gen_time = mean(generation_time, na.rm = TRUE),
            mean_length = mean(mean_length_cm, na.rm = TRUE),
            mean_weight = mean(mean_weight_g, na.rm = TRUE),
            species_richness = mean(species_richness, na.rm = TRUE),
            species_evenness = mean(species_evenness, na.rm = TRUE),
            sample_depth = mean(depth, na.rm = TRUE),
            sample_temp = mean(temp_c, na.rm = TRUE),
            sample_cond = mean(cond, na.rm = TRUE),
            sample_ph = mean(ph, na.rm = TRUE),
            sample_salinity = mean(sal_ppt, na.rm = TRUE),
            sample_do2 = mean(do2, na.rm = TRUE),
            .groups = "drop"
      ) |> 
      mutate(
            abund_m2 = abund / area,
            biomass_m2 = (abund * mean_weight) / area
      )

bio_even_grid_wm <- bio_even_grid |> 
      group_by(year, month, estuary, bay, zone, grid) |> 
      mutate(
            troph_wm = sum(troph * biomass_m2, na.rm = TRUE),
            k_wm = sum(k * biomass_m2, na.rm = TRUE),
            depth_max_wm = sum(depth_max * biomass_m2, na.rm = TRUE),
            depth_min_wm = sum(depth_min * biomass_m2, na.rm = TRUE),
            temp_min_wm = sum(temp_min * biomass_m2, na.rm = TRUE),
            temp_max_wm = sum(temp_max * biomass_m2, na.rm = TRUE),
            tl_max_wm = sum(tl_max * biomass_m2, na.rm = TRUE),
            gen_time_wm = sum(gen_time*biomass_m2, na.rm = TRUE),
            wm_denom = sum(biomass_m2, na.rm = TRUE)
      ) |> 
      ungroup()

bio_even_community_grid <- bio_even_grid_wm |> 
      distinct(year, month, estuary, bay, zone, grid, .keep_all = TRUE) |> 
      mutate(
            troph = troph_wm / wm_denom,
            k = k_wm / wm_denom,
            depth_max = depth_max_wm / wm_denom,
            depth_min = depth_min_wm / wm_denom,
            temp_min = temp_min_wm / wm_denom,
            temp_max = temp_max_wm / wm_denom,
            tl_max = tl_max_wm / wm_denom,
            gen_time = gen_time_wm / wm_denom
      ) |> 
      select(
            year, month, estuary, bay, zone, grid,
            abund_m2, biomass_m2, troph, k, depth_max, depth_min, temp_min, temp_max, tl_max, gen_time,
            species_richness, species_evenness, sample_depth, sample_temp, sample_cond, 
            sample_ph, sample_salinity, sample_do2
      )
glimpse(bio_even_community_grid)

for_detrend <- bio_even_community_grid |> 
      mutate(simple_date = make_date(year=year, month=month, day = 1))
rm(bio_even, bio_even_grid, bio_even_grid_wm)
### list metrics to detrend ---
metrics <- c("biomass_m2", "abund_m2", "tl_max", "k", "gen_time", 
             "depth_max", "depth_min", "temp_min", 
             "temp_max", "troph", "species_richness", "species_evenness")

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

##### removed seasonality because unneccessary but kept for future use if becomes interesting
# ### create season signature function ---
# seasonality_all <- function(df, metrics) {
#       df <- df |> arrange(simple_date)
#       
#       if (nrow(df) < 24) {
#             for (m in metrics) {
#                   df[[paste0("seasonal_", m)]] <- NA
#             }
#             return(df)
#       }
#       
#       for (m in metrics) {
#             ts_data <- ts(df[[m]], frequency = 12, start = c(min(df$year), min(df$month)))
#             fit <- stl(ts_data, s.window = "periodic")
#             df[[paste0("seasonal_", m)]] <- as.numeric(fit$time.series[, "seasonal"])
#       }
#       return(df)
# }

### apply functions and evaluate output ---
ss_detrended <- for_detrend |> 
      group_by(estuary, zone) |> 
      group_split() |> 
      lapply(detrend_all, metrics = metrics) |> 
      bind_rows()

detrended_full <- ss_detrended |> 
      group_by(estuary, zone, grid) |> 
      mutate(grid_obs = n()) |> 
      ungroup() |> 
      group_by(estuary, zone) |> 
      mutate(zone_obs = n()) |> 
      ungroup()

glimpse(detrended_full)

### read out dataset as a csv file ---
write_csv(detrended_full, "local-data/key-datasets/discrete-detrended-forage-fish-small-seine-time-series.csv")
write_csv(detrended_full, "local-data/discrete-detrended-forage-fish-small-seine-time-series.csv")
