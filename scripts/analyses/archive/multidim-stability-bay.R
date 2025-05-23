### Project: Forage Fish Resilience
### Author(s): MW
### Goal(s): Merge final datasets without zone-level resolution
### Date(s): January 2025

# Load necessary libraries
librarian::shelf(tidyverse, FSA, readr, forcats, multcompView, readxl, dplyr, 
                 splitstackshape, purrr, zoo, pracma, vegan, e1071, codyn, lubridate, forecast)

# Utility function
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

# Set palettes
estuary_palette <- c("Apalachicola Bay"="#e6d7b9", "Charlotte Harbor"="#a7c4a0", "Cedar Key"="#64a988",
                     'Northern Indian River'="#89c8d9", "Northeast Florida"="#f2c6b4", 'Tampa Bay'='#dba5a4',
                     "Southern Indian River"="#bfaed9")

heat_wave_palette <- c("Moderate"="lightgrey", "Significant"="pink", "Severe"="#BF616A", "Extreme"="darkred")

cold_snap_palette <- c("Moderate"="lightgrey", "Significant"="#88C0D0", "Severe"="#4682B4", "Extreme"="darkblue")

# Read and summarize discrete data without zone


# Discrete forage fish data

discrete <- read_csv("local-data/key-datasets/discrete_foragefish_final.csv") |> 
      group_by(bay, year, month) |> 
      summarize(across(detrended_bm_m2:seasonal_pelagic_prop, \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# Read in disturbance data

hurricanes <- read_xlsx('local-data/hurricane-intensity-information.xlsx') |> 
      rename(bay = estuary) |> 
      mutate(start_date = make_date(year = year, month = month, day = 1),
             end_date = make_date(year = year, month = month, day = 2),
             event = "hurricane") |> 
      select(-c('year', 'month')) |> 
      select(bay, event, storm_name = `storm name`, start_date, end_date)

coldsnaps <- read_csv('local-data/marine-cold-snap-severity.csv') |> 
      rename(storm_name = coldsnap_event_id, start_date = coldsnap_start, end_date = coldsnap_end) |> 
      mutate(storm_name = paste(storm_name, "CS", sep = "_")) |> 
      mutate(event = "coldsnap") |> 
      select(bay, event, storm_name, start_date, end_date)

heatwaves <- read_csv('local-data/marine-heat-wave-severity.csv') |> 
      rename(storm_name = mhw_event_id, start_date = mhw_start, end_date = mhw_end) |> 
      mutate(storm_name = paste(storm_name, "HW", sep = "_")) |> 
      mutate(event = "mhw") |> 
      select(bay, event, storm_name, start_date, end_date)

disturbances <- bind_rows(hurricanes, coldsnaps, heatwaves) |> 
      rename(event_id = storm_name) |> 
      filter(between(start_date, ymd("1996-01-01"), ymd('2023-12-31')))

# Calculate baseline, resistance, recovery at bay level
new <- disturbances |> 
      mutate(
            baseline = map2(bay, start_date, \(x, date) 
                            discrete |> filter(bay == x, between(ym(paste(year, month, sep = '-')), date %m-% months(12), date %m-% months(1)))),
            b_bio = map(baseline, \(df) df |> summarize(mean = mean(detrended_bm_m2),
                                                        lci = quantile(detrended_bm_m2, 0.025),
                                                        uci = quantile(detrended_bm_m2, 0.975))),
            resist_bio = map2(bay, end_date, \(x, date) 
                              discrete |> filter(bay == x, between(ym(paste(year, month, sep = '-')), date %m+% months(0), date %m+% months(1)))),
            recover_bio = pmap(list(bay, b_bio, end_date), \(x, df, date) {
                  rec <- discrete |> filter(bay == x) |> 
                        mutate(lci = df$lci, uci = df$uci) |> 
                        filter(ym(paste(year, month, sep = "-")) > date, 
                               between(detrended_bm_m2, lci, uci))
                  if (nrow(rec) == 0) return(tibble())
                  rec |> filter(ym(paste(year, month, sep = "-")) == min(ym(paste(year, month, sep = "-"))))
            })
      )

# Clean and unnest
new_clean <- new |> 
      select(-baseline) |> 
      mutate(year = year(start_date), month = month(start_date)) |> 
      filter(map_int(b_bio, nrow) > 0, map_int(resist_bio, nrow) > 0, map_int(recover_bio, nrow) > 0) |> 
      unnest(c(b_bio, resist_bio, recover_bio), names_sep = "_") |> 
      select(bay, event, event_id, year, month, b_bio_mean, b_bio_lci, b_bio_uci,
             resist_bio_year, resist_bio_month, resist_bio_detrended_bm_m2,
             recover_bio_year, recover_bio_month, recover_bio_detrended_bm_m2)

# Calculate resistance, recovery, and join with stressors
met <- new_clean |> 
      mutate(resist = log1p(resist_bio_detrended_bm_m2 / b_bio_mean),
             abs_resist = abs(resist),
             resist_date = make_date(resist_bio_year, resist_bio_month, 1),
             recover_date = make_date(recover_bio_year, recover_bio_month, 1),
             recover_mos = lubridate::interval(resist_date, recover_date) %/% months(1)) |> 
      filter(!is.nan(resist))

# Read stressor attributes
canes <- read_csv('local-data/hurricane-intensity-information.csv')
cs <- read_csv('local-data/marine-cold-snap-severity.csv')
hw <- read_csv('local-data/marine-heat-wave-severity.csv')

canes_clean <- canes |> 
      rename(event_id = `storm name`, hurricane_category = category, hurricane_wind_kts = windspeed)  |> 
      mutate(event = "hurricane")  |> 
      select(bay = estuary, event_id, event, hurricane_category, hurricane_wind_kts) |> 
      mutate(hurricane_category = case_when(hurricane_category == 0 ~ 0.5, TRUE ~ hurricane_category))

cs_clean <- cs |> 
      rename(event_id = coldsnap_event_id) |> 
      mutate(event = "coldsnap", event_id = paste(event_id, "CS", sep = "_")) |> 
      rename(max_anomaly = min_anomaly) |> 
      select(bay, event_id, event, duration, max_anomaly, mean_anomaly, cum_anomaly)

hw_clean <- hw |> 
      rename(event_id = mhw_event_id) |> 
      mutate(event = "mhw", event_id = paste(event_id, "HW", sep = "_")) |> 
      select(bay, event_id, event, duration, max_anomaly, mean_anomaly, cum_anomaly)

stressors_combined <- bind_rows(canes_clean, cs_clean, hw_clean)

met_dist <- met |> 
      left_join(stressors_combined, by = c("bay", "event_id", "event")) |> 
      mutate(
            recover_mos_adj = if_else(recover_mos == 0, 0.1, recover_mos),
            lrr = log((abs(resist_bio_detrended_bm_m2 - b_bio_mean) + b_bio_mean) / b_bio_mean),
            resistance_raw = -1 * lrr,
            resilience_raw = log((abs(recover_bio_detrended_bm_m2 - b_bio_mean) + b_bio_mean) / 
                                       (b_bio_mean * recover_mos_adj))
      )

# Calculate standardized resistance per stressor
stressor_vars <- c("hurricane_wind_kts", "hurricane_category", "duration", "max_anomaly", "mean_anomaly", "cum_anomaly")
for (var in stressor_vars) {
      new_name <- paste0("resistance_", var)
      met_dist[[new_name]] <- with(met_dist, ifelse(!is.na(get(var)), -1 * log(lrr / get(var)), NA_real_))
}

# Pivot to long format
met_dist_long <- met_dist |> 
      pivot_longer(
            cols = starts_with("resistance_") & !starts_with("resistance_raw"),
            names_to = "stressor_type",
            names_prefix = "resistance_",
            values_to = "resistance"
      ) |> 
      filter(!is.na(resistance)) |> 
      mutate(recovery_months_inflated = if_else(recover_mos_adj == 0.1, "yes", "no"))

met_dist_long |> 
      # filter(recovery_months_inflated != "yes") |> 
      filter(resistance_raw >= -1.5) |> 
      ggplot(aes(x = resilience_raw, y = resistance, color = stressor_type)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~bay)
