###project: Forage Fish Resilience
###author(s): MW
###goal(s): resistance and resilience calculations
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

discrete <- read_csv("local-data/key-datasets/discrete-detrended-forage-fish-small-seine-time-series.csv") |> 
      mutate(simple_date = make_date(year = year, month = month, day = 1)) |> 
      group_by(bay, zone, year, month) |> 
      summarize(across(detrended_biomass_m2:detrended_species_evenness, \(x) mean(x)), .groups = "drop") |> 
      # summarize(across(anomaly_biomass_m2:anomaly_species_evenness, \(x) mean(x)), .groups = "drop") |> 
      # summarize(across(biomass_m2:species_evenness, \(x) mean(x)), .groups = "drop") |> 
      filter(year >= 1996) |> 
      mutate(detrended_bm_m2 = detrended_biomass_m2 + 2) |> 
      filter(!is.na(detrended_bm_m2)) |> 
      select(-detrended_biomass_m2) |> 
      arrange(bay, zone, year, month)

filled_ts <- discrete |> 
      mutate(
            month = as.integer(month),
            year = as.integer(year)
      ) |> 
      complete(
            bay, zone,
            year = full_seq(year, 1),
            month = 1:12
      ) |> 
      arrange(bay, zone, year, month) |> 
      mutate(simple_date = as.Date(sprintf("%04d-%02d-01", year, month))) |> 
      group_by(bay, zone) |> 
      mutate(
            detrended_bm_m2_interp = na.approx(
                  detrended_bm_m2,
                  x = simple_date,
                  na.rm = FALSE 
            )
      ) |> 
      ungroup() |> 
      filter(!is.na(detrended_bm_m2_interp)) |> 
      select(bay, zone, year, month, simple_date, detrended_bm_m2_interp) |> 
      arrange(bay, zone, year, month)

discrete <- filled_ts |> 
      rename(detrended_bm_m2 = detrended_bm_m2_interp) |> 
      mutate(detrended_bm_m2 = abs(detrended_bm_m2))

hurricanes <- read_xlsx('local-data/hurricane-intensity-information.xlsx') |> 
      rename(bay = estuary) |> 
      mutate(start_date = make_date(year = year, month = month, day = 1) ,
             end_date = make_date(year = year, month = month, day = 2),
             event = "hurricane") |> 
      select(-c('year', 'month')) |> 
      select(bay, event, storm_name = `storm name`, start_date, end_date)

coldsnaps <- read_csv('local-data/marine-cold-snap-severity.csv') |> 
      rename(storm_name = coldsnap_event_id,
             start_date = coldsnap_start,
             end_date = coldsnap_end) |> 
      mutate(storm_name = paste(storm_name, "CS", sep = "_")) |> 
      select(bay, event, storm_name, start_date, end_date)
      
heatwaves <- read_csv('local-data/marine-heat-wave-severity.csv') |> 
      rename(storm_name = mhw_event_id,
             start_date = mhw_start,
             end_date = mhw_end) |> 
      mutate(storm_name = paste(storm_name, "HW", sep = "_")) |> 
      select(bay, event, storm_name, start_date, end_date)

disturbances = bind_rows(hurricanes, coldsnaps, heatwaves) |> 
      rename(event_id = storm_name) |> 
      filter(between(start_date, ymd("1996-01-01"), ymd('2023-12-31')))

### notes from chatting with ryan
### look into weighting the traits prior to detrending (or weight with detrended biomass)
### how do you measure baseline?
## six month leading average (six months prior to disturbance event [me] + 95% CI for return to baseline [stevens et al. 2016]) 
### how do you determine recovery?
## stevens et al 2016 - 95% ci of six month leading average

### map it out ----
new <- disturbances |> 
      mutate(baseline = map2(bay, start_date, \(x, date) 
                             discrete |> filter(bay == x,
                                                between(ym(paste(year,month,sep = '-')), 
                                                        date %m-% months(3),
                                                        date %m-% months(1)))),
             b_bio = map(baseline, \(df) df |> group_by(zone) |> 
                               summarize(mean = mean(detrended_bm_m2),
                                         lci = quantile(detrended_bm_m2, 0.025),
                                         uci = quantile(detrended_bm_m2, 0.975))),
             ### un-nest the b_bio column to have zone for each one (only problem is you'll have to add zone to each to have it separate)
             ### potentially revert 50 obs min for this analysis - not as important as it is for stability metrics
             resist_bio = map2(bay, end_date, \(x, date) 
                            discrete |> filter(bay == x,
                                               between(ym(paste(year,month,sep = '-')), 
                                                       date %m+% months(0),
                                                       date %m+% months(1)))),
             recover_bio = pmap(list(bay, b_bio, end_date), \(x, df, date) {
                   rec <- discrete |>
                         filter(bay == x) |>
                         left_join(df, by = "zone") |>
                         filter(
                               ym(paste(year, month, sep = "-")) > date,
                               between(detrended_bm_m2, lci, uci)
                         )
                   ### return empty tibble safely
                   if (nrow(rec) == 0) return(tibble())  
                   
                   rec |>
                         group_by(zone) |>
                         filter(
                               ym(paste(year, month, sep = "-")) == min(ym(paste(year, month, sep = "-")))
                         )
             })
             ### created nested data frame between resist and recover date to get resilience, mapping linear regression
             ### also need to extract bio value for that zone and stuff - dont care about all the trait stuff (so, pmap_dbl)
      )

new_clean <- new |>
      select(-baseline) |>
      mutate(
            year = year(start_date),
            month = month(start_date)
      ) |>
      filter(map_int(b_bio, nrow) > 0,
             map_int(resist_bio, nrow) > 0,
             map_int(recover_bio, nrow) > 0) |>  # keep only non-empty nested data
      unnest(b_bio, names_sep = "_") |>
      unnest(resist_bio, names_sep = "_") |>
      unnest(recover_bio, names_sep = "_") |>
      mutate(
            zone_match = b_bio_zone == resist_bio_zone & resist_bio_zone == recover_bio_zone
      ) |> 
      select(bay, event, event_id, start_date, end_date, year, month,
             b_bio_zone, b_bio_mean, b_bio_lci, b_bio_uci,
             resist_bio_bay, resist_bio_zone, resist_bio_year, resist_bio_month, resist_bio_detrended_bm_m2, 
             recover_bio_bay, recover_bio_zone, recover_bio_year, recover_bio_month, recover_bio_detrended_bm_m2, zone_match) |> 
      distinct()

newclean1 <- new_clean |> filter(zone_match == TRUE) |> 
      mutate(zone = b_bio_zone) |> 
      arrange(bay, year, month, zone, event) |> 
      select(bay, year, month, zone, event, everything())

newclean2 <- newclean1 |> 
      select(-c('start_date', 'end_date', 'b_bio_zone', 'resist_bio_bay', 'resist_bio_zone', 'recover_bio_bay', 
                'recover_bio_zone', 'zone_match'))
glimpse(newclean2)

### calculating resistance and recovery ----

met <- newclean2 |> 
      mutate(resist_date = make_date(resist_bio_year, resist_bio_month, 1),
             recover_date = make_date(recover_bio_year, recover_bio_month, 1),
             recover_mos = lubridate::interval(resist_date, recover_date) %/% months(1),
             recover_days = case_when(
                   recover_mos == 0 ~ 30,
                   recover_mos >=1 ~ recover_mos*30
             ),
             recover_mos = case_when(
                   recover_mos == 0 ~ 0.1,
                   TRUE ~ recover_mos
             ))

### read in disturbance data again ---
canes <- read_csv('local-data/hurricane-intensity-information.csv')
cs <- read_csv('local-data/marine-cold-snap-severity.csv')
hw <- read_csv('local-data/marine-heat-wave-severity.csv')

canes_clean <- canes |> 
      rename(event_id = `storm name`,
             hurricane_category = category,
             hurricane_wind_kts = windspeed)  |> 
      mutate(event = "hurricane")  |> 
      select(bay = estuary, event_id, event, hurricane_category, hurricane_wind_kts) |> 
      mutate(hurricane_category = case_when(
            hurricane_category == 0 ~ 0.5,
            TRUE ~ hurricane_category
      ))

cs_clean <- cs  |> 
      rename(event_id = coldsnap_event_id)  |> 
      mutate(event = "coldsnap")  |> 
      mutate(event_id = paste(event_id, "CS", sep = "_")) |> 
      rename(max_anomaly = min_anomaly) |> 
      select(bay, event_id, event, duration, max_anomaly, mean_anomaly, cum_anomaly)

hw_clean <- hw  |> 
      rename(event_id = mhw_event_id)  |> 
      mutate(event = "mhw")  |> 
      mutate(event_id = paste(event_id, "HW", sep = "_")) |> 
      select(bay, event_id, event, duration, max_anomaly, mean_anomaly, cum_anomaly)

stressors_combined <- bind_rows(canes_clean, cs_clean, hw_clean)

scale_vars <- c("hurricane_category", "hurricane_wind_kts", "duration",
                "max_anomaly", "mean_anomaly", "cum_anomaly")

# Min-max scaling function that handles NAs
min_max_scale <- function(x) {
      rng <- range(x, na.rm = TRUE)
      scaled <- (x - rng[1]) / (rng[2] - rng[1])
      scaled * 9 + 1
}

met_dist <- met |> 
      left_join(stressors_combined, by = c("bay", "event_id", "event")) |> 
      mutate(across(all_of(scale_vars), min_max_scale))

### standardized for stressor (only) below - this code works ----
stressor_vars <- c(
      "hurricane_wind_kts",
      # "hurricane_category",
      # "duration",
      # "max_anomaly",
      # "mean_anomaly",
      "cum_anomaly"
)

met_dist <- met_dist |>
      mutate(

            # Calculate LRR once
            lrr = log((((abs(resist_bio_detrended_bm_m2 - b_bio_mean)) + b_bio_mean)) / b_bio_mean),

            # Calculate resilience
            observed_resilience = log(((((abs(resist_bio_detrended_bm_m2 - b_bio_mean)) + b_bio_mean)) / b_bio_mean) / (recover_mos))
      )

# Add resistance for each stressor
for (var in stressor_vars) {
      new_name <- paste0("resistance_", var)
      met_dist[[new_name]] <- with(met_dist, ifelse(!is.na(get(var)),
                                                    -1 * log(lrr / get(var)),
                                                    NA_real_))
}

met_dist_long <- met_dist |>
      pivot_longer(
            cols = starts_with("resistance_"),
            names_to = "stressor_type",
            names_prefix = "resistance_",
            values_to = "resistance"
      ) |>
      filter(!is.na(resistance)) |>
      # filter(resistance >= 0) |> 
      mutate(recovery_inflated = case_when(
            recover_mos == 0.1 ~ 'yes',
            TRUE ~ 'no'
      ))

met_dist_long |> 
      # filter(recovery_inflated == 'yes') |>
      ggplot(aes(x = observed_resilience, y = resistance, color = event)) +
      geom_point()

final <- met_dist_long |> filter(resistance >= 0)

write_csv(met_dist_long, 'local-data/key-datasets/resistance-resilience-calcs.csv')
glimpse(met_dist_long)
