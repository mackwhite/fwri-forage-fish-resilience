###project: Forage Fish Resilience
###author(s): MW
###goal(s): combining disturbance events
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, splitstackshape, purrr, zoo, pracma, broom)

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

severity_palette = c("Moderate"="#88C0D0",
                     "Significant"="#EBCB8B",
                     "Severe"="#BF616A",
                     "Extreme"="black")

### read in data and bind -----
heatwaves <- read_csv('local-data/marine-heat-wave-severity.csv')
glimpse(heatwaves)

coldsnaps <- read_csv('local-data/marine-cold-snap-severity.csv')
glimpse(coldsnaps)

fish <- read_csv('local-data/detrended-smallseine-zone-biomass-ts.csv')
glimpse(fish)

### select columns to move forward
mhw <- heatwaves |> 
      select(year, month, estuary, event, mhw_event_id, mhw_start, mhw_end, 
             duration, max_anomaly, mean_anomaly, variance_anomaly, cum_anomaly,
             flag) |> 
      mutate(dist_id = paste(event, mhw_event_id, sep = "_")) |> 
      select(-mhw_event_id) |> 
      rename(event_start = mhw_start,
             event_end = mhw_end,
             abs_anomaly = max_anomaly)
glimpse(mhw)

mcs <- coldsnaps |> 
      select(year, month, estuary, event, coldsnap_event_id, coldsnap_start, coldsnap_end, 
             duration, min_anomaly, mean_anomaly, variance_anomaly, cum_anomaly,
             flag) |> 
      mutate(dist_id = paste(event, coldsnap_event_id, sep = "_")) |> 
      select(-coldsnap_event_id) |> 
      rename(event_start = coldsnap_start,
             event_end = coldsnap_end,
             abs_anomaly = min_anomaly)      
glimpse(mcs)

dist <- rbind(mhw,mcs) |> distinct()

fullj <- full_join(fish, dist)
glimpse(fullj)

### clean up envi ---
### clean environment ---
keep <- c("fullj")
rm(list = setdiff(ls(), keep))
glimpse(fullj)

all <- fullj |> 
      select(estuary, zone, year, month, simple_date, 
             detrended_bm_m2, max_size, gen_time, max_depth, min_depth,
             pref_temp_min, pref_temp_mean, pref_temp_max, trophic_level, 
             benthic_prop, pelagic_prop,
             event, dist_id, event_start, event_end, duration,
             abs_anomaly, mean_anomaly, variance_anomaly, cum_anomaly, 
             flag) |> 
      arrange(estuary, zone, year, month)
glimpse(all)

all1 <- all |> 
      mutate(treatment = case_when(
            !is.na(dist_id) ~ "dist",
            !is.na(lead(dist_id)) & is.na(dist_id) ~ "cont",
            TRUE ~ "other"
      )) |> 
      mutate(dist_id = case_when(
            !is.na(lead(dist_id)) & is.na(dist_id) ~ lead(dist_id),
            TRUE ~ dist_id
      )) |> 
      mutate(event = case_when(
            !is.na(lead(event)) & is.na(event) ~ lead(event),
            TRUE ~ event
      )) |> 
      filter(treatment != "other") |> 
      arrange(estuary, zone, year, month)
glimpse(all1)

all2 <- all1 |> 
      pivot_wider(names_from = treatment, 
                  values_from = c(
                        detrended_bm_m2,
                        max_size,
                        gen_time,
                        max_depth, 
                        min_depth,
                        pref_temp_min,
                        pref_temp_mean,
                        pref_temp_max,
                        trophic_level,
                        benthic_prop,
                        pelagic_prop), 
                  names_glue = "{treatment}_{.value}") |> 
      select(-c(cont_benthic_prop, dist_benthic_prop, cont_pelagic_prop, dist_pelagic_prop)) |> 
      arrange(estuary, zone, year, month) |> 
      group_by(estuary, zone) |> 
      fill(starts_with("cont_"), .direction = "down") |> 
      ungroup() |> 
      select(-c(event_start, event_end, simple_date)) |> 
      filter(!is.na(duration),
             !is.na(zone))
glimpse(all2)

all3 <- all2 |> 
      mutate(resistance = log(dist_detrended_bm_m2/cont_detrended_bm_m2))

resilience0 <- all |> 
      arrange(estuary, zone, simple_date) |> 
      group_by(estuary, zone, dist_id) |> 
      group_split() |> 
      map_dfr(function(sub_df) {
            extreme_dates <- sub_df |> 
                  filter(!is.na(flag)) |> 
                  pull(simple_date)
            
            if (length(extreme_dates) < 2) return(NULL)
            
            map_dfr(seq_len(length(extreme_dates) -1), function(i) {
                  start_date <- extreme_dates[i]
                  end_date <- extreme_dates[i+1]
                  
                  data_interval <- sub_df |> 
                        filter(simple_date >= start_date & simple_date <= end_date)
                  
                  if (nrow(data_interval) < 2) return(NULL)
                      
                  model<- lm(detrended_bm_m2 ~ simple_date, data = data_interval)
                  
                  tidy(model) |> 
                        mutate(
                              estuary = unique(sub_df$estuary),
                              zone = unique(sub_df$zone),
                              start_date = start_date,
                              end_date = end_date,
                              n_obs = nrow(data_interval)
                        )
            })
      })

resilience_all_flags <- all |> 
      arrange(estuary, zone, simple_date) |> 
      group_by(estuary, zone) |> 
      group_split() |> 
      map_dfr(function(sub_df) {
            flagged_dates <- sub_df |> 
                  filter(!is.na(flag)) |> 
                  pull(simple_date)
            
            if (length(flagged_dates) < 2) return(NULL)
            
            map_dfr(seq_len(length(flagged_dates) - 1), function(i) {
                  start_date <- flagged_dates[i]
                  end_date <- flagged_dates[i + 1]
                  
                  data_interval <- sub_df |> 
                        filter(simple_date >= start_date & simple_date <= end_date)
                  
                  if (nrow(data_interval) < 2) return(NULL)
                  
                  model <- lm(detrended_bm_m2 ~ simple_date, data = data_interval)
                  
                  tidy(model) |> 
                        mutate(
                              estuary = unique(sub_df$estuary),
                              zone = unique(sub_df$zone),
                              start_date = start_date,
                              end_date = end_date,
                              n_obs = nrow(data_interval)
                        )
            })
      })

resilience1 <- resilience_all_flags |> 
      mutate(n_obs = case_when(
            start_date == end_date ~ NA,
            TRUE ~ n_obs
      )) |> 
      filter(n_obs > 2 & !is.na(n_obs))

dist_summary <- all |> 
      # filter(!is.na(dist_id))  |> 
      arrange(estuary, zone, year, month, simple_date) |> 
      group_by(estuary, zone, year, month) |> 
      mutate(
            dist_id_new = first(dist_id),
            event_start_new = min(event_start, na.rm = TRUE),
            event_end_new = max(event_end, na.rm = TRUE),
            cum_anomaly_sum = sum(cum_anomaly, na.rm = TRUE)
      ) %>%
      ungroup()

test <- dist_summary |> filter(event_end != event_end_new | event_start != event_start_new)

### mack needs to figure out this resilience code... how can we get it fixed up?
### then move onto recovery, then temporal stability
