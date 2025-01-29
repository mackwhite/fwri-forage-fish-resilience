###project: Forage Fish Resilience
###author(s): MW
###goal(s): defining MHW using Hobday et al 2016
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, splitstackshape, purrr, zoo, pracma)

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
temp <- read_csv('local-data/archive/temp-rainfall-data.csv') |> 
      filter(metric == "max_temp") |> 
      mutate(jday = yday(date)) |> 
      group_by(bay, estuary, jday) |> 
      mutate(dmean = mean(value, na.rm = TRUE),
             dsd = sd(value, na.rm = TRUE)) |> 
      ungroup() |> 
      group_by(bay, estuary, month) |> 
      mutate(mmean = mean(value, na.rm = TRUE),
             msd = sd(value, na.rm = TRUE)) |> 
      ungroup() |> 
      select(-c('is_filled', 'daymonth')) |> 
      select(bay, estuary, date, year, month, day, jday, 
             metric, value, dmean, dsd, mmean, msd)

nacheck(temp)
glimpse(temp)

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

### visualize climatological means ---

temp |> 
      select(bay, estuary, jday, dmean) |> 
      distinct() |> 
      ggplot(aes(x = jday, y = dmean, color = estuary)) + 
      geom_line(linewidth = 2) +
      labs(x = "Julian Day", y = "Climatological Mean Temperature (°C)", color = 'Estuary') +
      theme_bw() +
      facet_wrap(~estuary) +
      scale_color_manual(values = estuary_palette)+
      # scale_x_continuous(breaks = c(1960,1970,1980,1990,2000,2010,2020)) +
      # scale_y_continuous(breaks = c(0.0,0.3,0.6,0.9,1.2)) +
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"), 
            axis.title.x = element_text(size = 15, face = "bold", colour = "black"),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none",
            # legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 12, face = "bold", colour = "black", hjust = 0.5))

# ggsave('figs/climatological-maximum-means.png',
#        dpi = 800,
#        units= 'in',
#        height = 6,
#        width = 6.5)

### defining thresholds for 90th percentile evaluation ---
threshold <- temp |> 
      group_by(bay, estuary, month) |> 
      group_by(bay, estuary, jday) |> 
      mutate(
            threshold_90 = quantile(value, 0.90, na.rm = TRUE),
            threshold_95 = quantile(value, 0.95, na.rm = TRUE),
            threshold_99 = quantile(value, 0.99, na.rm = TRUE) 
      ) |> 
      mutate(flag = case_when(
            value >= threshold_99 ~ "Extreme",
            value >= threshold_95 ~ "Severe",
            value >= threshold_90 ~ "Significant",
            TRUE ~ "Moderate"
      )) |> 
      mutate(threshold_breached = if_else(
            value >= threshold_90,
            "yes", 
            "no", 
      )) |> 
      ungroup() |> 
      group_by(bay, estuary) |> 
      arrange(bay, estuary, date) |> 
      mutate(event_id= data.table::rleid(threshold_breached, by = bay)) |> 
      ungroup() |> 
      group_by(bay, estuary, event_id) |> 
      mutate(run_length = n(),
             mhw = if_else(
                   ### if threshold was greater than 90th percentile AND duration is greater than or equal to five days
                   threshold_breached == "yes" & run_length >= 5,
                   "mhw",
                   "normal"
             )) |> 
      ungroup() |> 
      arrange(bay, estuary, date)

nacheck(threshold)
glimpse(threshold)

threshold1 <- threshold |> 
      # group_by(bay, estuary) |> 
      arrange(bay, date) |> 
      # Flag days where the threshold was breached
      mutate(mhw_event = ifelse(mhw == "mhw", 1, 0)) |> 
      # Assign initial event ID using run-length encoding
      mutate(mhw_event_id = case_when(
            mhw == "mhw" ~ event_id,
            mhw == "normal" ~ NA_integer_
      )) |> 
      ungroup() |> 
      arrange(bay, date)

threshold2 <- threshold1 |> 
      group_by(bay, mhw_event_id) |>
      mutate(mhw_start = case_when(
            !is.na(mhw_event_id) ~ min(date),
            is.na(mhw_event_id) ~ NA_Date_),
            mhw_end = case_when(
                  !is.na(mhw_event_id) ~ max(date),
                  is.na(mhw_event_id) ~ NA_Date_))

threshold3 <- threshold2 |> 
      filter(!is.na(mhw_event_id)) |> 
      select(bay, estuary, mhw_event_id, mhw_start, mhw_end) |> 
      distinct()

threshold4 <- threshold3 |> 
      group_by(bay) |> 
      arrange(bay, mhw_start) |> 
      mutate(gap_days = as.numeric(difftime(mhw_start, lag(mhw_end), units = "days"))) |> 
      mutate(mhw_event_id2 = if_else(
            gap_days > 0 & gap_days <= 2,
            lag(mhw_event_id),
            mhw_event_id
      )) |> 
      mutate(diff = mhw_event_id-mhw_event_id2)

threshold5 <- threshold4 |> 
      arrange(bay, mhw_event_id) |> 
      group_by(bay, mhw_event_id2) |> 
      mutate(mhw_end2 = case_when(
            lead(diff) >= 1 ~ lead(mhw_end),
            TRUE ~ mhw_end),
            mhw_end = mhw_end2) |> 
      select(bay, estuary, mhw_event_id2, mhw_start, mhw_end) |> 
      rename(mhw_event_id = mhw_event_id2) |> 
      arrange(bay, mhw_start, mhw_event_id) |> 
      group_by(mhw_event_id, bay) |> 
      slice_head(n=1) |> 
      ungroup()

threshold5_expand <- threshold5 |> 
      mutate(date_seq = map2(mhw_start, mhw_end, ~seq(.x, .y, by = 'day'))) |> 
      unnest(date_seq) |> 
      rename(date = date_seq) |> 
      group_by(mhw_event_id) |> 
      mutate(duration = n()) |>
      group_by(bay) |> 
      mutate(duration = if_else(
            is.na(mhw_event_id),
            sum(is.na(mhw_event_id)),
            duration)) |>
      ungroup() |> 
      mutate(event = "mhw") |> 
      select(-mhw_event_id)

nacheck(threshold5_expand)
glimpse(threshold5_expand)
glimpse(threshold2)

threshold6 <- threshold2 |> 
      select(-mhw_start, -mhw_end) |> 
      left_join(threshold5_expand, by = c("bay", "estuary", "date")) |> 
      arrange(bay, date) |> 
      ungroup()

glimpse(threshold6)
nacheck(threshold6)

mhw <- threshold6 |> 
      select(bay, estuary, date, jday, 
             metric, value, dmean, dsd, mmean, msd, threshold_90, 
             threshold_breached, event, mhw_start, mhw_end, duration) |> 
      mutate(year = year(date),
             month = month(date),
             flag = case_when(
                   duration >= 35 ~ 'remove',
                   TRUE ~ 'safe'
             )) |> 
      filter(flag == "safe") |> 
      select(-flag)

nacheck(mhw)
glimpse(mhw)

### clean environment before next step ---
keep <- c("mhw","nacheck")
rm(list = setdiff(ls(), keep))

mhw1 <- mhw |> 
      group_by(bay, mhw_start, mhw_end) |> 
      mutate(mhw_event_id = if_else(
                  event == "mhw",                            
                  paste0(bay, "_", cur_group_id()),          
                  NA_character_)) |> 
      ungroup()

mhw2 <- mhw1 |> 
      filter(!is.na(mhw_event_id)) |> 
      group_by(bay, mhw_event_id) |> 
      mutate(max = max(value),
             mean = mean(value),
             mean_90 = mean(threshold_90),
             daily_anomaly = value - threshold_90,
             max_anomaly = max-mean_90,
             mean_anomaly = mean-mean_90,
             variance_anomaly = sd(value),
             cum_anomaly = sum(daily_anomaly)) |> 
      ungroup()
      
nacheck(mhw2)
glimpse(mhw2)

mhw3 <- mhw2 |> 
      group_by(bay, estuary, mhw_event_id) |> 
      summarize(auc = trapz(as.numeric(date), mean_anomaly),
                duration = duration,
                start_date = mhw_start,
                end_date = mhw_end) |> 
      ungroup() |> 
      distinct() |> 
      group_by(bay, estuary) |> 
      mutate(
            auc_90 = quantile(auc, 0.90, na.rm = TRUE),
            auc_95 = quantile(auc, 0.95, na.rm = TRUE),
            auc_99 = quantile(auc, 0.99, na.rm = TRUE) 
      ) |> 
      mutate(flag = case_when(
            auc >= auc_99 ~ "Extreme",
            auc >= auc_95 ~ "Severe",
            auc >= auc_90 ~ "Significant",
            TRUE ~ "Moderate"
      )) |> 
      ungroup()

nacheck(mhw3)
glimpse(mhw3)

mhw_severity <- mhw1 |> 
      left_join(mhw3, by = c("bay", "estuary", "mhw_event_id", "duration"))

mhw_severity2 <- mhw2 |> 
      left_join(mhw3, by = c("bay", "estuary", "mhw_event_id", "duration"))

# write_csv(mhw_severity, 'local-data/marine-heat-wave-timeseries.csv')
# write_csv(mhw_severity2, 'local-data/marine-heat-wave-severity.csv')