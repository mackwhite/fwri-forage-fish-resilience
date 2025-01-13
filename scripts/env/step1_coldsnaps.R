###project: Forage Fish Resilience
###author(s): MW
###goal(s): cold snap severity calculations 
## following Boucek & Rehage 2014 global change biology 
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

### read in data and bind -----
ap <- read_csv('local-data/AP_legacy_data.csv') |> 
      ### add estuary code
      mutate(Bay = 'AP') |> 
      ### clean up column names - can't stand it
      janitor::clean_names() |> 
      ### generate date column
      mutate(date = ymd(paste(year, month, day, sep = "-"))) |> 
      ### filter out the february 29 (ie leap years) in data that are invalid and throwing parsing errors
      filter(!is.na(date)) |> 
      ### generate daymonth column
      mutate(daymonth = paste(month, day, sep = "-"))

ck <- read_csv('local-data/CK_legacy_data.csv') |> 
      mutate(Bay = 'CK')|> 
      ### clean up column names - can't stand it
      janitor::clean_names() |> 
      ### generate date column
      mutate(date = ymd(paste(year, month, day, sep = "-"))) |> 
      ### filter out the february 29 (ie leap years) in data that are invalid and throwing parsing errors
      filter(!is.na(date)) |> 
      ### generate daymonth column
      mutate(daymonth = paste(month, day, sep = "-"))


tb <- read_csv('local-data/TB_legacy_data.csv') |> 
      mutate(Bay = 'TB')|> 
      ### clean up column names - can't stand it
      janitor::clean_names() |> 
      ### generate date column
      mutate(date = ymd(paste(year, month, day, sep = "-"))) |> 
      ### filter out the february 29 (ie leap years) in data that are invalid and throwing parsing errors
      filter(!is.na(date)) |> 
      ### generate daymonth column
      mutate(daymonth = paste(month, day, sep = "-"))

ch <- read_csv('local-data/CH_legacy_data.csv') |> 
      mutate(Bay = 'CH') |> 
      ### clean up column names - can't stand it
      janitor::clean_names() |> 
      ### generate date column
      mutate(date = ymd(paste(year, month, day, sep = "-"))) |> 
      ### filter out the february 29 (ie leap years) in data that are invalid and throwing parsing errors
      filter(!is.na(date)) |> 
      ### generate daymonth column
      mutate(daymonth = paste(month, day, sep = "-"))

tq <- read_csv('local-data/TQ_legacy_data.csv')|> 
      mutate(Bay = 'TQ') |> 
      janitor::clean_names() |> 
      mutate(date = ymd(paste(year, month, day, sep = "-"))) |> 
      filter(!is.na(date)) |> 
      mutate(daymonth = paste(month, day, sep = "-"))

ir <- read_csv('local-data/IR_legacy_data.csv')|> 
      mutate(Bay = 'IR') |> 
      ### clean up column names - can't stand it
      janitor::clean_names() |> 
      ### generate date column
      mutate(date = ymd(paste(year, month, day, sep = "-"))) |> 
      ### filter out the february 29 (ie leap years) in data that are invalid and throwing parsing errors
      filter(!is.na(date)) |> 
      ### generate daymonth column
      mutate(daymonth = paste(month, day, sep = "-"))

jx <- read_csv('local-data/JX_legacy_data.csv') |> 
      mutate(Bay = 'JX') |> 
      ### clean up column names - can't stand it
      janitor::clean_names() |> 
      ### generate date column
      mutate(date = ymd(paste(year, month, day, sep = "-"))) |> 
      ### filter out the february 29 (ie leap years) in data that are invalid and throwing parsing errors
      filter(!is.na(date)) |> 
      ### generate daymonth column
      mutate(daymonth = paste(month, day, sep = "-"))

### join data together ---
dfs <- rbind(ap, ch, ck, ir, jx, tb, tq) |> 
      ### remove unnecessary columns
      select(-coopid, -mean_temp) |>
      ### order data in way that makes sense
      select(bay, date, daymonth, everything())

### check out the data
glimpse(dfs)

### clean environment before next step ---
keep <- c("dfs", "nacheck")
rm(list = setdiff(ls(), keep))

### separate data by metrics of interest and NA-fill for interpolation -----
rainfall <- dfs |> 
      select(-c(min_temp, max_temp)) |> 
      ### replace missing value code -99.99 with NA
      mutate(across(where(is.numeric), ~ na_if(., -99.99))) |> 
      ### replace missing value code -99.9 with NA
      mutate(across(where(is.numeric), ~ na_if(., -99.9))) |> 
      ### remove NAs - i.e., before data were recorded for this particular metric
      na.omit() |> 
      ### convert imperial to metric and set up for joins
      mutate(rainfall = precipitation*2.54,
             metric = "rainfall", 
             value = rainfall) |> 
      ### select columns to move forward with
      select(bay, date, year, month, day, daymonth, metric, value)

min_temp <- dfs |> 
      select(-c(precipitation, max_temp)) |> 
      ### replace missing value code -99.99 with NA
      mutate(across(where(is.numeric), ~ na_if(., -99.99))) |> 
      ### replace missing value code -99.9 with NA
      mutate(across(where(is.numeric), ~ na_if(., -99.9))) |> 
      ### remove NAs - i.e., before data were recorded for this particular metric
      na.omit() |> 
      ### convert imperial to metric and set up for joins
      mutate(min_temp = (min_temp - 32) * 5 / 9,
             metric = "min_temp", 
             value = min_temp) |> 
      ### select columns to move forward with
      select(bay, date, year, month, day, daymonth, metric, value)

max_temp <- dfs |> 
      select(-c(precipitation, min_temp)) |> 
      ### replace missing value code -99.99 with NA
      mutate(across(where(is.numeric), ~ na_if(., -99.99))) |> 
      ### replace missing value code -99.9 with NA
      mutate(across(where(is.numeric), ~ na_if(., -99.9))) |> 
      ### remove NAs - i.e., before data were recorded for this particular metric
      na.omit() |> 
      ### convert imperial to metric and set up for joins
      mutate(max_temp = (max_temp - 32) * 5 / 9,
             metric = "max_temp", 
             value = max_temp) |> 
      ### select columns to move forward with
      select(bay, date, year, month, day, daymonth, metric, value)

dfs1 <- rbind(rainfall, max_temp, min_temp) |> 
      filter(year %in% 1954:2023) |> 
      arrange(bay, metric, date)

# yeartest <- dfs1 |> group_by(bay) |> summarize(max_yr = max(year), min_yr = min(year))

### clean environment before next step ---
keep <- c("dfs1", "nacheck")
rm(list = setdiff(ls(), keep))

##### na-fill the data -----
all_complete <- dfs1 |> 
      ### complete data sequence for each of the different metrics
      arrange(bay, metric, date) |> 
      group_by(bay, metric) |> 
      complete(date = seq.Date(min(date, na.rm = TRUE), 
                                  max(date, na.rm = TRUE), 
                                  by = 'day')) |> 
      ungroup() |> 
      arrange(bay, metric, date) |> 
      mutate(value = na.approx(value, na.rm = FALSE)) |> 
      ungroup() |> 
      ### fill everything up and down to account for NAs as a result of date sequence
      mutate(is_filled = ifelse(is.na(value), "filled", "real")) |> 
      fill(everything(), .direction = "downup") |> 
      ungroup() |> 
      ### identify consecutive days where data is filled
      arrange(bay, metric, date) |> 
      group_by(bay, metric) |> 
      mutate(run_id = data.table::rleid(is_filled)) |> 
      ungroup() |> 
      ### calculate and remove filled data for more than two weeks of data filled
      group_by(bay, metric, run_id) |> 
      mutate(run_length = n()) |> 
      ungroup() |> 
      filter(!(is_filled == 'filled' & run_length > 14)) |> 
      ### remove identifying columns no longer needed
      select(-c(run_id, run_length))

### check for NAs ---
nacheck(all_complete)
glimpse(all_complete)

### clean environment before next step ---
keep <- c("all_complete", "nacheck")
rm(list = setdiff(ls(), keep))

bay_names <- read_csv('local-data/for-joins/bay-to-estuary.csv') |> 
      janitor::clean_names()

severity <- all_complete |>
      left_join(bay_names, by = "bay")

# write_csv(severity, "local-data/temp-rainfall-data.csv")
      
### calculate severity index for minimum temperatures -----

min <- severity |> filter(metric == 'min_temp')
glimpse(min)

### calculate long-term avg, min, and sd to identify cold-fronts (Boucek & Rehage 2014) ---
lt_stats <- min |> 
      group_by(bay, estuary) |> 
      summarize(lt_avg = mean(value, na.rm = TRUE),
                lt_ext = min(value, na.rm = TRUE),
                lt_sd = sd(value, na.rm = TRUE))

### join and identify cold fronts of instances when minimum daily air temperature
### dropped to or below two standard deviations from the mean minimum daily temperature
### of the entire time series ---

events <- min |> left_join(lt_stats, by = c('bay', 'estuary')) |>
      mutate(event = if_else(
             value <= lt_avg - 2 * lt_sd,
             TRUE, 
             FALSE
      )) |> 
      select(-daymonth)

glimpse(events)
nacheck(events)

### generate severity indices according to Boucek & Rehage, 2014 ---

event_severity <- events |> 
      filter(event == TRUE) |> 
      mutate(severity = 1 / (value + (lt_avg - lt_ext))) |> 
      arrange(bay, estuary, date) |> 
      select(bay, estuary, everything())

glimpse(event_severity)
nacheck(event_severity)
# write_csv(event_severity, "local-data/cold-snap-severity.csv")

### summarize event severity and frequency across estuaries ---

annual_summary <- event_severity |> 
      group_by(bay, estuary, year) |> 
      summarize(severity_mean = mean(severity, na.rm = TRUE),
                severity_max = max(severity, na.rm = TRUE),
                event_days = n()) |> 
      ungroup()

glimpse(event_severity)
nacheck(event_severity) 
# writexl::write_xlsx(annual_summary, "tables/cold-snap-severity-summary.xlsx")

### visualize events and severity through time ------ 

### set color pallete for visualizations ---
estuary_palette = c("Apalachicola Bay"="#e6d7b9",
                    "Charlotte Harbor"="#a7c4a0",
                    "Cedar Key"="#64a988",
                    'Northern Indian River'="#89c8d9",
                    "Northeast Florida"="#f2c6b4",
                    'Tampa Bay'='#dba5a4',
                    "Southern Indian River"="#bfaed9")

annual_summary |> 
      ggplot(aes(x = year, y = event_days, color = estuary)) + 
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Year", y = "Cold Front Days", color = 'Estuary') +
      theme_bw() +
      facet_wrap(~estuary) +
      scale_color_manual(values = estuary_palette)+
      scale_x_continuous(breaks = c(1960,1980,2000,2020)) +
      scale_y_continuous(breaks = c(0,10,20,30,40,50)) +
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
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

# ggsave('figs/cold-front-days.png', 
#        dpi = 800, 
#        units= 'in', 
#        height = 6, 
#        width = 6.5)

annual_summary |> 
      group_by(estuary) |> 
      mutate(severity_scaled = scale(severity_mean, center = TRUE)) |> 
      ggplot(aes(x = year, y = severity_scaled, color = estuary)) + 
      geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Year", y = "Scaled Cold Front Severity", color = 'Estuary') +
      theme_bw() +
      facet_wrap(~estuary) +
      scale_color_manual(values = estuary_palette)+
      scale_x_continuous(breaks = c(1960,1980,2000,2020)) +
      # scale_y_continuous(breaks = c(0,10,20,30,40,50)) +
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
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
      
# ggsave('figs/cold-front-severity.png', 
#        dpi = 800, 
#        units= 'in', 
#        height = 6, 
#        width = 6.5)

### clean environment before next step ---
keep <- c("nacheck")
rm(list = setdiff(ls(), keep))

### calculate severity as a function of intensity and duration -----

min <- read_csv('local-data/cold-snap-severity.csv')

min_consec <- min |> 
      group_by(bay, estuary) |> 
      mutate(event_id = cumsum(c(1, diff(as.numeric(date)) != 1))) %>%  # Group consecutive cold front days
      ungroup()

auc <- min_consec |> 
      group_by(bay, estuary, event_id) |> 
      summarize(auc = trapz(as.numeric(date), severity),
                duration = n(),
                start_date = min(date),
                end_date = max(date)) |> 
      ungroup() |> 
      arrange(bay, estuary, start_date)

severe_cold_fronts <- auc |> 
      group_by(bay, estuary) |> 
      mutate(
            threshold_90 = quantile(auc, 0.90, na.rm = TRUE),
            threshold_95 = quantile(auc, 0.95, na.rm = TRUE),
            threshold_99 = quantile(auc, 0.99, na.rm = TRUE) 
      ) |> 
      mutate(flag = case_when(
            auc >= threshold_99 ~ "Extreme",
            auc >= threshold_95 ~ "Severe",
            auc >= threshold_90 ~ "Significant",
            TRUE ~ "Moderate"
      )) |> 
      ungroup() |> 
      separate(start_date, c('start_year', 'start_month', 'start_day')) |> 
      mutate(across(start_year:start_day, as.numeric))

nacheck(severe_cold_fronts)
glimpse(severe_cold_fronts)

### visualize events and severity through time ------ 

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

severe_cold_fronts |> 
      filter(flag %in% c("Significant", "Severe", "Extreme")) |>
      filter(start_year >= 1980) |> 
      group_by(bay, estuary) |>
      mutate(auc = scale(auc, center = TRUE)) |>
      ggplot(aes(x = start_year, y = auc, color = flag)) + 
      geom_point(size = 2) +
      geom_smooth(method = "lm", color = "black") +
      labs(x = "Year", y = "Scaled Discrete Cold Snap Severity (AUC)", color = 'Severity') +
      theme_bw() +
      facet_wrap(~estuary) +
      scale_color_manual(values = severity_palette)+
      scale_x_continuous(breaks = c(1960,1970,1980,1990,2000,2010,2020)) +
      # scale_y_continuous(breaks = c(0.0,0.3,0.6,0.9,1.2)) +
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
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

# ggsave('figs/severe-cold-snap-auc.png', 
#        dpi = 800, 
#        units= 'in', 
#        height = 6, 
#        width = 6.5)