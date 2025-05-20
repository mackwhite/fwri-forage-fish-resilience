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

small_seine_zone_df <- df |> 
      filter(gear_details == "21.3-m Seine") |> 
      # group_by(year, month, day, estuary, zone) |> 
      group_by(year, month, estuary, zone) |> 
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
             pelagic_prop = mean(pelagic_n)) |> 
      ungroup() |> 
      mutate(simple_date = make_date(year = year, month = month, day = 1)) |> 
      mutate(est_zone = paste(estuary, zone, sep = "_"))
glimpse(small_seine_zone_df)

big_seine_zone_df <- df |> 
      filter(gear_details == "Haul Seine") |> 
      # group_by(year, month, day, estuary, zone) |> 
      group_by(year, month, estuary, zone) |> 
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
             pelagic_prop = mean(pelagic_n)) |> 
      ungroup() |> 
      mutate(simple_date = make_date(year = year, month = month, day = 1)) |> 
      mutate(est_zone = paste(estuary, zone, sep = "_")) |> 
      distinct()
glimpse(big_seine_zone_df)

### generate function to pull out trend for each estuary-zone combination ----
detrend <- function(df) {
      # arrange everything by data
      df <- df |> 
            arrange(simple_date)
      # interpolate missing values
      df$bm_m2 <- na.approx(df$bm_m2, na.rm = FALSE)
      # check if the group has enough data (at least 24 months)
      if (nrow(df) < 24) {
            df$detrended_bm_m2 <- df$bm_m2  # If too few points, return original values
            return(df)
      }
      # convert to tsibble for irregular time series handling
      df_tsibble <- df |> 
            as_tsibble(index = simple_date)
      # convert to time series format
      tsdf <- ts(df_tsibble$bm_m2, frequency = 12, start = c(min(df$year), min(df$month)))
      # fit stl to determine seasonality - 'periodic' means fixed seasonality
      fit <- stl(tsdf, s.window = "periodic")
      # extract detrended time series
      df$detrended_bm_m2 <- as.numeric(fit$time.series[,"trend"])
      # return dataframe
      return(df)
}

ss_detrended <- small_seine_zone_df |> 
      group_by(estuary, zone) |> 
      group_split() |> 
      lapply(detrend) |> 
      bind_rows() |> 
      arrange(est_zone, simple_date) |> 
      group_by(est_zone) |> 
      mutate(n = n()) |> 
      ungroup()

### checking out how seasonality was handled ---
ss_detrended |> 
      filter(estuary == "Apalachicola Bay", zone == "A") |> 
      ggplot(aes(x=simple_date)) +
      geom_line(aes(y = bm_m2, color = "Original bm_m2"), size = 1) +
      geom_line(aes(y = detrended_bm_m2, color = "Detrended bm_m2"), size = 1, linetype = "dashed") +
      labs(title = "Comparison of Original and Detrended Biomass",
           x = "Date",
           y = "Biomass (bm_m2)",
           color = "Legend") +
      theme_minimal()

ss_detrended |> 
      filter(estuary == "Apalachicola Bay", zone == "C") |> 
      ggplot(aes(x=simple_date)) +
      geom_line(aes(y = bm_m2, color = "Original bm_m2"), size = 1) +
      geom_line(aes(y = detrended_bm_m2, color = "Detrended bm_m2"), size = 1, linetype = "dashed") +
      labs(
           # title = "Comparison of Original and Detrended Biomass",
           title = "Apalachicola Bay - Zone C",
           x = "Date",
           y = "Biomass (bm_m2)",
           color = "Legend"
           ) +
      theme_minimal()

### generate function to visualize seasonality for each estuary-zone combination ----
seasonality <- function(df) {
      # arrange everything by data
      df <- df |> 
            arrange(simple_date)
      # interpolate missing values
      # df$bm_m2 <- na.approx(df$bm_m2, na.rm = FALSE)
      # check if the group has enough data (at least 24 months)
      if (nrow(df) < 24) {
            df$seasonal_bm_m2 <- NA  # If too few points, return original values
            return(df)
      }
      # convert to tsibble for irregular time series handling
      # df_tsibble <- df |> 
      #       as_tsibble(index = date)
      # convert to time series format
      tsdf <- ts(df$bm_m2, frequency = 12, start = c(min(df$year), min(df$month)))
      # fit stl to determine seasonality - 'periodic' means fixed seasonality
      fit <- stl(tsdf, s.window = "periodic")
      # extract detrended time series
      df$seasonal_bm_m2 <- as.numeric(fit$time.series[,"seasonal"])
      # return dataframe
      return(df)
}

ss_seasonal <- ss_detrended |> 
      group_by(estuary, zone) |> 
      group_split() |> 
      lapply(seasonality) |> 
      bind_rows()

### checking out seasonality ---
ss_seasonal |> 
      filter(estuary == "Apalachicola Bay", zone == "A", year == 2010) |> 
      ggplot(aes(x=simple_date, y = seasonal_bm_m2)) +
      geom_line(size = 1.5, color = "black", linetype = "dotted") +
      geom_smooth(size = 2) +
      labs(title = "Seasonal Component of Biomass",
           x = "Date",
           y = "Biomass (m2)") +
      theme_minimal()
glimpse(ss_seasonal)
write_csv(ss_seasonal, "local-data/detrended-smallseine-zone-biomass-ts.csv")

### notes - data really isn't there for some of the sites - especially true with haul seine data