###project: Forage Fish Resilience
###author(s): MW
###goal(s): breakpoints of time series ----
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### set color palettes ----
estuary_palette = c("Apalachicola Bay"="#e6d7b9",
                    "Charlotte Harbor"="#a7c4a0",
                    "Cedar Key"="#64a988",
                    'Northern Indian River'="#89c8d9",
                    "Northeast Florida"="#f2c6b4",
                    'Tampa Bay'='#dba5a4',
                    "Southern Indian River"="#bfaed9")

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readr, zoo, MuMIn, corrplot, performance, ggeffects,
                 ggpubr, parameters, ggstats, brms, mixedup, multcompView, changepoint)

### read in discrete time series dataset ----
df <- read_csv('local-data/key-datasets/discrete-detrended-forage-fish-small-seine-time-series.csv')
glimpse(df)

dat <- df |> 
      select(-abund_m2:-species_evenness) |>
      rename_with(~ str_remove(.x, "^detrended_"), starts_with("detrended_")) |> 
      filter(grid_obs >= 40 & grid_obs <= 295) |> 
      select(-c(grid_obs, zone_obs)) |> 
      group_by(year, estuary, zone) |> 
      summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |> 
      mutate(date = make_date(year, 1, 1)) |> 
      arrange(estuary, zone, date)

# Step 2: Check for 60 consecutive months
zone_validity <- dat |>
      group_by(estuary, zone) |>
      mutate(
            month_diff = interval(lag(date), date) / months(1),
            # Start a new group when there's a gap > 1 month
            group_id = cumsum(is.na(month_diff) | month_diff != 1)
      ) |>
      group_by(estuary, zone, group_id) |>
      summarize(n_months = n(), .groups = "drop") |>
      filter(n_months >= 120) |>
      distinct(estuary, zone)

# Step 3: Keep only valid zones in the main dataset
dat_filtered <- dat |>
      semi_join(zone_validity, by = c("estuary", "zone"))

### visualize time series for each zone ----
ggplot(dat, aes(x = date, y = biomass_m2, color = zone, group = zone)) +
      geom_line(size = 0.5) +
      facet_wrap(~ estuary, scales = "free") +
      labs(
            title = "Biomass (m²) Over Time by Zone and Estuary",
            x = "Date",
            y = "Biomass (m²)",
            color = "Zone"
      ) +
      theme_minimal()

### breakpoint analyses ----

# Define function to apply changepoint detection
detect_mean_shifts <- function(df) {
      if (nrow(df) < 10) return(tibble(segment = NA, mean = NA, start_date = NA, end_date = NA))
      
      result <- cpt.mean(df$biomass_m2, method = "PELT", penalty = "BIC")
      breaks <- c(0, cpts(result), nrow(df))
      
      segments <- purrr::map2_dfr(breaks[-length(breaks)] + 1, breaks[-1], function(start, end) {
            mean_val <- mean(df$biomass_m2[start:end], na.rm = TRUE)
            tibble(
                  segment = paste0(start, "-", end),
                  mean = mean_val,
                  start_date = df$simple_date[start],
                  end_date = df$simple_date[end]
            )
      })
      
      return(segments)
}

mean_shift_df <- dat %>%
      group_by(estuary, zone, grid) %>%
      arrange(year, month) %>%
      group_modify(~ detect_mean_shifts(.x)) |> 
      ungroup() |> 
      arrange(estuary, zone, grid) |> 
