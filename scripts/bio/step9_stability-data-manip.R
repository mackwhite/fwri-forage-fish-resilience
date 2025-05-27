###project: Forage Fish Resilience
###author(s): MW
###goal(s): generating summarized datasets
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, FSA, readr, forcats, multcompView, readxl, dplyr, splitstackshape, purrr, zoo, pracma, vegan, e1071)

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

### read in detrended data -----
full <- read_csv("local-data/key-datasets/discrete-detrended-forage-fish-small-seine-time-series.csv")

detrended_summary <- full |> 
      group_by(estuary, bay, zone, grid) |> 
      summarize(
            n_obs = n(),  # number of rows (monthly samples)
            start_date = min(simple_date, na.rm = TRUE),
            end_date = max(simple_date, na.rm = TRUE),
            n_months = lubridate::interval(start_date, end_date) %/% months(1) + 1,
            across(
                  starts_with("detrended_"),
                  list(
                        mean = ~mean(.x, na.rm = TRUE),
                        sd   = ~sd(.x, na.rm = TRUE)
                  ),
                  .names = "{.col}_{.fn}"
            ),
            .groups = "drop") |> 
      rename_with(
            ~ str_remove(., "^detrended_"),
            .cols = starts_with("detrended_")
      ) |> 
      filter(n_obs > 1) |> 
      mutate(biomass_cv = biomass_m2_sd/biomass_m2_mean,
             biomass_stability = 1/biomass_cv)

### identify breakpoint in relatinship between sample size and stability ----
gam_fit <- gam(biomass_stability ~ s(n_obs), data = detrended_summary)

pred_df <- data.frame(n_obs = seq(min(detrended_summary$n_obs), max(detrended_summary$n_obs), length.out = 200))

pred_df <- cbind(
      pred_df,
      predict(gam_fit, newdata = pred_df, se.fit = TRUE) |> 
            as.data.frame()
)

names(pred_df)[2:3] <- c("fit", "se")

pred_df |> 
      ggplot(aes(x = n_obs, y = fit)) + 
      geom_line(linewidth = 1) +
      geom_ribbon(aes(ymin = fit - 2*se, ymax = fit + 2*se), color = "black", size = 0.75, alpha = 0.5) +
      scale_x_continuous(breaks = c(0,50,100,150,200,250,300)) +
      scale_y_continuous(breaks = c(-0.6,-0.2,0.2,0.6,1.0,1.4,1.8)) +
      geom_vline(xintercept = 40, size = 1.5, color = "red", linetype = 2) +
      # geom_vline(xintercept = 295, size = 1.5, color = "red", linetype = 2) +
      annotate("text", x = 60, y = 1.58, label = "40", 
               size = 5, fontface = "bold") + 
      # annotate("text", x = 320, y = 1.58, label = "295", 
      #          size = 5, fontface = "bold") + 
      labs(x = "Number of Observations",
           y = "Biomass Stability") +
      theme(axis.text = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_text(size = 12, face = "bold", colour = "black"),
            plot.title = element_text(size = 10, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = 'bold'),
            legend.title = element_text(face = 'bold'))

ggsave('figs/stability-samplesize-threshold.png',
       dpi = 600, units= 'in', height = 5, width = 5)

df_filtered <- detrended_summary |> 
      filter(n_obs >= 40 & n_obs <= 295)
glimpse(df_filtered)

coords <- read_rds('local-data/key-datasets/forage_fish_master.RDS') |> 
      select(bay, zone, grid, latitude, longitude) |> 
      distinct() |> 
      group_by(bay, zone, grid) |> 
      summarize(lat = mean(latitude, na.rm = TRUE),
                long = mean(longitude, na.rm = TRUE),
                .groups = "drop")

df_final <- df_filtered |> left_join(coords)
write_csv(df_final, "local-data/stability-filterd-may2025.csv")
