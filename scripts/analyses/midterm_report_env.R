###project: Forage Fish Resilience
###author(s): MW
###goal(s): generating figures for midterm report 
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

heat_wave_palette = c("Moderate"="lightgrey",
                     "Significant"="pink",
                     "Severe"="#BF616A",
                     "Extreme"="darkred")

cold_snap_palette = c("Moderate"="lightgrey",
                      "Significant"="#88C0D0",
                      "Severe"="#4682B4",
                      "Extreme"="darkblue")

### read in data and bind -----

heat_waves <- read_csv("local-data/marine-heat-wave-severity.csv")
hw_distinct <- heat_waves |> 
      select(bay, estuary, mhw_event_id, year, metric, auc, duration, mean_anomaly, max_anomaly) |> 
      distinct()

cold_snaps <- read_csv('local-data/marine-cold-snap-severity.csv') |> 
      rename(max_anomaly = min_anomaly)
cs_distinct <- cold_snaps |> 
      select(bay, estuary, coldsnap_event_id, year, metric, auc, duration, mean_anomaly, max_anomaly) |> 
      distinct()

glimpse(heat_waves)
glimpse(cold_snaps)
glimpse(hw_distinct)
glimpse(cs_distinct)

###########################################################################
# mean annual figures for cold snaps and heat waves -----------------------
###########################################################################

# cold snap figures -------------------------------------------------------
cs_distinct |> 
      group_by(estuary, year) |> 
      summarize(mean_annual_auc = mean(auc, na.rm = TRUE),
                mean_annual_duration = mean(duration, na.rm = TRUE),
                max_annual_anomaly = mean(max_anomaly, na.rm = TRUE),
                n = n_distinct(coldsnap_event_id)) |> 
      ungroup() |> 
      # filter(flag %in% c("Significant", "Severe", "Extreme")) |>
      filter(year >= 1980) |>
      ggplot(aes(x = year, y = mean_annual_auc)) + 
      geom_point(size = 2, color = "#4682B4") +
      geom_smooth(method = "lm", color = "black") +
      labs(x = "Year", y = "Mean Annual Cold Snap Severity (AUC)", color = 'Intensity') +
      theme_bw() +
      facet_wrap(~estuary, scales = "free") +
      # scale_color_manual(values = cold_snap_palette)+
      scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
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

ggsave('figs/mean-annual-cold-snap-auc.png',
       dpi = 800,
       units= 'in',
       height = 6,
       width = 6.5)

cs_distinct |> 
      group_by(estuary, year) |> 
      summarize(mean_annual_auc = mean(auc, na.rm = TRUE),
                mean_annual_duration = mean(duration, na.rm = TRUE),
                max_annual_anomaly = mean(max_anomaly, na.rm = TRUE),
                n = n_distinct(coldsnap_event_id)) |> 
      ungroup() |> 
      # filter(flag %in% c("Significant", "Severe", "Extreme")) |>
      filter(year >= 1980) |>
      ggplot(aes(x = year, y = mean_annual_duration)) + 
      geom_point(size = 2, color = "#4682B4") +
      geom_smooth(method = "lm", color = "black") +
      labs(x = "Year", y = "Mean Annual Cold Snap Duration (days)", color = 'Intensity') +
      theme_bw() +
      facet_wrap(~estuary, scales = "free") +
      # scale_color_manual(values = cold_snap_palette)+
      scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
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

ggsave('figs/mean-annual-cold-snap-duration.png',
       dpi = 800,
       units= 'in',
       height = 6,
       width = 6.5)

cs_distinct |> 
      group_by(estuary, year) |> 
      summarize(mean_annual_auc = mean(auc, na.rm = TRUE),
                mean_annual_duration = mean(duration, na.rm = TRUE),
                max_annual_anomaly = mean(max_anomaly, na.rm = TRUE),
                n = n_distinct(coldsnap_event_id)) |> 
      ungroup() |> 
      # filter(flag %in% c("Significant", "Severe", "Extreme")) |>
      filter(year >= 1980) |>
      ggplot(aes(x = year, y = max_annual_anomaly)) + 
      geom_point(size = 2, color = "#4682B4") +
      geom_smooth(method = "lm", color = "black") +
      labs(x = "Year", y = "Mean Annual Cold Snap Anomaly (°C)", color = 'Intensity') +
      theme_bw() +
      facet_wrap(~estuary, scales = "free") +
      # scale_color_manual(values = cold_snap_palette)+
      scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
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

ggsave('figs/mean-annual-cold-snap-anomaly.png',
       dpi = 800,
       units= 'in',
       height = 6,
       width = 6.5)

# heat wave figures -------------------------------------------------------
hw_distinct |> 
      group_by(estuary, year) |> 
      summarize(mean_annual_auc = mean(auc, na.rm = TRUE),
                mean_annual_duration = mean(duration, na.rm = TRUE),
                max_annual_anomaly = mean(max_anomaly, na.rm = TRUE)) |> 
      ungroup() |> 
      # filter(flag %in% c("Significant", "Severe", "Extreme")) |>
      filter(year >= 1980) |>
      ggplot(aes(x = year, y = mean_annual_auc)) + 
      geom_point(size = 2, color = "#BF616A") +
      geom_smooth(method = "lm", color = "black") +
      labs(x = "Year", y = "Mean Annual Heat Wave Severity (AUC)", color = 'Intensity') +
      theme_bw() +
      facet_wrap(~estuary, scales = "free") +
      # scale_color_manual(values = cold_snap_palette)+
      scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
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

ggsave('figs/mean-annual-heat-wave-auc.png',
       dpi = 800,
       units= 'in',
       height = 6,
       width = 6.5)

hw_distinct |> 
      group_by(estuary, year) |> 
      summarize(mean_annual_auc = mean(auc, na.rm = TRUE),
                mean_annual_duration = mean(duration, na.rm = TRUE),
                max_annual_anomaly = mean(max_anomaly, na.rm = TRUE)) |> 
      ungroup() |> 
      # filter(flag %in% c("Significant", "Severe", "Extreme")) |>
      filter(year >= 1980) |>
      ggplot(aes(x = year, y = mean_annual_duration)) + 
      geom_point(size = 2, color = "#BF616A") +
      geom_smooth(method = "lm", color = "black") +
      labs(x = "Year", y = "Mean Annual Heat Wave Duration (days)", color = 'Intensity') +
      theme_bw() +
      facet_wrap(~estuary, scales = "free") +
      # scale_color_manual(values = cold_snap_palette)+
      scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
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

ggsave('figs/mean-annual-heat-wave-duration.png',
       dpi = 800,
       units= 'in',
       height = 6,
       width = 6.5)


hw_distinct |> 
      group_by(estuary, year) |> 
      summarize(mean_annual_auc = mean(auc, na.rm = TRUE),
                mean_annual_duration = mean(duration, na.rm = TRUE),
                max_annual_anomaly = mean(max_anomaly, na.rm = TRUE)) |> 
      ungroup() |> 
      # filter(flag %in% c("Significant", "Severe", "Extreme")) |>
      filter(year >= 1980) |>
      ggplot(aes(x = year, y = max_annual_anomaly)) + 
      geom_point(size = 2, color = "#BF616A") +
      geom_smooth(method = "lm", color = "black") +
      labs(x = "Year", y = "Mean Annual Heat Wave Anomaly (°C)", color = 'Intensity') +
      theme_bw() +
      facet_wrap(~estuary, scales = "free") +
      # scale_color_manual(values = cold_snap_palette)+
      scale_x_continuous(breaks = c(1950,1960,1970,1980,1990,2000,2010,2020)) +
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

ggsave('figs/mean-annual-heat-wave-anomaly.png',
       dpi = 800,
       units= 'in',
       height = 6,
       width = 6.5)