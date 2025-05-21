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

### read in detrended data and bind -----
df <- read_csv("local-data/detrended-smallseine-ts.csv")
estuary <- read_csv("local-data/archive/for-joins/bay-to-estuary.csv") |> 
      janitor::clean_names()
site_info <- read_csv("local-data/annual_site_information_of_interest.csv")

### calculate everything annually ----

df_annual_sample_size <- df |> 
      group_by(estuary, zone, grid, year) |> 
      summarize(total_biomass_ann = mean(bm_m2, na.rm = TRUE),
                total_biomass_ann_detrend = mean(detrended_bm_m2, na.rm = TRUE),
                total_biomass_ann_seasonality = mean(seasonal_bm_m2, na.rm = TRUE),
                total_abund_ann = mean(abund, na.rm = TRUE),
                total_abund_ann_detrend = mean(detrended_abund, na.rm = TRUE),
                total_abund_ann_seasonality = mean(seasonal_abund, na.rm = TRUE),
                species_richness_ann = mean(species_richness, na.rm = TRUE),
                species_richness_ann_detrend = mean(detrended_species_richness, na.rm = TRUE),
                species_richness_ann_seasonality = mean(seasonal_species_richness, na.rm = TRUE),
                species_evenness_ann = mean(species_evenness, na.rm = TRUE),
                species_evenness_ann_detrend = mean(detrended_species_evenness, na.rm = TRUE),
                species_evenness_ann_seasonality = mean(seasonal_species_evenness, na.rm = TRUE),
                max_size_ann = mean(max_size, na.rm = TRUE),
                max_size_ann_detrend = mean(detrended_max_size, na.rm = TRUE),
                max_size_ann_seasonality = mean(seasonal_max_size, na.rm = TRUE),
                k_ann = mean(k, na.rm = TRUE),
                k_ann_detrend = mean(detrended_k, na.rm = TRUE),
                k_ann_seasonality = mean(seasonal_k, na.rm = TRUE),
                generation_time_ann = mean(gen_time, na.rm = TRUE),
                generation_time_ann_detrend = mean(detrended_gen_time, na.rm = TRUE),
                generation_time_ann_seasonality = mean(seasonal_gen_time, na.rm = TRUE),
                depth_max_ann = mean(max_depth, na.rm = TRUE),
                depth_max_ann_detrend = mean(detrended_max_depth, na.rm = TRUE),
                depth_max_ann_seasonality = mean(seasonal_max_depth, na.rm = TRUE),
                depth_min_ann = mean(min_depth, na.rm = TRUE),
                depth_min_ann_detrend = mean(detrended_min_depth, na.rm = TRUE),
                depth_min_ann_seasonality = mean(seasonal_min_depth, na.rm = TRUE),
                temp_min_ann = mean(pref_temp_min, na.rm = TRUE),
                temp_min_ann_detrend = mean(detrended_pref_temp_min, na.rm = TRUE),
                temp_min_ann_seasonality = mean(seasonal_pref_temp_min, na.rm = TRUE),
                temp_mean_ann = mean(pref_temp_mean, na.rm = TRUE),
                temp_mean_ann_detrend = mean(detrended_pref_temp_mean, na.rm = TRUE),
                temp_mean_ann_seasonality = mean(seasonal_pref_temp_mean, na.rm = TRUE),
                temp_max_ann = mean(pref_temp_max, na.rm = TRUE),
                temp_max_ann_detrend = mean(detrended_pref_temp_max, na.rm = TRUE),
                temp_max_ann_seasonality = mean(seasonal_pref_temp_max, na.rm = TRUE),
                troph_ann = mean(trophic_level, na.rm = TRUE),
                troph_ann_detrend = mean(detrended_trophic_level, na.rm = TRUE),
                troph_ann_seasonality = mean(seasonal_trophic_level, na.rm = TRUE),
                benthic_prop_ann = mean(benthic_prop, na.rm = TRUE),
                benthic_prop_ann_detrend = mean(detrended_benthic_prop, na.rm = TRUE),
                benthic_prop_ann_seasonality = mean(seasonal_benthic_prop, na.rm = TRUE),
                pelagic_prop_ann = mean(pelagic_prop, na.rm = TRUE),
                pelagic_prop_ann_detrend = mean(detrended_pelagic_prop, na.rm = TRUE),
                pelagic_prop_ann_seasonality = mean(seasonal_pelagic_prop, na.rm = TRUE),
                grid_n = n()) |> 
      ungroup() |> 
      group_by(estuary, zone, year) |> 
      mutate(zone_n = n()) |> 
      ungroup() |> 
      left_join(estuary, by = "estuary") |> 
      left_join(site_info, by = join_by("zone", "grid", "year", "bay"))

### generate stability dataset ----
df_stability <- df_annual_sample_size |> 
      group_by(estuary, zone, grid, bay) |> 
      summarize(
            ### biomass - regular
            comm_bm_mean = mean(total_biomass_ann, na.rm = TRUE),
            comm_bm_sd = sd(total_biomass_ann, na.rm = TRUE),
            comm_bm_cv = (sd(total_biomass_ann, na.rm = TRUE) / mean(total_biomass_ann, na.rm = TRUE)),
            comm_bm_stability = 1/comm_bm_cv,
            ### biomass - detrended
            comm_bm_mean_detrended = mean(total_biomass_ann_detrend, na.rm = TRUE),
            comm_bm_sd_detrended = sd(total_biomass_ann_detrend, na.rm = TRUE),
            comm_bm_cv_detrended = (sd(total_biomass_ann_detrend, na.rm = TRUE) / mean(total_biomass_ann_detrend, na.rm = TRUE)),
            comm_bm_stability_detrended = 1/comm_bm_cv_detrended,
            ### abundance - regular
            comm_abund_mean = mean(total_abund_ann, na.rm = TRUE),
            comm_abund_sd = sd(total_abund_ann, na.rm = TRUE),
            comm_abund_cv = (sd(total_abund_ann, na.rm = TRUE) / mean(total_biomass_ann, na.rm = TRUE)),
            comm_abund_stability = 1/comm_bm_cv,
            ### abundance - detrended
            comm_abund_mean_detrended = mean(total_abund_ann_detrend, na.rm = TRUE),
            comm_abund_sd_detrended = sd(total_abund_ann_detrend, na.rm = TRUE),
            comm_abund_cv_detrended = (sd(total_abund_ann_detrend, na.rm = TRUE) / mean(total_biomass_ann_detrend, na.rm = TRUE)),
            comm_abund_stability_detrended = 1/comm_abund_cv_detrended,
            ### community diversity and traits ---
            comm_species_richness = mean(species_richness_ann, na.rm = TRUE),
            comm_species_richness_detrended = mean(species_richness_ann_detrend, na.rm = TRUE),
            comm_species_evenness = mean(species_evenness_ann, na.rm = TRUE),
            comm_species_evenness_detrended = mean(species_evenness_ann_detrend, na.rm = TRUE),
            comm_max_size = mean(max_size_ann, na.rm = TRUE),
            comm_max_size_detrended = mean(max_size_ann_detrend, na.rm = TRUE),
            comm_k = mean(k_ann, na.rm = TRUE),
            comm_k_detrended = mean(k_ann_detrend, na.rm = TRUE),
            comm_generation_time = mean(generation_time_ann, na.rm = TRUE),
            comm_generation_time_detrended = mean(generation_time_ann_detrend, na.rm = TRUE),
            comm_depth_min = mean(depth_min_ann, na.rm = TRUE),
            comm_depth_min_detrended = mean(depth_min_ann_detrend, na.rm = TRUE),
            comm_depth_max = mean(depth_max_ann, na.rm = TRUE),
            comm_depth_max_detrended = mean(depth_max_ann_detrend, na.rm = TRUE),
            comm_temp_mean = mean(temp_mean_ann, na.rm = TRUE),
            comm_temp_mean_detrended = mean(temp_mean_ann_detrend, na.rm = TRUE),
            comm_temp_min = mean(temp_min_ann, na.rm = TRUE),
            comm_temp_min_detrended = mean(temp_min_ann_detrend, na.rm = TRUE),
            comm_temp_max = mean(temp_max_ann, na.rm = TRUE),
            comm_temp_max_detrended = mean(temp_max_ann_detrend, na.rm = TRUE),
            comm_benthic_prop = mean(benthic_prop_ann, na.rm = TRUE),
            comm_benthic_prop_detrended = mean(benthic_prop_ann_detrend, na.rm = TRUE),
            comm_pelagic_prop = mean(pelagic_prop_ann, na.rm = TRUE),
            comm_pelagic_prop_detrended = mean(pelagic_prop_ann_detrend, na.rm = TRUE),
            n = sum(grid_n),
            mean_lat = mean(lat),
            mean_long = mean(long),
            mean_temp = mean(obs_temp),
            mean_depth = mean(obs_depth),
            mean_cond = mean(obs_cond),
            mean_ph = mean(obs_ph),
            mean_sal = mean(obs_sal),
            mean_do2 = mean(obs_do2),
            sd_temp = sd(obs_temp),
            sd_depth = sd(obs_depth),
            sd_cond = sd(obs_cond),
            sd_ph = sd(obs_ph),
            sd_sal = sd(obs_sal),
            sd_do2 = sd(obs_do2)
            ) |> 
      filter(comm_bm_stability_detrended < 6)

glimpse(df_stability)

### check to make sure sample size doesn't have strong, significant relationship
lm_model <- lm(comm_bm_stability_detrended ~ n, data = df_stability)
summary(lm_model)

df_stability |> 
      ggplot(aes(x = n, y = comm_bm_stability_detrended)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      labs(x = "Total Observations (#)", y = "Biomass Stability") +
      scale_x_continuous(breaks = c(50,100,150,200,250,300)) +
      scale_y_continuous(breaks = c(1,2,3,4,5,6,7)) +
      annotate('text', x = 300, y = 6, label = bquote({R^2} == 0.02),
               size = 5, fontface = "bold") +
      annotate("text", x = 300, y = 5.7, label = bquote(italic(p) < 0.002), 
               size = 5, fontface = "bold") + 
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(size = 15, face = "bold", colour = "black"),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            legend.position = "none",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"))

ggsave('figs/stability-sample-size-regression.png',
       dpi = 800,
       units= 'in',
       height = 6,
       width = 6)

### generate some simple figures ----
df_stability |> 
      # filter(bay != "JX") |> 
      ggplot(aes(x = fct_reorder(estuary, comm_bm_stability_detrended, .fun = median), 
                 y = comm_bm_stability_detrended, 
                 fill = bay)) +
      geom_jitter(aes(color = bay), shape = 16, size = 2, width = 0.2, alpha = 1.0)+
      geom_boxplot(outlier.shape = NA, alpha = 0.35) +      
      scale_fill_manual(values = estuary_palette) +
      labs(x = "Total Observations (#)", y = "Forage Fish Biomass Stability (1/CV)", fill = "Estuary", color = "Estuary") +
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"))

ggsave('figs/stability-by-estuary.png',
       dpi = 800,
       units= 'in',
       height = 6,
       width = 12)

### version two - without northeast florida ---
ggsave('figs/stability-by-estuary-no-jx.png',
       dpi = 800,
       units= 'in',
       height = 6,
       width = 12)

### effects of species richnness on stability ----
model <- lm(log1p(comm_bm_stability) ~ log1p(comm_species_richness), data = df_stability)
summary(model)$r.squared 
summary(model)
r2 <- summary(model)$r.squared

df_stability |>
      filter(comm_species_richness >= 1) |>
      ggplot(aes(x = log1p(comm_species_richness), y = log1p(comm_bm_stability))) +
      geom_point(aes(color = estuary), size = 2) +  # Adds the scatter plot points
      geom_smooth(method = "lm", size = 2, color = "black", linetype = "solid", se = FALSE) +
      labs(x = "log(Species Richness)",
           y = "log(Biomass Stability)",
           color = "Estuary") +
      scale_x_continuous(breaks = c(1.25,1.50,1.75,2.00,2.25,2.50,2.75,3.00), limits = c(1.13,3.0)) +
      scale_y_continuous(breaks = c(0.25,0.50,0.75,1.00,1.25)) +
      annotate('text', x = 1.22, y = 1.38, label = bquote({R^2} == 0.28),
               size = 5, fontface = "bold") +
      annotate("text", x = 1.27, y = 1.33, label = bquote(italic(p) < 2 %*% 10^-16), 
               size = 5, fontface = "bold") + 
      scale_color_manual(values = estuary_palette) +
      theme(panel.background = element_rect(fill = "white"),
            axis.text = element_text(size = 12, face = "bold", colour = "black"), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(size = 15, face = "bold", colour = "black"),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            legend.position = c(0.85,0.27),
            # legend.justification = c(1, 0),
            legend.text = element_text(face = "bold", color = "black"),
            legend.title = element_text(face = "bold", color = "black"))

ggsave('figs/species-richness-stability.png',
       dpi = 800,
       units= 'in',
       height = 6,
       width = 6)

write_csv(df_stability, "local-data/stability-model-data-may2025.csv")

### Notes from meeting with Ted Switzer ----
### smaller seines may have only been seasonal in the early years
### 1995 and on is probably perfectly fine - check before that
### Charlotte Harbor  & Tampa Bay - those would be the ones that went back the furtherst
### maybe indian river as well
### if anything weird pops out in 2020, take a peak

### length-weight stuff 

### may be caveats to grids through time

### separating precipitation from physical damage from datasets
### mid to late 90s some pretty severe drought/precipitations
### greenwood paper on some of this (early 2000s)
### precipitation or maybe flow time series available daily
### 2004 charlie hit CH (freshwater influence)
## super strong storm that hit directly head on (mangrove die off - could be a paper)
### unexplained signals that may not be captured in models
### red flag on tequesta - catfish and mullet lesions started
### until late in the 20-teens... 
### two parallel analyses - based on different sampling gear, where the
### 21.3 m seines gear would exclude tequesta
### tampa effects of seagrass restoration
### some really standout hydro years in the late 90s on west coast

# read out files for moving forward ---------------------------------------

### community-level data for each sampling event ----
write_csv(df, "local-data/key-datasets/discrete-community-timeseries.csv")

### summarized community-level data at annual timescale ----
write_csv(df_annual_sample_size, "local-data/key-datasets/annual-community-timeseries.csv")

### summarized community-level data at period-of-record timescale ----
write_csv(df_stability, "local-data/key-datasets/community-biomass-stability.csv")
