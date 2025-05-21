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

### read in data and bind -----
df <- read_rds("local-data/key-datasets/forage_fish_master.RDS")
glimpse(df)
estuary <- read_csv("local-data/archive/for-joins/bay-to-estuary.csv") |> 
      janitor::clean_names()

### species table for report ---
spp_table <- df |> 
      group_by(common_name, scientific_name) |> 
      summarize(obs = sum(n),
                length_cm = mean(mean_length_cm, na.rm = TRUE),
                weight_g = mean(mean_weight_g, na.rm = TRUE)) |> 
      ungroup() |> 
      mutate(proportion = obs/sum(obs),
             percentage = (obs / sum(obs)) * 100) |> 
      arrange(desc(obs))
# write_csv(spp_table, "tables/species-table.csv")

df_total <- df |>
      left_join(estuary, by = "bay") |> 
      group_by(year, month, day, bay, estuary, gear_details, rep, zone, subzone, grid) |>
      summarize(total_biomass = sum(n*mean_weight_g, na.rm = TRUE),
                total_biomass_m2 = total_biomass/area_m2,
                total_n = sum(n, na.rm = TRUE),
                species_richness = n_distinct(scientific_name),
                max_size = mean(tl_max, na.rm = TRUE),
                mean_k = mean(k, na.rm = TRUE),
                mean_generation_time = mean(generation_time, na.rm = TRUE),
                mean_depth_max = mean(depth_max, na.rm = TRUE),
                mean_depth_min = mean(depth_min, na.rm = TRUE),
                mean_temp_pref_min = mean(temp_pref_min, na.rm = TRUE),
                mean_tem_pref_mean = mean(temp_pref_mean, na.rm = TRUE),
                mean_temp_pref_max = mean(temp_pref_max, na.rm = TRUE),
                mean_troph = mean(troph, na.rm = TRUE),
                benthic_n = sum(n * (feeding_path == "benthic"), na.rm = TRUE),
                pelagic_n = sum(n * (feeding_path == "pelagic"), na.rm = TRUE),
                benthic_prop = benthic_n/total_n,
                pelagic_prop = pelagic_n/total_n
                ) |>
      ungroup() |>
      distinct() |> 
      mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |> 
      mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))

### table for report ---

table <- df_total |> 
      group_by(estuary) |> 
      summarize(min_year = min(year),
                max_year = max(year),
                zones = n_distinct(zone),
                grids = n_distinct(grid))

low_obs_flag <- df_total |> 
      group_by(bay, estuary, zone, grid, gear_details) |> 
      summarize(total_obs = n_distinct(date),
                flag = case_when(
                      total_obs <= 50 ~ "remove",
                      # total_obs >= 150 ~ "remove",
                      TRUE ~ "keep"))

### filter out sites with low sample sizes ----

df_total_sample <- df_total |> 
      left_join(low_obs_flag, by = c("bay", "estuary","zone", "grid", "gear_details")) |> 
      filter(flag == "keep") |>
      select(-flag) |> 
      filter(year >= 1996)

### reading in evenness metrics from step 8 below (wouldve made sense to do first)
### adds columns "h" for shannon evenness and "s" for species richness to double check matrix work was solid
### s and species_richness lines up, so makes sense - it worked :)

evenness <- read_csv("local-data/evenness_metrics_for_join_in_stepseven.csv") |> 
      mutate(rep = as.character(rep)) |> 
      rename(species_evenness = H,
             species_richness_test = S)

df_total_sample_size <- df_total_sample |> 
      left_join(evenness, by = c("year", "month", "day", "bay", "estuary", "gear_details", "rep", "zone", "subzone", "grid"))
glimpse(df_total_sample_size)

### calculate everything annually ----

df_annual_sample_size <- df_total_sample_size |> 
      group_by(bay, estuary, zone, grid, year, gear_details) |> 
      summarize(total_biomass_ann = mean(total_biomass_m2, na.rm = TRUE),
                total_n_ann = mean(total_n, na.rm = TRUE),
                species_richness_ann = mean(species_richness, na.rm = TRUE),
                species_evenness_ann = mean(species_evenness, na.rm = TRUE),
                max_size_ann = mean(max_size, na.rm = TRUE),
                k_ann = mean(mean_k, na.rm = TRUE),
                generation_time_ann = mean(mean_generation_time, na.rm = TRUE),
                depth_max_ann = mean(mean_depth_max, na.rm = TRUE),
                depth_min_ann = mean(mean_depth_min, na.rm = TRUE),
                temp_min_ann = mean(mean_temp_pref_min, na.rm = TRUE),
                temp_mean_ann = mean(mean_tem_pref_mean, na.rm = TRUE),
                temp_max_ann = mean(mean_temp_pref_max, na.rm = TRUE),
                troph_ann = mean(mean_troph, na.rm = TRUE),
                benthic_prop_ann = mean(benthic_prop, na.rm = TRUE),
                pelagic_prop_ann = mean(pelagic_prop, na.rm = TRUE))

### generate stability dataset ----

df_stability <- df_annual_sample_size |> 
      group_by(bay, estuary, zone, grid, gear_details) |> 
      summarize(comm_bm_mean = mean(total_biomass_ann, na.rm = TRUE),
                comm_bm_sd = sd(total_biomass_ann, na.rm = TRUE),
                comm_bm_cv = (sd(total_biomass_ann, na.rm = TRUE) / mean(total_biomass_ann, na.rm = TRUE)),
                comm_bm_stability = 1/comm_bm_cv,
                comm_species_richness = mean(species_richness_ann, na.rm = TRUE),
                comm_species_evenness = mean(species_evenness_ann, na.rm = TRUE),
                comm_max_size = mean(max_size_ann, na.rm = TRUE),
                comm_k = mean(k_ann, na.rm = TRUE),
                comm_generation_time = mean(generation_time_ann, na.rm = TRUE),
                comm_depth_min = mean(depth_min_ann, na.rm = TRUE),
                comm_depth_max = mean(depth_max_ann, na.rm = TRUE),
                comm_temp_mean = mean(temp_mean_ann, na.rm = TRUE),
                comm_temp_min = mean(temp_min_ann, na.rm = TRUE),
                comm_temp_max = mean(temp_max_ann, na.rm = TRUE),
                comm_benthic_prop = mean(benthic_prop_ann, na.rm = TRUE),
                comm_pelagic_prop = mean(pelagic_prop_ann, na.rm = TRUE)) |> 
      left_join(low_obs_flag, by = c("bay", "estuary","zone", "grid", "gear_details"))

### check to make sure sample size doesn't have strong, significant relationship
lm_model <- lm(comm_bm_stability ~ total_obs, data = df_stability)
summary(lm_model)

df_stability |> 
      ggplot(aes(x = total_obs, y = comm_bm_stability)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      labs(x = "Total Observations (#)", y = "Forage Fish Biomass Stability (1/CV)") +
      scale_x_continuous(breaks = c(50,100,150,200,250,300)) +
      scale_y_continuous(breaks = c(0.0,0.5,1.0,1.5,2.0,2.5)) +
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(size = 15, face = "bold", colour = "black"),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            # panel.grid.major = element_blank(),
            # panel.grid.minor = element_blank(),
            legend.position = "none",
            # legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"))

ggsave('figs/stability-sample-size-regression.png',
       dpi = 800,
       units= 'in',
       height = 6,
       width = 6.5)

### generate some simple figures ----

df_stability |> 
      ggplot(aes(x = fct_reorder(bay, comm_bm_stability, .fun = median), 
                 y = comm_bm_stability, 
                 fill = estuary)) +
      geom_jitter(aes(color = estuary), shape = 16, size = 2, width = 0.2, alpha = 1.0)+
      geom_boxplot(outlier.shape = NA, alpha = 0.35) +      
      scale_fill_manual(values = estuary_palette) +
      facet_wrap(~gear_details) +
      labs(x = "Total Observations (#)", y = "Forage Fish Biomass Stability (1/CV)") +
      # scale_x_continuous(breaks = c(50,60,70,80,90,100,110,120)) +
      scale_y_continuous(breaks = c(0.0,0.5,1.0,1.5,2.0,2.5)) +
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            # panel.grid.major = element_blank(),
            # panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"))

# ggsave('figs/stability-by-estuary.png',
#        dpi = 800,
#        units= 'in',
#        height = 6,
#        width = 10)

obs_summary <- df_stability |> 
      group_by(bay) |> 
      summarise(n = n(), .groups = "drop")

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
      annotate('text', x = 1.22, y = 1.35, label = bquote({R^2} == 0.11),
               size = 5, fontface = "bold") +
      annotate("text", x = 1.265, y = 1.28, label = bquote(italic(p) < 2 %*% 10^-16), 
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

# write_csv(df_stability, "local-data/stability-model-data-012025.csv")

df_total_sample_size |> 
      mutate(gear_estuary = paste(gear_details, estuary, sep = "_")) |> 
      group_by(date, gear_estuary) |> 
      mutate(mean = mean(total_biomass_m2, na.rm = TRUE)) |> 
      ggplot(aes(x=date, y=mean)) +
      geom_line()+
      facet_wrap(~gear_estuary) + 
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(size = 15, face = "bold", colour = "black"),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            legend.position = "none",
            # legend.text = element_text(size = 12, color = "black", face = 'bold'),
            # legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"))

df_total_sample_size |> 
      mutate(gear_estuary = paste(gear_details, estuary, sep = "_")) |> 
      group_by(year, gear_estuary) |> 
      mutate(mean = mean(total_biomass_m2, na.rm = TRUE)) |> 
      ggplot(aes(x=year, y=mean)) +
      geom_line()+
      facet_wrap(~gear_estuary, scales = "free") + 
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"), 
            axis.line = element_line(color = "black"),
            axis.title.x = element_text(size = 15, face = "bold", colour = "black"),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            legend.position = "none",
            # legend.text = element_text(size = 12, color = "black", face = 'bold'),
            # legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"))
      
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
write_csv(df_total_sample_size, "local-data/key-datasets/discrete-community-timeseries.csv")

### summarized community-level data at annual timescale ----
write_csv(df_annual_sample_size, "local-data/key-datasets/annual-community-timeseries.csv")

### summarized community-level data at period-of-record timescale ----
write_csv(df_stability, "local-data/key-datasets/community-biomass-stability.csv")
