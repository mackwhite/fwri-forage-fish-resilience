###project: Forage Fish Resilience
###author(s): MW
###goal(s): bayesian regression models
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readr, zoo, MuMIn, corrplot, performance, ggeffects,
                 ggpubr, parameters, ggstats, brms, mixedup)

dat <- read_csv('local-data/key-datasets/community-biomass-stability.csv') |> 
      filter(total_obs >= 50) |> 
      select(-flag) |> 
      rename(gear = gear_details,
             stability = comm_bm_stability) |> 
      filter(gear != "Haul Seine") |> 
      select(-gear, -comm_bm_mean, -comm_bm_sd, -comm_bm_cv, -total_obs, -bay)
glimpse(dat)

test <- dat |> group_by(estuary) |> count()
print(test)

dat_scaled <- dat |>
      select(estuary, zone, grid, stability, everything()) |>
      mutate(stability = scale(stability)) |>
      group_by(estuary) |>
      ## this is a function syntax
      mutate(across(comm_species_richness:comm_pelagic_prop, \(x) scale(x, center = TRUE))) |>
      ungroup()
glimpse(dat_scaled)

### set priors following Lemoine (2019, Ecology)
pr = prior(normal(0, 1), class = 'b')

### test out correlations ---
test_corr <- dat_scaled |> select(-estuary, -zone, -grid)
matrix <- cor(test_corr, use = 'complete.obs')
corrplot(matrix, method = "number", type = "lower", tl.col = "black", tl.srt = 45)

df <- dat_scaled |> 
      rename_with(~gsub("comm_", "", .x), everything())
glimpse(df)
### full models ----
### round one ----
m1 <- brm(stability ~ species_richness + (species_richness|estuary),
          data = df, prior = pr, warmup = 1000, iter = 10000, chains = 4)

m2 <- brm(stability ~ species_evenness + (species_evenness|estuary),
          data = df, prior = pr, warmup = 1000, iter = 10000, chains = 4)

m3 <- brm(stability ~ max_size + (max_size|estuary),
          data = df, prior = pr, warmup = 1000, iter = 10000, chains = 4)

m4 <- brm(stability ~ k + (k|estuary),
          data = df, prior = pr, warmup = 1000, iter = 10000, chains = 4)

m5 <- brm(stability ~ generation_time + (generation_time|estuary),
          data = df, prior = pr, warmup = 1000, iter = 10000, chains = 4)

m6 <- brm(stability ~ depth_min + (depth_min|estuary),
          data = df, prior = pr, warmup = 1000, iter = 10000, chains = 4)

m7 <- brm(stability ~ depth_max + (depth_max|estuary),
          data = df, prior = pr, warmup = 1000, iter = 10000, chains = 4)

m8 <- brm(stability ~ temp_min + (temp_min|estuary),
          data = df, prior = pr, warmup = 1000, iter = 10000, chains = 4)

m9 <- brm(stability ~ temp_max + (temp_max|estuary),
          data = df, prior = pr, warmup = 1000, iter = 10000, chains = 4)

m10 <- brm(stability ~ benthic_prop + (benthic_prop|estuary),
          data = df, prior = pr, warmup = 1000, iter = 10000, chains = 4)

m11 <- brm(stability ~ pelagic_prop + (pelgic_prop|estuary),
           data = df, prior = pr, warmup = 1000, iter = 10000, chains = 4)

model_table <- performance::compare_performance(m1,m2)
model_selection <- model_table |>
      mutate(dWAIC = WAIC - min(WAIC))

### clean environment ---
rm(list = setdiff(ls(), c("dat", "df", 'm1')))

### round two ----

### save best overall model ----
full_model <- m1
full_model_re_slope <- mixedup::extract_random_coefs(full_model)
full_model_re_slope_exp <- full_model_re_slope |>
      mutate(nozero = map2_lgl(lower_2.5, upper_97.5, \(x, y) between(0, x, y)))
full_model_fe_slope <- mixedup::extract_fixed_effects(full_model)
summary(full_model)
# save(full_model, file = "output/ms-second-round/models/fullmodel.RData")

# performance::performance(full_model)

### visualize model results ----
### set color pallete for visualizations ---
estuary_palette = c("Apalachicola Bay"="#e6d7b9",
                    "Charlotte Harbor"="#a7c4a0",
                    "Cedar Key"="#64a988",
                    'Northern Indian River'="#89c8d9",
                    "Northeast Florida"="#f2c6b4",
                    'Tampa Bay'='#dba5a4',
                    "Southern Indian River"="#bfaed9",
                    "Overall"="black")

### visualizations for species richness ---
richness_re1 <- ggpredict(full_model,
                           type = "re",
                           terms = c('species_richness[-4:6 by=0.01]', 'estuary'))
richness_re <- as.data.frame(richness_re1)

richness_fe1 <- ggpredict(full_model,
                           type = "fe",
                           terms = c('species_richness[-4:6 by=0.01]', 'estuary'))
richness_fe <- as.data.frame(richness_fe1) |>
      mutate(group = 'Overall')

richness_all <- rbind(richness_re, richness_fe)

richness_all |>
      # mutate(group = factor(
      #       group,
      #       levels = c("Overall", "FCE", "MCR", "PCCC", "PCCS", "SBC", "VCR")
      # )) |>
      ggplot(aes(x = x, y = predicted, color = group)) +
      geom_smooth(method = "lm", linewidth = 1.5) +
      labs(x = 'Species Richness', y = 'Forage Fish Stability', color = 'Program') +
      theme_classic() +
      scale_color_manual(values = estuary_palette) +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            # axis.title.x = element_blank(),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            # axis.title.y = element_blank(),
            legend.position = "right",
            legend.background = element_blank(),
            legend.key = element_rect(fill = 'white'),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))

mech_fe <- full_model_fe_slope |>
      rename(effect = term) |>
      filter(effect != 'Intercept') |>
      mutate(group = "Overall") |>
      select(group, effect, value, se, lower_2.5, upper_97.5)

mech_re <- full_model_re_slope |>
      filter(effect != "Intercept") |>
      select(group, effect, value, se, lower_2.5, upper_97.5)

mech_slopes <- rbind(mech_fe, mech_re)
glimpse(mech_slopes)

mech_slopes |> 
      mutate(group = case_when(
            group == "Cedar.Key" ~ "Cedar Key",
            group == "Charlotte.Harbor" ~ "Charlotte Harbor",
            group == "Apalachicola.Bay" ~ "Apalachicola Bay",
            group == "Northern.Indian.River" ~ "Northern Indian River",
            group == "Southern.Indian.River" ~ "Southern Indian River",
            group == "Tampa.Bay" ~ "Tampa Bay",
            group == "Northeast.Florida" ~ "Northeast Florida",
            TRUE ~ group
      )) |> 
      mutate(group = factor(group,
                            levels = c("Cedar Key", "Charlotte Harbor", "Apalachicola Bay",
                                       "Northern Indian River", "Southern Indian River",
                                       "Tampa Bay", "Northeast Florida", "Overall"))) |>
            filter(effect == "species_richness") |>
            ggplot(aes(x = value, y = group, color = group)) +
            geom_point(size = 3) +
            geom_errorbarh(aes(xmin = lower_2.5, xmax = upper_97.5), size = 1, height = 0) +
            geom_vline(xintercept = 0, size = 1) +
            # scale_x_continuous(labels = function(x) sprintf("%.1f", x),
            #                    limits = c(-1.1,0.3),
            #                    breaks = seq(-0.9,0.3, by = 0.3)) +
            labs(x = 'Species Richness Beta', y = 'Estuary', color = "Estuary") +
            theme_classic() +
            scale_color_manual(values = estuary_palette) +
            theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
                  axis.text.y = element_text(face = "bold", color = "black", size = 12),
                  axis.title.x = element_text(face = "bold", color = "black", size = 14),
                  # axis.title.y = element_text(face = "bold", color = "black", size = 14),
                  axis.title.y = element_blank(),
                  legend.position = "right",
                  legend.background = element_blank(),
                  legend.key = element_rect(fill = 'white'),
                  legend.text = element_text(face = "bold", color = "black", size = 12),
                  legend.title = element_text(face = "bold", color = "black", size = 14))
