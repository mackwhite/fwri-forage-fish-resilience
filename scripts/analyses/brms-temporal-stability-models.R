###project: Forage Fish Resilience
###author(s): MW
###goal(s): simple anova and Bayesian regression models
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
                 ggpubr, parameters, ggstats, brms, mixedup, multcompView)

### read in stability dataset ----
dat <- read_csv('local-data/key-datasets/stability_foragefish_final.csv')
### only needed the first coding session
# df <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
#       filter(biomass_stability <= 2) |> 
#       mutate(coast = case_when(
#             estuary %in% c("Northeast Florida", "Southern Indian River", 
#                            "Northern Indian River") ~ "Atlantic Coast",
#             TRUE ~ "Gulf Coast"
#       ))
# glimpse(df)

### read in disturbance dataset ----
# dist <- read_csv('local-data/key-datasets/resistance-resilience-calcs.csv') |> 
#       group_by(bay, zone) |> 
#       summarize(resistance = mean(resistance, na.rm = TRUE),
#                 resilience = mean(resilience_raw, na.rm = TRUE),
#                 .groups = "drop")
# glimpse(dist)

## save new copy without stability outlier in Northeast Florida Estuary + resist/resil metrics ----
# dat <- df |> 
#       left_join(dist, by = join_by("bay", "zone")) |> 
#       select(-c(resistance.x, resistance.y, resilience.x, resilience.y))
# 
# write_csv(dat, 'local-data/key-datasets/stability_foragefish_final.csv')
# write_csv(dat, 'local-data/stability_foragefish_final.csv')
# rm(df,dist)

### visualize stability across estuaries ----
### simple anova
anova_model <- aov(biomass_stability ~ estuary, data = dat)
summary(anova_model)
### Tukey HSD post hoc test
tukey_result <- TukeyHSD(anova_model)
tukey_letters <- multcompView::multcompLetters(tukey_result$estuary[,"p adj"])$Letters

ymax_vals <- dat |> 
      group_by(estuary) |> 
      summarize(ymax = max(biomass_stability, na.rm = TRUE), .groups = "drop")

letter_df <- data.frame(
      estuary = names(tukey_letters),
      cld = tukey_letters
) |> 
      mutate(cld = case_when(
            estuary == "Northeast Florida" ~ "a",
            estuary %in% c("Southern Indian River", "Apalachicola Bay") ~ "b",
            TRUE ~ "c"
      ))
letter_df <- letter_df |> left_join(ymax_vals)

### wrap labels for plotting
wrapped_labels <- dat |> 
      distinct(estuary) |> 
      mutate(label_wrapped = str_replace(estuary, " ", "\n")) |> 
      deframe()

dat |> 
      mutate(estuary = fct_reorder(estuary, biomass_stability, .fun = median, .desc = TRUE)) |> 
      ggplot(aes(x = estuary, y = biomass_stability)) +
      geom_boxplot(aes(fill = estuary), outlier.shape = NA) +
      geom_jitter(color = "black", alpha = 0.3, size = 1) +
      geom_text(data = letter_df, aes(x = estuary, y = ymax + 0.07, label = cld),
                inherit.aes = FALSE, fontface = "bold", size = 4) +
      scale_fill_manual(values = estuary_palette) +
      scale_x_discrete(labels = wrapped_labels) + 
      scale_y_continuous(breaks = c(0.4,0.8,1.2,1.6)) +
      theme_bw() +
      labs(x = "Estuary", y = "Biomass Stability", fill = "Estuary") + 
      theme(
            axis.text = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_text(size = 12, face = "bold", colour = "black"),
            plot.title = element_text(size = 10, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "none",
            legend.text = element_text(face = 'bold'),
            legend.title = element_text(face = 'bold')
      )

# ggsave('figs/biomass-stability-by-estuary.png',
#        dpi = 600, units= 'in', height = 4, width = 8)

### scale data for models ----
dat_scaled <- dat |>
      select(-n_obs, -n_months, -biomass_m2_mean, -biomass_m2_sd, 
             -abund_m2_mean, -abund_m2_sd, -biomass_cv) |> 
      select(coast, estuary, bay, zone, grid, lat, long, 
             start_date, end_date, biomass_stability,
             resistance, resilience, everything()) |>
      mutate(across(biomass_stability:resilience, \(x) scale(x, center = TRUE))) |>
      group_by(estuary) |>
      mutate(across(tl_max_mean:abundance_synchrony, \(x) scale(x, center = TRUE))) |>
      ungroup() |> 
      select(coast, estuary, bay, zone, grid, lat, long, 
             start_date, end_date, biomass_stability,
             resistance, resilience,
             tl_max_mean, k_mean, gen_time_mean, depth_max_mean, depth_min_mean, 
             temp_max_mean, temp_min_mean, troph_mean, species_richness_mean, species_evenness_mean,
             biomass_turnover, biomass_synchrony) |> 
      rename_with(~ str_remove(., "_mean$"), ends_with("_mean")) |> 
      rename(species_synchrony = biomass_synchrony,
             species_turnover = biomass_turnover,
             growth_rate = k)
      
glimpse(dat_scaled)

### set priors following Lemoine (2019, Ecology)
pr = prior(normal(0, 1), class = 'b')

### test out correlations ---
test_corr <- dat_scaled |> select(-c('coast', 'estuary', 'bay', 'zone', 'grid', 
                                     'lat', 'long','start_date', 'end_date'))
matrix <- cor(test_corr, use = 'complete.obs')
corrplot(matrix, method = "number", type = "lower", tl.col = "black", tl.srt = 45)

df <- dat_scaled
### clean up env ---
rm(list = setdiff(ls(), c("df", "pr")))
glimpse(df)

### full models ----
### correlations to be aware of
# tl_max and growth rate

# growth rate and gen_time
# growth rate and temp_max
# growth rate and temp_min

# gen_time and depth_min
# gen_time and temp_max
# gen_time and temp_min

# depth_min and temp_max
# depth_min and temp_min

# temp_min and temp_max 

# species richness and species evenness
### round one ----
m1 <- brm(biomass_stability ~ tl_max + (tl_max|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m2 <- brm(biomass_stability ~ growth_rate + (growth_rate|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m3 <- brm(biomass_stability ~ gen_time + (gen_time|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m4 <- brm(biomass_stability ~ depth_max + (depth_max|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m5 <- brm(biomass_stability ~ depth_min + (depth_min|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m6 <- brm(biomass_stability ~ temp_max + (temp_max|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m7 <- brm(biomass_stability ~ temp_min + (temp_min|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m8 <- brm(biomass_stability ~ troph + (troph|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m9 <- brm(biomass_stability ~ species_richness + (species_richness|estuary),
           data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m10 <- brm(biomass_stability ~ species_evenness + (species_evenness|estuary),
           data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m11 <- brm(biomass_stability ~ species_turnover + (species_turnover|estuary),
           data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m12 <- brm(biomass_stability ~ species_synchrony + (species_synchrony|estuary),
           data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

model_table <- performance::compare_performance(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
model_selection <- model_table |>
      mutate(dWAIC = WAIC - min(WAIC))

write_csv(model_selection, "tables/foragefish_brms_fullmodel_rd1.csv")

### clean environment ---
rm(list = setdiff(ls(), c("dat", "df", 'm3', 'pr')))

### full models ----
### correlations to be aware of
# tl_max and growth rate

# growth rate and gen_time
# growth rate and temp_max
# growth rate and temp_min

# gen_time and depth_min
# gen_time and temp_max
# gen_time and temp_min

# depth_min and temp_max
# depth_min and temp_min

# temp_min and temp_max 

# species richness and species evenness
### round two ----
m1.3 <- brm(biomass_stability ~ tl_max + gen_time + (gen_time + tl_max|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m4.3 <- brm(biomass_stability ~ depth_max + gen_time + (gen_time + depth_max|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m8.3 <- brm(biomass_stability ~ troph +  gen_time + (gen_time + troph|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m9.3 <- brm(biomass_stability ~ species_richness + gen_time + (gen_time + species_richness|estuary),
          data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m10.3 <- brm(biomass_stability ~ species_evenness + gen_time + (gen_time + species_evenness|estuary),
           data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m11.3 <- brm(biomass_stability ~ species_turnover + gen_time + (gen_time + species_turnover|estuary),
           data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m12.3 <- brm(biomass_stability ~ species_synchrony + gen_time + (gen_time + species_synchrony|estuary),
           data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

model_table <- performance::compare_performance(m1.3, m3, m4.3, m8.3, m9.3, m10.3, m11.3, m12.3)
model_selection <- model_table |>
      mutate(dWAIC = WAIC - min(WAIC))

write_csv(model_selection, "tables/foragefish_brms_fullmodel_rd2.csv")

### clean environment ---
rm(list = setdiff(ls(), c("dat", "df", 'm10.3', 'pr')))

### full models ----
### correlations to be aware of
# tl_max and growth rate

# growth rate and gen_time
# growth rate and temp_max
# growth rate and temp_min

# gen_time and depth_min
# gen_time and temp_max
# gen_time and temp_min

# depth_min and temp_max
# depth_min and temp_min

# temp_min and temp_max 

# species richness and species evenness
### round three ----
m1.3.10 <- brm(biomass_stability ~ tl_max + gen_time + species_evenness + (species_evenness + gen_time + tl_max|estuary),
            data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m4.3.10 <- brm(biomass_stability ~ depth_max + gen_time + species_evenness + (species_evenness + gen_time + depth_max|estuary),
            data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m8.3.10 <- brm(biomass_stability ~ troph +  gen_time + species_evenness + (species_evenness + gen_time + troph|estuary),
            data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m11.3.10 <- brm(biomass_stability ~ species_turnover + gen_time + species_evenness + (species_evenness + gen_time + species_turnover|estuary),
             data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m12.3.10 <- brm(biomass_stability ~ species_synchrony + gen_time + species_evenness + (species_evenness + gen_time + species_synchrony|estuary),
             data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

model_table <- performance::compare_performance(m1.3.10, m4.3.10, m8.3.10, m10.3, m11.3.10, m12.3.10)
model_selection <- model_table |>
      mutate(dWAIC = WAIC - min(WAIC))

write_csv(model_selection, "tables/foragefish_brms_fullmodel_rd3.csv")

### clean environment ---
rm(list = setdiff(ls(), c("dat", "df", 'm1.3.10', 'pr')))

### full models ----
### correlations to be aware of
# tl_max and growth rate

# growth rate and gen_time
# growth rate and temp_max
# growth rate and temp_min

# gen_time and depth_min
# gen_time and temp_max
# gen_time and temp_min

# depth_min and temp_max
# depth_min and temp_min

# temp_min and temp_max 

# species richness and species evenness
### round four ----

m4.3.10.1 <- brm(biomass_stability ~  tl_max + depth_max + gen_time + species_evenness + (tl_max + species_evenness + gen_time + depth_max|estuary),
               data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m8.3.10.1 <- brm(biomass_stability ~  tl_max + troph +  gen_time + species_evenness + (tl_max + species_evenness + gen_time + troph|estuary),
               data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m11.3.10.1 <- brm(biomass_stability ~  tl_max + species_turnover + gen_time + species_evenness + (tl_max + species_evenness + gen_time + species_turnover|estuary),
                data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m12.3.10.1 <- brm(biomass_stability ~  tl_max + species_synchrony + gen_time + species_evenness + (tl_max + species_evenness + gen_time + species_synchrony|estuary),
                data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

model_table <- performance::compare_performance(m1.3.10, m4.3.10.1, m8.3.10.1, m11.3.10.1, m12.3.10.1)
model_selection <- model_table |>
      mutate(dWAIC = WAIC - min(WAIC))

write_csv(model_selection, "tables/foragefish_brms_fullmodel_rd4.csv")

### clean environment ---
rm(list = setdiff(ls(), c("dat", "df", 'm12.3.10.1', 'pr')))

### round five ----

m4.3.10.1.12 <- brm(biomass_stability ~ species_synchrony + tl_max + depth_max + gen_time + species_evenness + (species_synchrony + tl_max + species_evenness + gen_time + depth_max|estuary),
                 data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m8.3.10.1.12 <- brm(biomass_stability ~ species_synchrony + tl_max + troph +  gen_time + species_evenness + (species_synchrony + tl_max + species_evenness + gen_time + troph|estuary),
                 data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

m11.3.10.1.12 <- brm(biomass_stability ~ species_synchrony + tl_max + species_turnover + gen_time + species_evenness + (species_synchrony + tl_max + species_evenness + gen_time + species_turnover|estuary),
                  data = df, prior = pr, warmup = 100, iter = 1000, chains = 4)

model_table <- performance::compare_performance(m4.3.10.1.12, m8.3.10.1.12, m11.3.10.1.12, m12.3.10.1)
model_selection <- model_table |>
      mutate(dWAIC = WAIC - min(WAIC))

write_csv(model_selection, "tables/foragefish_brms_fullmodel_rd5.csv")

### clean environment ---
rm(list = setdiff(ls(), c("dat", "df", 'm12.3.10.1', 'pr')))

### save best overall model ----
full_model <- m12.3.10.1
full_model_re_slope <- mixedup::extract_random_coefs(full_model)
full_model_re_slope_exp <- full_model_re_slope |>
      mutate(nozero = map2_lgl(lower_2.5, upper_97.5, \(x, y) between(0, x, y)))
full_model_fe_slope <- mixedup::extract_fixed_effects(full_model)
summary(full_model)
# save(full_model, file = "models/full-foragefish-brms-model.RData")
# write_rds(full_model, 'models/full-foragefish-brms-model.rds')