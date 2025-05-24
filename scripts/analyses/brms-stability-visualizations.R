###project: Forage Fish Resilience
###author(s): MW
###goal(s): visualizing Bayesian regression models for forage fish project
###date(s): May 2025
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

estuary_palette_abb = c("AP"="#e6d7b9",
                        "CH"="#a7c4a0",
                        "CK"="#64a988",
                        'NIR'="#89c8d9",
                        "JX"="#f2c6b4",
                        'TB'='#dba5a4',
                        "SIR"="#bfaed9")

est <- c('Apalachicola Bay', 'Cedar Key', 'Tampa Bay', 'Charlotte Harbor',
         'Southern Indian River', 'Northern Indian River', 'Northeast Florida')

est_abb <- c('AP', 'CK', 'TB', 'CH', 'SIR', 'NIR', 'JX')

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readr, zoo, MuMIn, corrplot, performance, ggeffects,
                 ggpubr, patchwork, parameters, ggstats, brms, mixedup, multcompView, grid)

### read in stability dataset and summarize to determine order of estuaries in vis ----
stability <- read_csv('local-data/key-datasets/stability_foragefish_final.csv')
order <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
      group_by(estuary) |> 
      summarize(mn = mean(biomass_stability),
                med = median(biomass_stability)) |> 
      arrange(mn)

mod = readRDS('models/full-foragefish-brms-model.rds')

# summary stats -----------------------------------------------------------
post = posterior_samples(mod)

### generation time ----
mean(post$`r_estuary[Northeast.Florida,gen_time]` + post$b_gen_time < 0)
mean(post$`r_estuary[Southern.Indian.River,gen_time]` + post$b_gen_time < 0)
mean(post$`r_estuary[Apalachicola.Bay,gen_time]` + post$b_gen_time < 0)
mean(post$`r_estuary[Tampa.Bay,gen_time]` + post$b_gen_time < 0)
mean(post$`r_estuary[Charlotte.Harbor,gen_time]` + post$b_gen_time < 0)
mean(post$`r_estuary[Cedar.Key,gen_time]` + post$b_gen_time < 0)
mean(post$`r_estuary[Northern.Indian.River,gen_time]` + post$b_gen_time < 0)

### species evenness ----
mean(post$`r_estuary[Northeast.Florida,species_evenness]` + post$b_species_evenness < 0)
mean(post$`r_estuary[Southern.Indian.River,species_evenness]` + post$b_species_evenness < 0)
mean(post$`r_estuary[Apalachicola.Bay,species_evenness]` + post$b_species_evenness < 0)
mean(post$`r_estuary[Tampa.Bay,species_evenness]` + post$b_species_evenness < 0)
mean(post$`r_estuary[Charlotte.Harbor,species_evenness]` + post$b_species_evenness < 0)
mean(post$`r_estuary[Cedar.Key,species_evenness]` + post$b_species_evenness < 0)
mean(post$`r_estuary[Northern.Indian.River,species_evenness]` + post$b_species_evenness < 0)

### max size ----
mean(post$`r_estuary[Northeast.Florida,tl_max]` + post$b_tl_max < 0)
mean(post$`r_estuary[Southern.Indian.River,tl_max]` + post$b_tl_max < 0)
mean(post$`r_estuary[Apalachicola.Bay,tl_max]` + post$b_tl_max < 0)
mean(post$`r_estuary[Tampa.Bay,tl_max]` + post$b_tl_max < 0)
mean(post$`r_estuary[Charlotte.Harbor,tl_max]` + post$b_tl_max < 0)
mean(post$`r_estuary[Cedar.Key,tl_max]` + post$b_tl_max < 0)
mean(post$`r_estuary[Northern.Indian.River,tl_max]` + post$b_tl_max < 0)

### species synchrony ----
mean(post$`r_estuary[Northeast.Florida,species_synchrony]` + post$b_species_synchrony < 0)
mean(post$`r_estuary[Southern.Indian.River,species_synchrony]` + post$b_species_synchrony < 0)
mean(post$`r_estuary[Apalachicola.Bay,species_synchrony]` + post$b_species_synchrony < 0)
mean(post$`r_estuary[Tampa.Bay,species_synchrony]` + post$b_species_synchrony < 0)
mean(post$`r_estuary[Charlotte.Harbor,species_synchrony]` + post$b_species_synchrony < 0)
mean(post$`r_estuary[Cedar.Key,species_synchrony]` + post$b_species_synchrony < 0)
mean(post$`r_estuary[Northern.Indian.River,species_synchrony]` + post$b_species_synchrony < 0)

# generation time visualizations ------------------------------------------

# random effects
re95 = mixedup::extract_random_coefs(mod, ci_level = c(0.95)) |> 
      filter(effect %in% c('Intercept', 'gen_time'))
re80 = mixedup::extract_random_coefs(mod, ci_level = c(0.8)) |> 
      filter(effect %in% c('Intercept', 'gen_time'))

re_beta = left_join(re95, re80) |> 
      rename(term = effect,
             estuary = group)

# fixed effects
fe95 = mixedup::extract_fixed_effects(mod, ci_level = c(0.95)) |> 
      filter(term %in% c('Intercept', 'gen_time'))

fe80 = mixedup::extract_fixed_effects(mod, ci_level = c(0.8)) |> 
      filter(term %in% c('Intercept', 'gen_time'))

fe_beta = left_join(fe95, fe80) |> 
      mutate(estuary = 'Overall') 

df_beta = bind_rows(re_beta, fe_beta) |> 
      filter(term != 'Intercept') |> 
      mutate(estuary = case_when(
            estuary == "Northeast.Florida" ~ "Northeast Florida",
            estuary == "Southern.Indian.River" ~ "Southern Indian River",
            estuary == "Apalachicola.Bay" ~ "Apalachicola Bay",
            estuary == "Tampa.Bay" ~ "Tampa Bay",
            estuary == "Charlotte.Harbor" ~ "Charlotte Harbor",
            estuary == "Cedar.Key" ~ "Cedar Key", 
            estuary == "Northern.Indian.River" ~ "Northern Indian River",
      )) |> 
      mutate(estuary = factor(estuary, levels = est)) |> 
      filter(!is.na(estuary))

# make equation data set
df_eq = bind_rows(re_beta, fe_beta) |> 
      select(term, estuary, value) |> 
      pivot_wider(names_from = term, values_from = value)  |> 
      rename(beta = gen_time) |> 
      mutate(estuary = case_when(
            estuary == "Northeast.Florida" ~ "Northeast Florida",
            estuary == "Southern.Indian.River" ~ "Southern Indian River",
            estuary == "Apalachicola.Bay" ~ "Apalachicola Bay",
            estuary == "Tampa.Bay" ~ "Tampa Bay",
            estuary == "Charlotte.Harbor" ~ "Charlotte Harbor",
            estuary == "Cedar.Key" ~ "Cedar Key", 
            estuary == "Northern.Indian.River" ~ "Northern Indian River",
      )) |> 
      filter(!is.na(estuary))

dat <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
      select(estuary,
             value = gen_time_mean) |>
      distinct() |> 
      group_by(estuary) |> 
      mutate(scaled = scale(value)) |> 
      slice(c(which.min(scaled), which.max(scaled))) |> 
      ungroup()

dat_scaled = dat |>
      group_by(estuary, scaled) |> 
      mutate(i = row_number()) |> 
      select(estuary, scaled)

raw <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
      select(estuary,
             value = gen_time_mean,
             stab = biomass_stability)

df <- dat_scaled |> 
      left_join(df_eq) |> 
      mutate(pred = beta*scaled + Intercept,
             stab = pred*sd(raw$stab) + mean(raw$stab)) |> 
      left_join(dat) |> 
      mutate(estuary = factor(estuary, levels = est)) 

a = df |>
      ggplot(aes(value, stab, color = estuary))+
      geom_point(data = raw, aes(value, stab, color = estuary), size = 2) +
      geom_line(linewidth = 1.75) +
      scale_color_manual(values = estuary_palette) +
      labs(y = 'Biomass Stability', title = 'Generation Time', x = NULL)+
      theme_classic()+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
a

raw <- raw |> 
      mutate(estuary = case_when(
            estuary == "Northeast Florida" ~ "JX",
            estuary == "Southern Indian River" ~ "SIR",
            estuary == "Apalachicola Bay" ~ "AP",
            estuary == "Tampa Bay" ~ "TB",
            estuary == "Charlotte Harbor" ~ "CH",
            estuary == "Cedar Key" ~ "CK",
            estuary == "Northern Indian River" ~ "NIR"
      )) |>
      mutate(estuary = factor(estuary, levels = est_abb))

b = df_beta |> 
      mutate(estuary = case_when(
            estuary == "Northeast Florida" ~ "JX",
            estuary == "Southern Indian River" ~ "SIR",
            estuary == "Apalachicola Bay" ~ "AP",
            estuary == "Tampa Bay" ~ "TB",
            estuary == "Charlotte Harbor" ~ "CH",
            estuary == "Cedar Key" ~ "CK",
            estuary == "Northern Indian River" ~ "NIR"
      )) |>
      mutate(estuary = factor(estuary, levels = est_abb)) |> 
      ggplot(aes(estuary, value, color = estuary)) +
      geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = 0.75) +
      geom_pointrange(aes(ymin = lower_10, ymax = upper_90), linewidth = 2)+
      geom_pointrange(aes(ymin = lower_2.5, ymax = upper_97.5), linewidth = 1, size = .9) +
      labs(y = 'Beta', x = 'Estuary')+
      scale_color_manual(values = estuary_palette_abb) +
      scale_y_continuous(breaks = c(-1.0, -0.5, 0.0, 0.5, 1.0, 1.5), limits = c(-1.0, 1.52)) +
      coord_flip()+
      theme_classic()+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
b

ggpubr::ggarrange(a,b, align = 'h')

### clean environment ----
rm(list = setdiff(ls(), c("a", "b", 'mod', 'est', 'estuary_palette', 'est_abb', 'estuary_palette_abb')))

# species evenness visualizations -----------------------------------------
# random effects
re95 = mixedup::extract_random_coefs(mod, ci_level = c(0.95)) |> 
      filter(effect %in% c('Intercept', 'species_evenness'))
re80 = mixedup::extract_random_coefs(mod, ci_level = c(0.8)) |> 
      filter(effect %in% c('Intercept', 'species_evenness'))

re_beta = left_join(re95, re80) |> 
      rename(term = effect,
             estuary = group)

# fixed effects
fe95 = mixedup::extract_fixed_effects(mod, ci_level = c(0.95)) |> 
      filter(term %in% c('Intercept', 'species_evenness'))

fe80 = mixedup::extract_fixed_effects(mod, ci_level = c(0.8)) |> 
      filter(term %in% c('Intercept', 'species_evenness'))

fe_beta = left_join(fe95, fe80) |> 
      mutate(estuary = 'Overall') 

df_beta = bind_rows(re_beta, fe_beta) |> 
      filter(term != 'Intercept') |> 
      mutate(estuary = case_when(
            estuary == "Northeast.Florida" ~ "Northeast Florida",
            estuary == "Southern.Indian.River" ~ "Southern Indian River",
            estuary == "Apalachicola.Bay" ~ "Apalachicola Bay",
            estuary == "Tampa.Bay" ~ "Tampa Bay",
            estuary == "Charlotte.Harbor" ~ "Charlotte Harbor",
            estuary == "Cedar.Key" ~ "Cedar Key", 
            estuary == "Northern.Indian.River" ~ "Northern Indian River",
      )) |> 
      mutate(estuary = factor(estuary, levels = est)) |> 
      filter(!is.na(estuary))

# make equation data set
df_eq = bind_rows(re_beta, fe_beta) |> 
      select(term, estuary, value) |> 
      pivot_wider(names_from = term, values_from = value)  |> 
      rename(beta = species_evenness) |> 
      mutate(estuary = case_when(
            estuary == "Northeast.Florida" ~ "Northeast Florida",
            estuary == "Southern.Indian.River" ~ "Southern Indian River",
            estuary == "Apalachicola.Bay" ~ "Apalachicola Bay",
            estuary == "Tampa.Bay" ~ "Tampa Bay",
            estuary == "Charlotte.Harbor" ~ "Charlotte Harbor",
            estuary == "Cedar.Key" ~ "Cedar Key", 
            estuary == "Northern.Indian.River" ~ "Northern Indian River",
      )) |> 
      filter(!is.na(estuary))

dat <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
      select(estuary,
             value = species_evenness_mean) |>
      distinct() |> 
      group_by(estuary) |> 
      mutate(scaled = scale(value)) |> 
      slice(c(which.min(scaled), which.max(scaled))) |> 
      ungroup()

dat_scaled = dat |>
      group_by(estuary, scaled) |> 
      mutate(i = row_number()) |> 
      select(estuary, scaled)

raw <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
      select(estuary,
             value = species_evenness_mean,
             stab = biomass_stability) 

df <- dat_scaled |> 
      left_join(df_eq) |> 
      mutate(pred = beta*scaled + Intercept,
             stab = pred*sd(raw$stab) + mean(raw$stab)) |> 
      left_join(dat) |> 
      mutate(estuary = factor(estuary, levels = est))

c = df |>
      ggplot(aes(value, stab, color = estuary))+
      geom_point(data = raw, aes(value, stab, color = estuary), size = 2) +
      geom_line(linewidth = 1.75) +
      scale_color_manual(values = estuary_palette) +
      labs(y = 'Biomass Stability', title = 'Species Evenness', x = NULL)+
      theme_classic()+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
c

raw <- raw |> 
      mutate(estuary = case_when(
            estuary == "Northeast Florida" ~ "JX",
            estuary == "Southern Indian River" ~ "SIR",
            estuary == "Apalachicola Bay" ~ "AP",
            estuary == "Tampa Bay" ~ "TB",
            estuary == "Charlotte Harbor" ~ "CH",
            estuary == "Cedar Key" ~ "CK",
            estuary == "Northern Indian River" ~ "NIR"
      )) |>
      mutate(estuary = factor(estuary, levels = est_abb))

d = df_beta |> 
      mutate(estuary = case_when(
            estuary == "Northeast Florida" ~ "JX",
            estuary == "Southern Indian River" ~ "SIR",
            estuary == "Apalachicola Bay" ~ "AP",
            estuary == "Tampa Bay" ~ "TB",
            estuary == "Charlotte Harbor" ~ "CH",
            estuary == "Cedar Key" ~ "CK",
            estuary == "Northern Indian River" ~ "NIR"
      )) |>
      mutate(estuary = factor(estuary, levels = est_abb)) |> 
      ggplot(aes(estuary, value, color = estuary)) +
      geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = 0.75) +
      geom_pointrange(aes(ymin = lower_10, ymax = upper_90), linewidth = 2)+
      geom_pointrange(aes(ymin = lower_2.5, ymax = upper_97.5), linewidth = 1, size = .9) +
      labs(y = 'Beta', x = 'Estuary')+
      scale_color_manual(values = estuary_palette_abb) +
      scale_y_continuous(breaks = c(-1.0, -0.5, 0.0, 0.5, 1.0, 1.5), limits = c(-1.0, 1.52)) +
      coord_flip()+
      theme_classic()+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
d

ggpubr::ggarrange(c,d, align = 'h')

### clean environment ----
rm(list = setdiff(ls(), c("a", "b", "c", "d", 'mod', 'est', 'estuary_palette', 'est_abb', 'estuary_palette_abb')))

# mean max size visualizations ------------------------------------------

# random effects
re95 = mixedup::extract_random_coefs(mod, ci_level = c(0.95)) |> 
      filter(effect %in% c('Intercept', 'tl_max'))
re80 = mixedup::extract_random_coefs(mod, ci_level = c(0.8)) |> 
      filter(effect %in% c('Intercept', 'tl_max'))

re_beta = left_join(re95, re80) |> 
      rename(term = effect,
             estuary = group)

# fixed effects
fe95 = mixedup::extract_fixed_effects(mod, ci_level = c(0.95)) |> 
      filter(term %in% c('Intercept', 'tl_max'))

fe80 = mixedup::extract_fixed_effects(mod, ci_level = c(0.8)) |> 
      filter(term %in% c('Intercept', 'tl_max'))

fe_beta = left_join(fe95, fe80) |> 
      mutate(estuary = 'Overall') 

df_beta = bind_rows(re_beta, fe_beta) |> 
      filter(term != 'Intercept') |> 
      mutate(estuary = case_when(
            estuary == "Northeast.Florida" ~ "Northeast Florida",
            estuary == "Southern.Indian.River" ~ "Southern Indian River",
            estuary == "Apalachicola.Bay" ~ "Apalachicola Bay",
            estuary == "Tampa.Bay" ~ "Tampa Bay",
            estuary == "Charlotte.Harbor" ~ "Charlotte Harbor",
            estuary == "Cedar.Key" ~ "Cedar Key", 
            estuary == "Northern.Indian.River" ~ "Northern Indian River",
      )) |> 
      mutate(estuary = factor(estuary, levels = est)) |> 
      filter(!is.na(estuary))

# make equation data set
df_eq = bind_rows(re_beta, fe_beta) |> 
      select(term, estuary, value) |> 
      pivot_wider(names_from = term, values_from = value)  |> 
      rename(beta = tl_max) |> 
      mutate(estuary = case_when(
            estuary == "Northeast.Florida" ~ "Northeast Florida",
            estuary == "Southern.Indian.River" ~ "Southern Indian River",
            estuary == "Apalachicola.Bay" ~ "Apalachicola Bay",
            estuary == "Tampa.Bay" ~ "Tampa Bay",
            estuary == "Charlotte.Harbor" ~ "Charlotte Harbor",
            estuary == "Cedar.Key" ~ "Cedar Key", 
            estuary == "Northern.Indian.River" ~ "Northern Indian River",
      )) |> 
      filter(!is.na(estuary))

dat <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
      select(estuary,
             value = tl_max_mean) |>
      distinct() |> 
      group_by(estuary) |> 
      mutate(scaled = scale(value)) |> 
      slice(c(which.min(scaled), which.max(scaled))) |> 
      ungroup()

dat_scaled = dat |>
      group_by(estuary, scaled) |> 
      mutate(i = row_number()) |> 
      select(estuary, scaled)

raw <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
      select(estuary,
             value = tl_max_mean,
             stab = biomass_stability) 

df <- dat_scaled |> 
      left_join(df_eq) |> 
      mutate(pred = beta*scaled + Intercept,
             stab = pred*sd(raw$stab) + mean(raw$stab)) |> 
      left_join(dat) |> 
      mutate(estuary = factor(estuary, levels = est))

e = df |>
      ggplot(aes(value, stab, color = estuary))+
      geom_point(data = raw, aes(value, stab, color = estuary), size = 2) +
      geom_line(linewidth = 1.75) +
      scale_color_manual(values = estuary_palette) +
      labs(y = 'Biomass Stability', title = 'Mean Max Size', x = NULL)+
      theme_classic()+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
e

raw <- raw |> 
      mutate(estuary = case_when(
            estuary == "Northeast Florida" ~ "JX",
            estuary == "Southern Indian River" ~ "SIR",
            estuary == "Apalachicola Bay" ~ "AP",
            estuary == "Tampa Bay" ~ "TB",
            estuary == "Charlotte Harbor" ~ "CH",
            estuary == "Cedar Key" ~ "CK",
            estuary == "Northern Indian River" ~ "NIR"
      )) |>
      mutate(estuary = factor(estuary, levels = est_abb))

f = df_beta |> 
      mutate(estuary = case_when(
            estuary == "Northeast Florida" ~ "JX",
            estuary == "Southern Indian River" ~ "SIR",
            estuary == "Apalachicola Bay" ~ "AP",
            estuary == "Tampa Bay" ~ "TB",
            estuary == "Charlotte Harbor" ~ "CH",
            estuary == "Cedar Key" ~ "CK",
            estuary == "Northern Indian River" ~ "NIR"
      )) |>
      mutate(estuary = factor(estuary, levels = est_abb)) |> 
      ggplot(aes(estuary, value, color = estuary)) +
      geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = 0.75) +
      geom_pointrange(aes(ymin = lower_10, ymax = upper_90), linewidth = 2)+
      geom_pointrange(aes(ymin = lower_2.5, ymax = upper_97.5), linewidth = 1, size = .9) +
      labs(y = 'Beta', x = 'Estuary')+
      scale_color_manual(values = estuary_palette_abb) +
      scale_y_continuous(breaks = c(-1.0, -0.5, 0.0, 0.5, 1.0, 1.5), limits = c(-1.0, 1.52)) +
      coord_flip()+
      theme_classic()+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
f

ggpubr::ggarrange(e,f, align = 'h')

### clean environment ----
rm(list = setdiff(ls(), c("a", "b", "c", "d", "e", "f", 'mod', 'est', 'estuary_palette', 'est_abb', 'estuary_palette_abb')))

# species synchrony visualizations ------------------------------------------

# random effects
re95 = mixedup::extract_random_coefs(mod, ci_level = c(0.95)) |> 
      filter(effect %in% c('Intercept', 'species_synchrony'))
re80 = mixedup::extract_random_coefs(mod, ci_level = c(0.8)) |> 
      filter(effect %in% c('Intercept', 'species_synchrony'))

re_beta = left_join(re95, re80) |> 
      rename(term = effect,
             estuary = group)

# fixed effects
fe95 = mixedup::extract_fixed_effects(mod, ci_level = c(0.95)) |> 
      filter(term %in% c('Intercept', 'species_synchrony'))

fe80 = mixedup::extract_fixed_effects(mod, ci_level = c(0.8)) |> 
      filter(term %in% c('Intercept', 'species_synchrony'))

fe_beta = left_join(fe95, fe80) |> 
      mutate(estuary = 'Overall') 

df_beta = bind_rows(re_beta, fe_beta) |> 
      filter(term != 'Intercept') |> 
      mutate(estuary = case_when(
            estuary == "Northeast.Florida" ~ "Northeast Florida",
            estuary == "Southern.Indian.River" ~ "Southern Indian River",
            estuary == "Apalachicola.Bay" ~ "Apalachicola Bay",
            estuary == "Tampa.Bay" ~ "Tampa Bay",
            estuary == "Charlotte.Harbor" ~ "Charlotte Harbor",
            estuary == "Cedar.Key" ~ "Cedar Key", 
            estuary == "Northern.Indian.River" ~ "Northern Indian River",
      )) |> 
      mutate(estuary = factor(estuary, levels = est)) |> 
      filter(!is.na(estuary))

# make equation data set
df_eq = bind_rows(re_beta, fe_beta) |> 
      select(term, estuary, value) |> 
      pivot_wider(names_from = term, values_from = value)  |> 
      rename(beta = species_synchrony) |> 
      mutate(estuary = case_when(
            estuary == "Northeast.Florida" ~ "Northeast Florida",
            estuary == "Southern.Indian.River" ~ "Southern Indian River",
            estuary == "Apalachicola.Bay" ~ "Apalachicola Bay",
            estuary == "Tampa.Bay" ~ "Tampa Bay",
            estuary == "Charlotte.Harbor" ~ "Charlotte Harbor",
            estuary == "Cedar.Key" ~ "Cedar Key", 
            estuary == "Northern.Indian.River" ~ "Northern Indian River",
      )) |> 
      filter(!is.na(estuary))

dat <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
      select(estuary,
             value = biomass_synchrony) |>
      distinct() |> 
      group_by(estuary) |> 
      mutate(scaled = scale(value)) |> 
      slice(c(which.min(scaled), which.max(scaled))) |> 
      ungroup()

dat_scaled = dat |>
      group_by(estuary, scaled) |> 
      mutate(i = row_number()) |> 
      select(estuary, scaled)

raw <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
      select(estuary,
             value = biomass_synchrony,
             stab = biomass_stability)

df <- dat_scaled |> 
      left_join(df_eq) |> 
      mutate(pred = beta*scaled + Intercept,
             stab = pred*sd(raw$stab) + mean(raw$stab)) |> 
      left_join(dat) |> 
      mutate(estuary = factor(estuary, levels = est))

g = df |>
      ggplot(aes(value, stab, color = estuary))+
      geom_point(data = raw, aes(value, stab, color = estuary), size = 2) +
      geom_line(linewidth = 1.75) +
      scale_color_manual(values = estuary_palette) +
      labs(y = 'Biomass Stability', title = 'Species Synchrony', x = NULL)+
      theme_classic()+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
g

raw <- raw |> 
      mutate(estuary = case_when(
            estuary == "Northeast Florida" ~ "JX",
            estuary == "Southern Indian River" ~ "SIR",
            estuary == "Apalachicola Bay" ~ "AP",
            estuary == "Tampa Bay" ~ "TB",
            estuary == "Charlotte Harbor" ~ "CH",
            estuary == "Cedar Key" ~ "CK",
            estuary == "Northern Indian River" ~ "NIR"
      )) |>
      mutate(estuary = factor(estuary, levels = est_abb))

h = df_beta |> 
      mutate(estuary = case_when(
            estuary == "Northeast Florida" ~ "JX",
            estuary == "Southern Indian River" ~ "SIR",
            estuary == "Apalachicola Bay" ~ "AP",
            estuary == "Tampa Bay" ~ "TB",
            estuary == "Charlotte Harbor" ~ "CH",
            estuary == "Cedar Key" ~ "CK",
            estuary == "Northern Indian River" ~ "NIR"
      )) |>
      mutate(estuary = factor(estuary, levels = est_abb)) |> 
      ggplot(aes(estuary, value, color = estuary)) +
      geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = 0.75) +
      geom_pointrange(aes(ymin = lower_10, ymax = upper_90), linewidth = 2)+
      geom_pointrange(aes(ymin = lower_2.5, ymax = upper_97.5), linewidth = 1, size = .9) +
      labs(y = 'Beta', x = 'Estuary')+
      scale_color_manual(values = estuary_palette_abb) +
      scale_y_continuous(breaks = c(-1.0, -0.5, 0.0, 0.5, 1.0, 1.5), limits = c(-1.0, 1.52)) +
      coord_flip()+
      theme_classic()+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
h

ggpubr::ggarrange(g,h, align = 'h')

### clean environment ----
rm(list = setdiff(ls(), c("a", "b", "c", "d", "e", "f", "g", "h", 'mod', 'est', 'estuary_palette', 'est_abb', 'estuary_palette_abb')))

top <- a + c + e + g + plot_layout(nrow = 1, guides = "collect") & theme(legend.position = "none")
bot <- b + d + f + h + plot_layout(nrow = 1, guides = "collect") & theme(legend.position = "none")

final_plot <- top / bot + plot_layout(heights = c(1, 1))
final_plot

# Assume a, c, e, g are top-row plots
# and b, d, f, h are bottom-row plots

# Remove redundant axis titles
a <- a + labs(y = NULL) + labs(tag = "a") + theme(plot.tag = element_text(size = 14, face = "bold"),
                                                  plot.tag.position = c(0.02, 1))
c <- c + labs(y = NULL)
e <- e + labs(y = NULL)
g <- g + labs(y = NULL)

b <- b + labs(y = NULL, x = NULL) + labs(tag = "b") + theme(plot.tag = element_text(size = 14, face = "bold"),
                                                            plot.tag.position = c(0.02, 1))
d <- d + labs(y = NULL, x = NULL)
f <- f + labs(y = NULL, x = NULL)
h <- h + labs(y = NULL, x = NULL)

# Create row layouts
top_row <- a + c + e + g + plot_layout(nrow = 1) 
bot_row <- b + d + f + h + plot_layout(nrow = 1)

# Combine with a shared y-axis and x-axis annotation
final_plot <- (top_row / bot_row) +
      plot_layout(heights = c(1, 1)) +
      plot_annotation() +
      theme(legend.position = "none")

# Add shared axis labels using annotation
final_plot <- final_plot +
      plot_annotation(
            title = NULL,
            subtitle = NULL,
            caption = NULL,
            theme = theme(
                  plot.margin = margin(10, 10, 10, 10)
            )
      ) &
      theme(
            plot.title = element_text(hjust = 0.5),
            axis.title.y = element_blank(),
            axis.title.x = element_blank()
      )

grid::grid.newpage()
grid::grid.draw(
      patchwork::wrap_elements(full = final_plot) +
            
            # Y-axis label for the top row (Biomass Stability)
            patchwork::inset_element(
                  grid::textGrob("Biomass Stability", rot = 90, gp = gpar(fontsize = 14, fontface = "bold")),
                  left = 0, bottom = 0.55, right = 0.02, top = 0.95, align_to = "full"
            ) +
            
            # X-axis label for the bottom row (Beta)
            patchwork::inset_element(
                  grid::textGrob("Beta", gp = gpar(fontsize = 14, fontface = "bold")),
                  left = 0.3, bottom = 0.0, right = 0.7, top = 0.03, align_to = "full"
            ) +
            
            # Y-axis label for the bottom row (Estuary)
            patchwork::inset_element(
                  grid::textGrob("Estuary", rot = 90, gp = gpar(fontsize = 14, fontface = "bold")),
                  left = 0, bottom = 0.05, right = 0.019, top = 0.45, align_to = "full"
            )
)

# ggsave('figs/brms-model-output-two-panel.png',
#        dpi = 600, units= 'in', height = 6, width = 12)
