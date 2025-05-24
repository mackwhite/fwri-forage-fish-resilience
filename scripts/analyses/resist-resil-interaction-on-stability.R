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

estuary_palette_abb = c("AP"="#e6d7b9",
                        "CH"="#a7c4a0",
                        "CK"="#64a988",
                        'NIR'="#89c8d9",
                        "JX"="#f2c6b4",
                        'TB'='#dba5a4',
                        "SIR"="#bfaed9")

disturbance_palette = c("Cold Snap" = "#4575b4",
                        "Heat Wave" = "#d73027",
                        "Hurricane" = "#fdae61")

# est_abb <- c('AP', 'CK', 'TB', 'CH', 'SIR', 'NIR', 'JX')
est_abb <- c('JX', 'NIR', 'SIR', 'CH', 'TB', 'CK', 'AP')
### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readr, zoo, MuMIn, corrplot, mgcv, performance, ggeffects,
                 ggpubr, parameters, ggstats, brms, mixedup, multcompView, FSA, grid, patchwork)

### read in stability dataset ----
dat <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
      mutate(estuary = as.factor(estuary),
             y = biomass_stability)

### summarize to identify median + quantiles for each predictor ---
summary <- dat |> 
      summarize(
            ### resistance summary ---
            min_resistance = min(resistance, na.rm = TRUE),
            q25_resistance = quantile(resistance, 0.25, na.rm = TRUE),
            median_resistance = quantile(resistance, 0.50, na.rm = TRUE),
            q75_resistance = quantile(resistance, 0.75, na.rm = TRUE),
            max_resistance = max(resistance, na.rm = TRUE),
            ### resilience summary ---
            min_resilience = min(resilience, na.rm = TRUE),
            q25_resilience = quantile(resilience, 0.25, na.rm = TRUE),
            median_resilience = quantile(resilience, 0.50, na.rm = TRUE),
            q75_resilience = quantile(resilience, 0.75, na.rm = TRUE),
            max_resilience = max(resilience, na.rm = TRUE)
      )

resist <- gam(y ~ s(resistance, k = 2) + s(estuary, bs = "re"),
            family = gaussian(link = 'log'),
            data = dat,
            method = "REML")
concurvity(resist)
performance(resist)

resil <- gam(y ~ s(resilience, k = 2) + s(estuary, bs = "re"),
              family = gaussian(link = 'log'),
              data = dat,
              method = "REML")
concurvity(resil)
performance(resil)

int <- gam(y ~ te(resistance, resilience) + s(estuary, bs = "re"),
              family = gaussian(link = 'log'),
              data = dat,
              method = "REML")
concurvity(int)
performance(int)

performance::compare_performance(resist, resil, int)

### interactive effect of resistance and resilience -----

new_data <- expand.grid(
      resistance = seq(from = 1.812, to = 3.002, by = 0.001),
      resilience = seq(from = -0.3989, to = 2.4780, by = 0.001)
) |>
      mutate(estuary = dat$estuary[4])

pred <- predict(int, newdata=new_data, se.fit=TRUE)
new_data$fit <- pred$fit
new_data$se.fit <- pred$se.fit

pred_fit <- new_data |> 
      mutate(predicted_log = pred$fit,
             predicted = exp(predicted_log),
             lower_log = predicted_log - pred$se.fit,
             upper_log = predicted_log + pred$se.fit) |>
      mutate(lower = exp(lower_log),
             upper = exp(upper_log)) |>
      rename(x = resistance,
             y = resilience,
             z = predicted)

a <- pred_fit |> 
      ggplot(aes(x = x, y = y, fill = z)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma", direction = 1) + 
      labs(x = "Resistance", y = "Resilience", 
           fill = expression(bold("Biomass Stability"))) +
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            # plot.title = element_text(size = 16, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(face = 'bold', size = 8, color = "black"),
            legend.title = element_text(face = 'bold', size = 10, color = "black"))
a

### main effect of resistance -----

new_data <- expand.grid(
      resistance = seq(from = 1.812, to = 3.002, by = 0.01),
      resilience = 1.9289
) |>
      mutate(estuary = dat$estuary[4])

pred <- predict(int, newdata=new_data, se.fit=TRUE)
new_data$fit <- pred$fit
new_data$se.fit <- pred$se.fit

pred_fit <- new_data |> 
      mutate(predicted_log = pred$fit,
             predicted = exp(predicted_log),
             lower_log = predicted_log - pred$se.fit,
             upper_log = predicted_log + pred$se.fit) |>
      mutate(lower = exp(lower_log),
             upper = exp(upper_log)) |>
      rename(x = resistance,
             y = predicted)

b <- pred_fit |> 
      ggplot(aes(x = x, y = y)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
      geom_line(linewidth = 2, linetype = "solid") +
      theme_bw() +
      labs(x = "Resistance", y = "Biomass Stability") +
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            # plot.title = element_text(size = 16, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = 'bold'),
            legend.title = element_text(face = 'bold'))
b

### main effect of resilience -----

new_data <- expand.grid(
      resistance = 2.132,
      resilience = seq(from = 1.812, to = 3.002, by = 0.01)
) |>
      mutate(estuary = dat$estuary[4])

pred <- predict(int, newdata=new_data, se.fit=TRUE)
new_data$fit <- pred$fit
new_data$se.fit <- pred$se.fit

pred_fit <- new_data |> 
      mutate(predicted_log = pred$fit,
             predicted = exp(predicted_log),
             lower_log = predicted_log - pred$se.fit,
             upper_log = predicted_log + pred$se.fit) |>
      mutate(lower = exp(lower_log),
             upper = exp(upper_log)) |>
      rename(x = resilience,
             y = predicted)

c <- pred_fit |> 
      ggplot(aes(x = x, y = y)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
      geom_line(linewidth = 2, linetype = "solid") +
      theme_bw() +
      labs(x = "Resilience", y = "Biomass Stability") +
      theme(axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            # plot.title = element_text(size = 16, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = 'bold'),
            legend.title = element_text(face = 'bold'))
c