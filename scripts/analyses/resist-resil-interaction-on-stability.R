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
librarian::shelf(tidyverse, readr, zoo, MuMIn, corrplot, mgcv, performance, ggeffects,
                 ggpubr, parameters, ggstats, brms, mixedup, multcompView, FSA, grid, patchwork, ggcorrplot)

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

resist <- gam(y ~ s(resistance),
            family = gaussian(link = 'log'),
            data = dat,
            method = "REML")
concurvity(resist)
performance(resist)

resil <- gam(y ~ s(resilience),
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
      resilience = seq(from = -0.3989, to = 2.4780, by = 0.01)
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
cor(dat$resilience, dat$resistance)

### bayesian regression models ----

df <- dat |> 
      group_by(estuary, zone) |> 
      summarize(y = mean(y),
                resistance = mean(resistance),
                resilience = mean(resilience),
                .groups = 'drop') |> 
      mutate(y = scale(y, center = TRUE)) |> 
      mutate(across(resistance:resilience, \(x) scale(x, center = TRUE)))

### set priors following Lemoine (2019, Ecology)
pr = prior(normal(0, 1), class = 'b')

m1 <- brm(y ~ resistance + (resistance|estuary),
          data = df, prior = pr, warmup = 1000, iter = 100000, chains = 4)

m2 <- brm(y ~ resilience + (resilience|estuary),
          data = df, prior = pr, warmup = 1000, iter = 100000, chains = 4)

m3 <- brm(y ~ resistance + resilience + (resistance + resilience| estuary),
          data = df, prior = pr, warmup = 1000, iter = 100000, chains = 4)

model_list <- list(
      m1 = m1,
      m2 = m2, 
      m3 = m3
)

waic_table <- tibble(
      model = names(model_list),
      formula = map_chr(model_list, ~ paste(deparse(formula(.x)), collapse = " ")),
      waic = map_dbl(model_list, ~ waic(.x)$estimates["waic", "Estimate"])
) |> 
      arrange(waic) |> 
      mutate(delta_waic = waic - min(waic))

write_csv(waic_table, "tables/foragefish_brms_stabilityresistresil.csv")

save(m1, file = "models/stability-resist-resil-brmsmodel.RData")
write_rds(m1, 'models/stability-resist-resil-brmsmodel.rds')

rm(list = setdiff(ls(), c("dat", "m1")))
mod <- readRDS('models/stability-resist-resil-brmsmodel.rds')
# summary stats -----------------------------------------------------------
post = posterior_samples(mod)

### resistance ----
mean(post$`r_estuary[Northeast.Florida,resistance]` + post$b_resistance < 0)
mean(post$`r_estuary[Southern.Indian.River,resistance]` + post$b_resistance < 0)
mean(post$`r_estuary[Apalachicola.Bay,resistance]` + post$b_resistance < 0)
mean(post$`r_estuary[Tampa.Bay,resistance]` + post$b_resistance < 0)
mean(post$`r_estuary[Charlotte.Harbor,resistance]` + post$b_resistance < 0)
mean(post$`r_estuary[Cedar.Key,resistance]` + post$b_resistance < 0)
mean(post$`r_estuary[Northern.Indian.River,resistance]` + post$b_resistance < 0)

# resistance visualizations ------------------------------------------

# random effects
re95 = mixedup::extract_random_coefs(mod, ci_level = c(0.95)) |> 
      filter(effect %in% c('Intercept', 'resistance'))
re80 = mixedup::extract_random_coefs(mod, ci_level = c(0.8)) |> 
      filter(effect %in% c('Intercept', 'resistance'))

re_beta = left_join(re95, re80) |> 
      rename(term = effect,
             estuary = group)

# fixed effects
fe95 = mixedup::extract_fixed_effects(mod, ci_level = c(0.95)) |> 
      filter(term %in% c('Intercept', 'resistance'))

fe80 = mixedup::extract_fixed_effects(mod, ci_level = c(0.8)) |> 
      filter(term %in% c('Intercept', 'resistance'))

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
      rename(beta = resistance) |> 
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
             value = resistance) |>
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
             value = resistance,
             stab = biomass_stability)

df <- dat_scaled |> 
      left_join(df_eq) |> 
      mutate(pred = beta*scaled + Intercept,
             stab = pred*sd(raw$stab) + mean(raw$stab)) |> 
      left_join(dat) |> 
      mutate(estuary = factor(estuary, levels = est)) 

a <- df |>
      ggplot(aes(value, stab, color = estuary))+
      geom_point(data = raw, aes(value, stab, color = estuary), size = 2) +
      geom_line(linewidth = 1.75) +
      scale_color_manual(values = estuary_palette) +
      labs(y = 'Biomass Stability', title = NULL, x = "Resistance")+
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
      labs(y = 'Beta', x = NULL, title = NULL)+
      scale_color_manual(values = estuary_palette_abb) +
      # scale_y_continuous(breaks = c(-0.8, -0.6, -0.4, -0.2, 0.0, 0.2, 0.4), limits = c(-0.8, 0.5)) +
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

dat <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |> 
      mutate(estuary = as.factor(estuary),
             y = biomass_stability) |> 
      group_by(estuary, zone) |> 
      summarize(y = mean(y), 
                resistance = mean(resistance),
                resilience = mean(resilience),
                .groups = 'drop')
glimpse(dat)

m1 <- lm(y ~ resistance, data = dat)
summary(m1)

m2 <- lm(y ~ resilience, data = dat)
summary(m2)

m3 <- lm(resistance ~ resilience, data = dat)
summary(m3)

c <- dat |> 
      ggplot(aes(x = resistance, y = y)) +
      geom_jitter(aes(color = estuary), alpha = 1, size = 2.5) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      scale_fill_manual(values = estuary_palette) +
      scale_color_manual(values = estuary_palette) +
      labs(y = 'Biomass Stability', x = 'Resistance', title = NULL) +
      annotate('text',
               x = 3.2, y = 0.6,
               label = bquote({R^2} == 0.34),
               size = 3.2) +
      annotate('text',
               x = 3.25, y = 0.57,
               label = bquote(italic(p) < 6 %*% 10^-5),
               size = 3.2) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 12))

d <- dat |> 
      ggplot(aes(x = resilience, y = y)) +
      geom_jitter(aes(color = estuary), alpha = 1, size = 2.5) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      scale_fill_manual(values = estuary_palette) +
      scale_color_manual(values = estuary_palette) +
      labs(y = 'Biomass Stability', x = 'Resilience', title = NULL) +
      annotate('text',
               x = -1.1, y = 0.6,
               label = bquote({R^2} < 0.01),
               size = 3.2) +
      annotate('text',
               x = -1.1, y = 0.57,
               label = bquote(italic(p) == 0.988),
               size = 3.2) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 12))

e <- dat |> 
      ggplot(aes(x = resilience, y = resistance)) +
      geom_jitter(aes(color = estuary), alpha = 1, size = 2.5) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      scale_fill_manual(values = estuary_palette) +
      scale_color_manual(values = estuary_palette) +
      labs(y = 'Resistance', x = 'Resilience', title = NULL) +
      annotate('text',
               x = -1.1, y = 6.3,
               label = bquote({R^2} < 0.01),
               size = 3.2) +
      annotate('text',
               x = -1.1, y = 6.0,
               label = bquote(italic(p) == 0.646),
               size = 3.2) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 12))

### clean environment ----
rm(list = setdiff(ls(), c("a", "b", "c", "d", "e", 'mod', 'df', 'est', 'estuary_palette', 'est_abb', 'estuary_palette_abb')))
a
b
c
d
e


# Remove redundant axis titles
a <- a + labs(tag = "d") + theme(plot.tag = element_text(size = 14, face = "bold"),
                                                  plot.tag.position = c(0.02, 1))

b <- b + labs(tag = "e") + theme(plot.tag = element_text(size = 14, face = "bold"),
                                                            plot.tag.position = c(0.02, 1))

c <- c + labs(tag = "a") + theme(plot.tag = element_text(size = 14, face = "bold"),
                                 plot.tag.position = c(0.02, 1))

d <- d + labs(tag = "b") + theme(plot.tag = element_text(size = 14, face = "bold"),
                                 plot.tag.position = c(0.02, 1))

e <- e + labs(tag = "c") + theme(plot.tag = element_text(size = 14, face = "bold"),
                                 plot.tag.position = c(0.02, 1))

bot <- a + b + plot_layout(nrow = 1, guides = "collect") & theme(legend.position = "none",
                                                                 plot.margin = margin(10, 10, 10, 10))

top <- c + d + e + plot_layout(nrow = 1, guides = "collect") & theme(legend.position = "none",
                                                                     plot.margin = margin(10, 10, 10, 10))

final_plot <- top / bot +
      plot_layout(heights = c(0.35, 0.65)) 
      
final_plot
# r2(mod)
# Add shared axis labels using annotation
grid::grid.newpage()
grid::grid.draw(
      patchwork::wrap_elements(full = final_plot) +
            
            # Add R2 and Marginal R2 at bottom center
            patchwork::inset_element(
                  grid::textGrob("R² = 0.52; Marginal R² = 0.14", gp = gpar(fontsize = 12, fontface = "italic")),
                  left = 0.25, bottom = 0.00, right = 0.75, top = 0.01, align_to = "full"
            )
)

ggsave('figs/resist-resilience-five-panel.png',
       dpi = 600, units= 'in', height = 7, width = 9)

df <- dat |> 
      group_by(estuary, bay, zone) |> 
      summarize(xsr = mean(raw_species_richness_mean),
             xse = mean(raw_species_evenness_mean),
             xgt = mean(raw_gen_time_mean),
             xmd = mean(raw_depth_max_mean),
             y = mean(biomass_synchrony),
             yresist = mean(resistance),
             yresil = mean(resilience),
             .groups = 'drop') 
df |> 
      ggplot(aes(x = xse, y = y)) +
      geom_point(aes(color = estuary))

m<-lm(y ~ xse, dat = df)
summary(m)

m<-lm(biomass_synchrony ~ raw_species_evenness_mean, dat = dat)
summary(m)

dat |> 
      mutate(bay = case_when(
            bay == 'TQ' ~ "SIR",
            bay == 'IR' ~ 'NIR',
            TRUE ~ bay
      )) |> 
      ggplot(aes(x = raw_species_evenness_mean, y = biomass_synchrony)) +
      geom_jitter(aes(color = bay), alpha = 1, size = 3) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(y = 'Species Synchrony', x = 'Species Evenness', fill = 'Estuary',
           color = 'Estuary', title = NULL) +
      annotate('text',
               x = 1.3, y = 0.46,
               label = bquote({R^2} == 0.22),
               size = 5) +
      annotate('text',
               x = 1.29, y = 0.435,
               label = bquote(italic(p) < 2 %*% 10^-16),
               size = 5) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = c(0.85, 0.61),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 12))

ggsave('figs/synch-evenness-regression.png',
       dpi = 600, units= 'in', height = 5, width = 5.3)

m <-lm(yresist ~ xmd, dat = df)
summary(m)

m <-lm(resistance ~ raw_depth_max_mean, dat = dat)
summary(m)

df |> 
      mutate(bay = case_when(
            bay == 'TQ' ~ "SIR",
            bay == 'IR' ~ 'NIR',
            TRUE ~ bay
      )) |> 
      filter(xmd <= 70) |> 
      ggplot(aes(x = xmd, y = y)) +
      geom_jitter(aes(color = bay), alpha = 1, size = 3) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(y = 'Resistance', x = 'Max Depth', fill = 'Estuary',
           color = 'Estuary', title = NULL) +
      annotate('text',
               x = 35.3, y = 0.46,
               label = bquote({R^2} == 0.15),
               size = 5) +
      annotate('text',
               x = 36.6, y = 0.44,
               label = bquote(italic(p) < 2 %*% 10^-16),
               size = 5) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = c(0.93, 0.25),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 12))

ggsave('figs/maxdepth-resist-regression.png',
       dpi = 600, units= 'in', height = 5, width = 5.3)


m <-lm(yresist ~ xgt, dat = df)
summary(m)

df |> 
      mutate(bay = case_when(
            bay == 'TQ' ~ "SIR",
            bay == 'IR' ~ 'NIR',
            TRUE ~ bay
      )) |> 
      # filter(bay %in% c('SIR', 'TB', 'CH')) |> 
      ggplot(aes(x = xgt, y = y)) +
      geom_jitter(aes(color = bay), alpha = 1, size = 3) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(y = 'Resistance', x = 'Generation Time', fill = 'Estuary',
           color = 'Estuary', title = NULL) +
      annotate('text',
               x = 2.58, y = 0.46,
               label = bquote({R^2} == 0.001),
               size = 5) +
      annotate('text',
               x = 2.58, y = 0.44,
               label = bquote(italic(p) == 0.33),
               size = 5) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = "right",
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 12))

ggsave('figs/generationtime-resist-regression.png',
       dpi = 600, units= 'in', height = 5, width = 5.3)
