###project: Forage Fish Resilience
###author(s): MW
###goal(s): resistance and resilience calculations
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, FSA, readr, forcats, multcompView, readxl, dplyr, broom, ggpubr, lme4,
                 patchwork, splitstackshape, ggimage, purrr, zoo, pracma, vegan, e1071, codyn, lubridate, forecast)

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

estuary_palette_abb = c("AP"="#e6d7b9",
                        "CH"="#a7c4a0",
                        "CK"="#64a988",
                        'NIR'="#89c8d9",
                        "JX"="#f2c6b4",
                        'TB'='#dba5a4',
                        "SIR"="#bfaed9")

sig_palette = c("None" = "white",
                "Marginal" = "black",
                "Significant" = "red")

est <- c('Apalachicola Bay', 'Cedar Key', 'Tampa Bay', 'Charlotte Harbor',
         'Southern Indian River', 'Northern Indian River', 'Northeast Florida')

est_abb <- c('JX', 'NIR', 'SIR', 'CH', 'TB', 'CK', 'AP')
fish <- data.frame(x=1,y=0.5, image = 'figs/fish.png')
fish_icon <- "figs/fish.png"

### read in data ----
discrete <- read_csv("local-data/key-datasets/discrete-detrended-forage-fish-small-seine-time-series.csv") |> 
      group_by(bay, zone, year, month) |> 
      summarize(across(detrended_biomass_m2:detrended_species_evenness, \(x) mean(x)), .groups = "drop") |> 
      filter(year >= 1996) |> 
      rename_with(~ str_remove(.x, "^detrended_"), starts_with("detrended_")) |> 
      group_by(bay, zone, year) |> 
      summarize(across(biomass_m2:species_evenness, \(x) mean(x)), .groups = "drop") |> 
      mutate(bay = case_when(
            bay == "TQ" ~ "SIR",
            bay == "IR" ~ "NIR",
            TRUE ~ bay
      )) |> 
      arrange(bay, zone, year)

### scale and transform data accroding to Crossley et al 2020 -----
metrics <- c("biomass_m2", "abund_m2", "tl_max", "k", "gen_time", "troph", "species_richness", "species_evenness")

biomass_ts_long <- discrete |> 
      mutate(across(all_of(metrics), ~ log(.x + 1e-6), .names = "{.col}_log")) |> 
      mutate(across(ends_with("_log"), ~ scale(.x)[,1], .names = "{.col}_z")) |> 
      pivot_longer(
            cols = ends_with("_z"),
            names_to = "metric",
            values_to = "value"
      ) |> 
      mutate(
            variable = str_remove(metric, "_log_z"),
            year_scaled = (year - min(year)) / (max(year) - min(year))
      ) |> 
      group_by(bay, zone, variable) |> 
      filter(n_distinct(year) >= 4) |> 
      ungroup() |> 
      select(bay, zone, year, metric, value, variable, year_scaled)

trend_results_all <- biomass_ts_long |> 
      group_by(bay, zone, variable) |> 
      nest() |> 
      mutate(
            n_years = map_int(data, ~ n_distinct(.x$year)),
            
            model = map(data, ~ tryCatch(
                  gls(value ~ year_scaled, 
                      correlation = corAR1(form = ~ year_scaled), 
                      data = .x, method = "REML"),
                  error = function(e) tryCatch(
                        lm(value ~ year_scaled, data = .x), 
                        error = function(e2) NULL
                  )
            )),
            
            model_type = map_chr(model, ~ {
                  if (inherits(.x, "gls")) "gls_corAR1"
                  else if (inherits(.x, "lm")) "lm_fallback"
                  else "fail"
            }),
            
            slope = map_dbl(model, ~ {
                  if (!is.null(.x)) coef(.x)["year_scaled"] else NA_real_
            }),
            
            std_error = map_dbl(model, ~ {
                  if (inherits(.x, "lm")) {
                        summary(.x)$coefficients["year_scaled", "Std. Error"]
                  } else if (inherits(.x, "gls")) {
                        summary(.x)$tTable["year_scaled", "Std.Error"]
                  } else {
                        NA_real_
                  }
            }),
            
            p_value = map_dbl(model, ~ {
                  if (inherits(.x, "lm")) {
                        summary(.x)$coefficients["year_scaled", "Pr(>|t|)"]
                  } else if (inherits(.x, "gls")) {
                        summary(.x)$tTable["year_scaled", "p-value"]
                  } else {
                        NA_real_
                  }
            }),
            
            conf_low = slope - 1.96 * std_error,
            conf_high = slope + 1.96 * std_error,
            
            significant = case_when(
                  is.na(p_value) ~ NA_character_,
                  p_value < 0.05 & slope > 0 ~ "Increase",
                  p_value < 0.05 & slope < 0 ~ "Decline",
                  p_value < 0.1 ~ "Marginal",
                  TRUE ~ "Not significant"
            )
      ) |> 
      select(bay, zone, variable, n_years, slope, std_error, p_value, conf_low, conf_high, model_type, significant)

# visualize trends --------------------------------------------------------

# Order variables (metrics) for clarity if needed
metric_order <- c("biomass_m2", "abund_m2", "tl_max", "k", "gen_time", "troph", "species_richness", "species_evenness")

pretty_names <- c(
      biomass_m2 = "Biomass",
      abund_m2 = "Abundance",
      tl_max = "Max Length",
      k = "Growth Rate (k)",
      gen_time = "Generation Time",
      troph = "Trophic Level",
      species_richness = "Richness",
      species_evenness = "Evenness"
)

trend_plot_df <- trend_results_all |>
      mutate(
            variable = factor(variable, levels = metric_order),
            variable_pretty = pretty_names[as.character(variable)],
            measure = case_when(
                  variable_pretty == "Biomass" ~ 'Biomass/Abundance',
                  variable_pretty == "Abundance" ~ 'Biomass/Abundance',
                  variable_pretty == "Max Length" ~ 'Trait',
                  variable_pretty == "Growth Rate (k)" ~ 'Trait',
                  variable_pretty == "Generation Time" ~ 'Trait',
                  variable_pretty == "Trophic Level" ~ 'Trait',
                  variable_pretty == "Richness" ~ 'Diversity',
                  variable_pretty == "Evenness" ~ 'Diversity'
            ),
            bay = factor(bay, levels = est_abb),
            estuary_metric = paste(bay, variable_pretty, sep = " - ")
      ) |>
      filter(!is.na(bay), !is.na(variable_pretty)) |> 
      mutate(significance = case_when(
            significant == "Not significant" ~ "None",
            significant == "Marginal" ~ "Marginal",
            significant == "Increase" ~ "Significant",
            significant == "Decline" ~ "Significant",
      ))

a <- trend_plot_df |> 
      filter(variable_pretty == 'Biomass') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(
            aes(fill = significance),
            shape = 21, color = "black", size = 2, width = 0.25, alpha = 1
      ) + 
      scale_fill_manual(values = c(estuary_palette_abb, sig_palette)) +
      scale_color_manual(values = sig_palette) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary / Significance',
            color = 'Significance',
            title = NULL
      ) +
      scale_y_continuous(
            breaks = c(-5.0, -2.5, 0, 2.5, 5.0), 
            limits = c(-6, 6)
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
a

b <- trend_plot_df |> 
      filter(variable_pretty == 'Abundance') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(
            aes(fill = significance),
            shape = 21, color = "black", size = 2, width = 0.25, alpha = 1
      ) + 
      scale_fill_manual(values = c(estuary_palette_abb, sig_palette)) + 
      scale_color_manual(values = sig_palette) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary / Significance',
            color = 'Significance',
            title = NULL
      ) +
      scale_y_continuous(
            breaks = c(-5.0, -2.5, 0, 2.5, 5.0), 
            limits = c(-6, 6)
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
b

c <- trend_plot_df |> 
      filter(variable_pretty == 'Richness') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(
            aes(fill = significance),
            shape = 21, color = "black", size = 2, width = 0.25, alpha = 1
      ) + 
      scale_fill_manual(values = c(estuary_palette_abb, sig_palette)) + 
      scale_color_manual(values = sig_palette) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary / Significance',
            color = 'Significance',
            title = NULL
      ) +
      scale_y_continuous(
            breaks = c(-5.0, -2.5, 0, 2.5, 5.0), 
            limits = c(-6, 6)
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
c

d <- trend_plot_df |> 
      filter(variable_pretty == 'Evenness') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(
            aes(fill = significance),
            shape = 21, color = "black", size = 2, width = 0.25, alpha = 1
      ) + 
      scale_fill_manual(values = c(estuary_palette_abb, sig_palette)) +
      scale_color_manual(values = sig_palette) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary / Significance',
            color = 'Significance',
            title = NULL
      ) +
      scale_y_continuous(
            breaks = c(-5.0, -2.5, 0, 2.5, 5.0), 
            limits = c(-6, 6)
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
d

e <- trend_plot_df |> 
      filter(variable_pretty == 'Max Length') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +  
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(
            aes(fill = significance),
            shape = 21, color = "black", size = 2, width = 0.25, alpha = 1
      ) + 
      scale_fill_manual(values = c(estuary_palette_abb, sig_palette)) +  # combine palettes
      scale_color_manual(values = sig_palette) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary / Significance',
            color = 'Significance',
            title = NULL
      ) +
      scale_y_continuous(
            breaks = c(-5.0, -2.5, 0, 2.5, 5.0), 
            limits = c(-6, 6)
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
e

f <- trend_plot_df |> 
      filter(variable_pretty == 'Growth Rate (k)') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(
            aes(fill = significance),
            shape = 21, color = "black", size = 2, width = 0.25, alpha = 1
      ) + 
      scale_fill_manual(values = c(estuary_palette_abb, sig_palette)) +  # combine palettes
      scale_color_manual(values = sig_palette) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary / Significance',
            color = 'Significance',
            title = NULL
      ) +
      scale_y_continuous(
            breaks = c(-5.0, -2.5, 0, 2.5, 5.0), 
            limits = c(-6, 6)
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
f

g <- trend_plot_df |> 
      filter(variable_pretty == 'Generation Time') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +  
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(
            aes(fill = significance),
            shape = 21, color = "black", size = 2, width = 0.25, alpha = 1
      ) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_fill_manual(values = c(estuary_palette_abb, sig_palette)) + 
      scale_color_manual(values = sig_palette) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary / Significance',
            color = 'Significance',
            title = NULL
      ) +
      scale_y_continuous(
            breaks = c(-5.0, -2.5, 0, 2.5, 5.0), 
            limits = c(-6, 6)
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
g

h <- trend_plot_df |> 
      filter(variable_pretty == 'Trophic Level') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +  
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(
            aes(fill = significance),
            shape = 21, color = "black", size = 2, width = 0.25, alpha = 1
      ) + 
      scale_fill_manual(values = c(estuary_palette_abb, sig_palette)) +
      scale_color_manual(values = sig_palette) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary / Significance',
            color = 'Significance',
            title = NULL
      ) +
      scale_y_continuous(
            breaks = c(-5.0, -2.5, 0, 2.5, 5.0), 
            limits = c(-6, 6)
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
h

rm(list = setdiff(ls(), c("a", "b", "c", "d", 'e', 'f', 'g', 'h', 'estuary_palette_abb', 'est_abb', "trend_plot_df", "sig_palette", 'fish', 'fish_icon')))

median_fish <- trend_plot_df |>
      mutate(all = 'all') |> 
      filter(variable_pretty == "Biomass") |>
      group_by(all) |> 
      summarize(
            x = 'Estuary',
            y = median(slope, na.rm = TRUE)
      ) |>
      mutate(image = fish_icon)

a_b <- trend_plot_df |>
      filter(variable_pretty == "Biomass") |>
      ggplot(aes(x = "Estuary", y = slope)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      stat_summary(fun.data = function(x) {
            data.frame(
                  ymin = quantile(x, 0.25, na.rm = TRUE),
                  ymax = quantile(x, 0.75, na.rm = TRUE),
                  y = median(x, na.rm = TRUE) 
            )
      }, geom = "errorbar", width = 0.25, linewidth = 1.5, color = "black") +
      # geom_image(data = median_fish, aes(x = x, y = y, image = image), size = 0.40) +
      stat_summary(
            fun = median,
            geom = "point",
            shape = 21, size = 3.5,
            fill = "white", color = "black"
      ) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1)) +
      labs(
            x = NULL,
            y = NULL,
            title = NULL
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
a_b

median_fish <- trend_plot_df |>
      mutate(all = 'all') |> 
      filter(variable_pretty == "Abundance") |>
      group_by(all) |> 
      summarize(
            x = 'Estuary',
            y = median(slope, na.rm = TRUE)
      ) |>
      mutate(image = fish_icon)

b_b <- trend_plot_df |>
      filter(variable_pretty == "Abundance") |>
      ggplot(aes(x = "Estuary", y = slope)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      stat_summary(fun.data = function(x) {
            data.frame(
                  ymin = quantile(x, 0.25, na.rm = TRUE),
                  ymax = quantile(x, 0.75, na.rm = TRUE),
                  y = median(x, na.rm = TRUE)
            )
      }, geom = "errorbar", width = 0.25, linewidth = 1.5, color = "black") +
      # geom_image(data = median_fish, aes(x = x, y = y, image = image), size = 0.40) +
      stat_summary(
            fun = median,
            geom = "point",
            shape = 21, size = 3.5,
            fill = "white", color = "black"
      ) +
      scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1)) +
      labs(
            x = NULL,
            y = NULL,
            title = NULL
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
b_b

median_fish <- trend_plot_df |>
      mutate(all = 'all') |> 
      filter(variable_pretty == "Richness") |>
      group_by(all) |> 
      summarize(
            x = 'Estuary',
            y = median(slope, na.rm = TRUE)
      ) |>
      mutate(image = fish_icon)

c_b <- trend_plot_df |>
      filter(variable_pretty == "Richness") |>
      ggplot(aes(x = "Estuary", y = slope)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      stat_summary(fun.data = function(x) {
            data.frame(
                  ymin = quantile(x, 0.25, na.rm = TRUE),
                  ymax = quantile(x, 0.75, na.rm = TRUE),
                  y = median(x, na.rm = TRUE)  
            )
      }, geom = "errorbar", width = 0.25, linewidth = 1.5, color = "black") +
      # geom_image(data = median_fish, aes(x = x, y = y, image = image), size = 0.40) +
      stat_summary(
            fun = median,
            geom = "point",
            shape = 21, size = 3.5,
            fill = "white", color = "black"
      ) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1)) +
      labs(
            x = NULL,
            y = NULL,
            title = NULL
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
c_b

median_fish <- trend_plot_df |>
      mutate(all = 'all') |> 
      filter(variable_pretty == "Evenness") |>
      group_by(all) |> 
      summarize(
            x = 'Estuary',
            y = median(slope, na.rm = TRUE)
      ) |>
      mutate(image = fish_icon)

d_b <- trend_plot_df |>
      filter(variable_pretty == "Evenness") |>
      ggplot(aes(x = "Estuary", y = slope)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      stat_summary(fun.data = function(x) {
            data.frame(
                  ymin = quantile(x, 0.25, na.rm = TRUE),
                  ymax = quantile(x, 0.75, na.rm = TRUE),
                  y = median(x, na.rm = TRUE) 
            )
      }, geom = "errorbar", width = 0.25, linewidth = 1.5, color = "black") +
      # geom_image(data = median_fish, aes(x = x, y = y, image = image), size = 0.40) +
      stat_summary(
            fun = median,
            geom = "point",
            shape = 21, size = 3.5,
            fill = "white", color = "black"
      ) +
      scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1)) +
      labs(
            x = NULL,
            y = NULL,
            title = NULL
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
d_b

median_fish <- trend_plot_df |>
      mutate(all = 'all') |> 
      filter(variable_pretty == "Max Length") |>
      group_by(all) |> 
      summarize(
            x = 'Estuary',
            y = median(slope, na.rm = TRUE)
      ) |>
      mutate(image = fish_icon)

e_b <- trend_plot_df |>
      filter(variable_pretty == "Max Length") |>
      ggplot(aes(x = "Estuary", y = slope)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      stat_summary(fun.data = function(x) {
            data.frame(
                  ymin = quantile(x, 0.25, na.rm = TRUE),
                  ymax = quantile(x, 0.75, na.rm = TRUE),
                  y = median(x, na.rm = TRUE)
            )
      }, geom = "errorbar", width = 0.25, linewidth = 1.5, color = "black") +
      # geom_image(data = median_fish, aes(x = x, y = y, image = image), size = 0.40) +
      stat_summary(
            fun = median,
            geom = "point",
            shape = 21, size = 3.5,
            fill = "white", color = "black"
      ) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1)) +
      labs(
            x = NULL,
            y = NULL,
            title = NULL
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
e_b

median_fish <- trend_plot_df |>
      mutate(all = 'all') |> 
      filter(variable_pretty == "Growth Rate (k)") |>
      group_by(all) |> 
      summarize(
            x = 'Estuary',
            y = median(slope, na.rm = TRUE)
      ) |>
      mutate(image = fish_icon)

f_b <- trend_plot_df |>
      filter(variable_pretty == "Growth Rate (k)") |>
      ggplot(aes(x = "Estuary", y = slope)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      stat_summary(fun.data = function(x) {
            data.frame(
                  ymin = quantile(x, 0.25, na.rm = TRUE),
                  ymax = quantile(x, 0.75, na.rm = TRUE),
                  y = median(x, na.rm = TRUE)
            )
      }, geom = "errorbar", width = 0.25, linewidth = 1.5, color = "black") +
      # geom_image(data = median_fish, aes(x = x, y = y, image = image), size = 0.40) +
      stat_summary(
            fun = median,
            geom = "point",
            shape = 21, size = 3.5,
            fill = "white", color = "black"
      ) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1)) +
      labs(
            x = NULL,
            y = NULL,
            title = NULL
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
f_b

median_fish <- trend_plot_df |>
      mutate(all = 'all') |> 
      filter(variable_pretty == "Generation Time") |>
      group_by(all) |> 
      summarize(
            x = 'Estuary',
            y = median(slope, na.rm = TRUE)
      ) |>
      mutate(image = fish_icon)

g_b <- trend_plot_df |>
      filter(variable_pretty == "Generation Time") |>
      ggplot(aes(x = "Estuary", y = slope)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      stat_summary(fun.data = function(x) {
            data.frame(
                  ymin = quantile(x, 0.25, na.rm = TRUE),
                  ymax = quantile(x, 0.75, na.rm = TRUE),
                  y = median(x, na.rm = TRUE)
            )
      }, geom = "errorbar", width = 0.25, linewidth = 1.5, color = "black") +
      # geom_image(data = median_fish, aes(x = x, y = y, image = image), size = 0.40) +
      stat_summary(
            fun = median,
            geom = "point",
            shape = 21, size = 3.5,
            fill = "white", color = "black"
      ) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1)) +
      labs(
            x = NULL,
            y = NULL,
            title = NULL
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
g_b

median_fish <- trend_plot_df |>
      mutate(all = 'all') |>
      filter(variable_pretty == "Trophic Level") |>
      group_by(all) |>
      summarize(
            x = 'Estuary',
            y = median(slope, na.rm = TRUE)
      ) |>
      mutate(image = fish_icon)

h_b <- trend_plot_df |>
      filter(variable_pretty == "Trophic Level") |>
      ggplot(aes(x = "Estuary", y = slope)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      stat_summary(fun.data = function(x) {
            data.frame(
                  ymin = quantile(x, 0.25, na.rm = TRUE),
                  ymax = quantile(x, 0.75, na.rm = TRUE),
                  y = median(x, na.rm = TRUE)
            )
      }, geom = "errorbar", width = 0.25, linewidth = 1.5, color = "black") +
      # geom_image(data = median_fish, aes(x = x, y = y, image = image), size = 0.40) +
      stat_summary(
            fun = median,
            geom = "point",
            shape = 21, size = 3.5,
            fill = "white", color = "black"
      ) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), limits = c(-1,1)) +
      labs(
            x = NULL,
            y = NULL,
            title = NULL
      ) +
      theme_classic() +
      theme(
            axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none", 
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14)
      )
h_b

rm(list = setdiff(ls(), c("a", "b", "c", "d", 'e', 'f', 'g', 'h',
                          "a_b", "b_b", "c_b", "d_b", 'e_b', 'f_b', 'g_b', 'h_b',
                          'estuary_palette_abb', 'est_abb', "est", "sig_palette", "trend_plot_df",
                          'fish', 'fish_icon')))

a <- a + ggtitle('Biomass') + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", size = 13), plot.tag.position = c(0.02, 1), plot.tag = element_text(size = 14, face = "bold")) + labs(tag = 'a')
b <- b + ggtitle('Abundance') + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", size = 13))
c <- c + ggtitle('Species Richness') + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", size = 13))
d <- d + ggtitle('Species Evenness') + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 13))
e <- e + ggtitle('Max Length') + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", size = 13))
f <- f + ggtitle('Growth Rate') + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", size = 13))
g <- g + ggtitle('Generation Time') + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", size = 13),, plot.tag.position = c(0.02, 1), plot.tag = element_text(size = 14, face = "bold")) + labs(tag = 'a')
h <- h + ggtitle('Trophic Level') + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 13))

a_b <- a_b + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), plot.tag.position = c(0.02, 1), plot.tag = element_text(size = 14, face = "bold")) + labs(tag = 'b')
b_b <- b_b + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
c_b <- c_b + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
# d_b <- d_b + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
e_b <- e_b + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
f_b <- f_b + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
g_b <- g_b + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),, plot.tag.position = c(0.02, 1), plot.tag = element_text(size = 14, face = "bold")) + labs(tag = 'b')
# h_b <- h_b + theme(axis.text.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())

### diversity and abundance ----
r1 <- a + a_b + plot_layout(nrow = 1, widths = c(1,0.25))
r1

r2 <- b + b_b + plot_layout(nrow = 1, widths = c(1,0.25))
r2

r3 <- c + c_b + plot_layout(nrow = 1, widths = c(1,0.25))
r3

r4 <- d + d_b + plot_layout(nrow = 1, widths = c(1,0.25))
r4

final_plot <- (r1/r2/r3/r4)
final_plot

grid::grid.newpage()
grid::grid.draw(
      patchwork::wrap_elements(full = final_plot) +
            ### inset shared y axis label 
            patchwork::inset_element(
                  grid::textGrob('Temporal Trend', rot = 90, gp = grid::gpar(fontsize = 16, fontface = 'bold')),
                  left = 0.0, bottom = 0.25, right = 0.019, top = 0.75, align_to = 'full')
)

ggsave('figs/bm-abund-diversity-temporal-trends.png',
       dpi = 600, units= 'in', height = 7, width = 9)

### traits ----
r1 <- e + e_b + plot_layout(nrow = 1, widths = c(1,0.25))
r1

r2 <- f + f_b + plot_layout(nrow = 1, widths = c(1,0.25))
r2

r3 <- g + g_b + plot_layout(nrow = 1, widths = c(1,0.25))
r3

r4 <- h + h_b + plot_layout(nrow = 1, widths = c(1,0.25))
r4

final_plot <- (r3/r2/r1/r4)
final_plot

grid::grid.newpage()
grid::grid.draw(
      patchwork::wrap_elements(full = final_plot) +
            ### inset shared y axis label 
            patchwork::inset_element(
                  grid::textGrob('Temporal Trend', rot = 90, gp = grid::gpar(fontsize = 16, fontface = 'bold')),
                  left = 0.0, bottom = 0.25, right = 0.019, top = 0.75, align_to = 'full')
)

ggsave('figs/trait-temporal-trends.png',
       dpi = 600, units= 'in', height = 7, width = 9)

coords <- read_rds('local-data/key-datasets/forage_fish_master.RDS') |> 
      select(bay, zone, latitude, longitude) |> 
      distinct() |> 
      group_by(bay, zone) |> 
      summarize(lat = mean(latitude, na.rm = TRUE),
                long = mean(longitude, na.rm = TRUE),
                .groups = "drop") |> 
      mutate(bay = case_when(
            bay == 'IR' ~ 'NIR',
            bay == 'TQ' ~ 'SIR',
            TRUE ~ bay
      ))

lat_grad <- trend_plot_df |> left_join(coords)
glimpse(lat_grad)
lat_grad_bm <- lat_grad |> filter(variable_pretty == "Generation Time")
glimpse(lat_grad_bm)

m3 <- lm(slope ~ lat, data = lat_grad_bm)
summary(m3)
lat_grad_bm |> 
      ggplot(aes(x=lat,y=slope)) +
      geom_vline(xintercept = 28, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "solid") +
      geom_point(aes(color = bay), size = 3) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      annotate("text", x = 28.85, y = 3.4, label = "~biogegraphic transition zone",
               size = 2.7, fontface = "bold") +
      annotate("text", x = 28.9, y = 3.15, label = bquote(italic(Gilmore~1995) * ", " * italic(Troast~et~al.~2020)),
               size = 2.7, fontface = "bold") +
      annotate('text', 
               x = 30.3, y = 3.5,
               label = bquote({R^2} == 0.23),
               size = 4, fontface = 'bold') +
      annotate('text', 
               x = 30.3, y = 3.05,
               label = bquote(italic(p) <0.001),
               size = 4, fontface = 'bold') +
      scale_color_manual(values = estuary_palette_abb) +
      labs(x = 'Latitude', y = 'Temporal Trend', color = "Estuary", title = "Generation Time") +
      theme(axis.text = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_text(size = 12, face = "bold", colour = "black"),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = 'none',       
            legend.text = element_text(face = 'bold', size = 6),
            legend.title = element_text(face = 'bold'), size = 6,
            plot.margin = margin(5.5, 5.5, 5.5, 5.5))

ggsave('figs/generationtime-latitude-gradient-regression.png',
       dpi = 600, units= 'in', height = 5, width = 5)

lat_grad <- trend_plot_df |> left_join(coords)
glimpse(lat_grad)
lat_grad_bm <- lat_grad |> filter(variable_pretty == "Biomass")
glimpse(lat_grad_bm)

m3 <- lm(slope ~ lat, data = lat_grad_bm)
summary(m3)
lat_grad_bm |> 
      ggplot(aes(x=lat,y=slope)) +
      geom_vline(xintercept = 28, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "solid") +
      geom_point(aes(color = bay), size = 3) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      annotate("text", x = 28.6, y = 4.3, label = "~biogegraphic transition zone",
               size = 2.2, fontface = "bold") +
      annotate("text", x = 28.6, y = 4.0, label = bquote(italic(Gilmore~1995) * ", " * italic(Troast~et~al.~2020)),
               size = 2.0, fontface = "bold") +
      annotate('text', 
               x = 30.3, y = 4.4,
               label = bquote({R^2} == 0.19),
               size = 4, fontface = 'bold') +
      annotate('text', 
               x = 30.35, y = 3.85,
               label = bquote(italic(p) <0.01),
               size = 4, fontface = 'bold') +
      scale_color_manual(values = estuary_palette_abb) +
      labs(x = 'Latitude', y = 'Temporal Trend', color = "Estuary", title = "Biomass") +
      theme(axis.text = element_text(size = 10, face = "bold", colour = "black"),
            axis.title = element_text(size = 12, face = "bold", colour = "black"),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = 'none',       
            legend.text = element_text(face = 'bold', size = 6),
            legend.title = element_text(face = 'bold'), size = 6)

ggsave('figs/biomass-latitude-gradient-regression.png',
       dpi = 600, units= 'in', height = 4, width = 6)
