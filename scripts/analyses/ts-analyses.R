###project: Forage Fish Resilience
###author(s): MW
###goal(s): resistance and resilience calculations
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, FSA, readr, forcats, multcompView, readxl, dplyr, broom, ggpubr,
                 patchwork, splitstackshape, purrr, zoo, pracma, vegan, e1071, codyn, lubridate, forecast)

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

est <- c('Apalachicola Bay', 'Cedar Key', 'Tampa Bay', 'Charlotte Harbor',
         'Southern Indian River', 'Northern Indian River', 'Northeast Florida')

est_abb <- c('AP', 'CK', 'TB', 'CH', 'SIR', 'NIR', 'JX')

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
            bay = factor(bay),
            estuary_metric = paste(bay, variable_pretty, sep = " - ")
      ) |>
      filter(!is.na(bay), !is.na(variable_pretty))

a1 <- trend_plot_df |> 
      filter(variable_pretty == 'Biomass') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(aes(fill = bay), shape = 21, color = "black", size = 1.5, width = 0.25, alpha = 0.9) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary',
            title = "Biomass"
      ) +
      # scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0,7.5), 
      #                    limits = c(-6, 8)) +
      scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0), 
                         limits = c(-6, 6)) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 60, hjust = 1),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
a1

a2 <- trend_plot_df |> 
      filter(variable_pretty == 'Abundance') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(aes(fill = bay), shape = 21, color = "black", size = 1.5, width = 0.25, alpha = 0.9) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary',
            title = "Abundance"
      ) +
      # scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0,7.5), 
      #                    limits = c(-6, 8)) +
      scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0), 
                         limits = c(-6, 6)) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 60, hjust = 1),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
a2

b <- trend_plot_df |> 
      filter(variable_pretty == 'Richness') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(aes(fill = bay), shape = 21, color = "black", size = 1.5, width = 0.25, alpha = 0.9) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary',
            title = "Species Richness"
      ) +
      # scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0,7.5), 
      #                    limits = c(-6, 8)) +
      scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0), 
                         limits = c(-6, 6)) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 60, hjust = 1),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
b

c <- trend_plot_df |> 
      filter(variable_pretty == 'Evenness') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(aes(fill = bay), shape = 21, color = "black", size = 1.5, width = 0.25, alpha = 0.9) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary',
            title = "Species Evenness"
      ) +
      # scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0,7.5), 
      #                    limits = c(-6, 8)) +
      scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0), 
                         limits = c(-6, 6)) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 60, hjust = 1),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
c

e <- trend_plot_df |> 
      filter(variable_pretty == 'Max Length') |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(aes(fill = bay), shape = 21, color = "black", size = 1.5, width = 0.25, alpha = 0.9) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary',
            title = "Max Size"
      ) +
      # scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0,7.5), 
      #                    limits = c(-6, 8)) +
      scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0), 
                         limits = c(-6, 6)) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 60, hjust = 1),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
e

f <- trend_plot_df |> 
      filter(variable_pretty == "Growth Rate (k)") |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(aes(fill = bay), shape = 21, color = "black", size = 1.5, width = 0.25, alpha = 0.9) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary',
            title = "Growth Rate (k)"
      ) +
      # scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0,7.5), 
      #                    limits = c(-6, 8)) +
      scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0), 
                         limits = c(-6, 6)) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 60, hjust = 1),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
f

g <- trend_plot_df |> 
      filter(variable_pretty == "Generation Time") |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(aes(fill = bay), shape = 21, color = "black", size = 1.5, width = 0.25, alpha = 0.9) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary',
            title = "Generation Time"
      ) +
      # scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0,7.5), 
      #                    limits = c(-6, 8)) +
      scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0), 
                         limits = c(-6, 6)) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 60, hjust = 1),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
g

h <- trend_plot_df |> 
      filter(variable_pretty == "Trophic Level") |> 
      ggplot(aes(x = bay, y = slope, fill = bay)) +
      geom_boxplot(
            width = 0.5,
            outlier.shape = NA,
            color = "black"
      ) +
      geom_jitter(aes(fill = bay), shape = 21, color = "black", size = 1.5, width = 0.25, alpha = 0.9) + 
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(
            y = NULL,
            x = NULL,
            fill = 'Estuary',
            title = "Trophic Level"
      ) +
      # scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0,7.5), 
      #                    limits = c(-6, 8)) +
      scale_y_continuous(breaks = c(-5.0,-2.5,0,2.5,5.0), 
                         limits = c(-6, 6)) +
      theme_classic() +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 60, hjust = 1),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            axis.title.x = element_text(face = "bold", color = "black", size = 14),
            axis.title.y = element_text(face = "bold", color = "black", size = 14),
            plot.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5),
            strip.text = element_text(face = "bold", color = "black", size = 12),
            legend.position = "none",
            legend.background = element_blank(),
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 14))
h

rm(list = setdiff(ls(), c("a1", "a2", "b", "c", 'e', 'f', 'g', 'h', 'estuary_palette_abb', 'est_abb', "trend_plot_df")))

a1 <- a1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
a2 <- a2 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
b <- b + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
e <- e + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
f <- f + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
g <- g + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())

bio_abund_plot <- (a1/a2/b/c) +
      # plot_annotation(tag_levels = 'a') +
      theme(legend.position = 'none')
bio_abund_plot

trait_plot <- (e/f/g/h) +
      # plot_annotation(tag_levels = 'a') +
      theme(legend.position = 'none')
trait_plot
