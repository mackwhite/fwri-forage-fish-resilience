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
librarian::shelf(tidyverse, readr, zoo, MuMIn, corrplot, performance, ggeffects,
                 ggpubr, parameters, ggstats, brms, mixedup, multcompView, FSA, grid, patchwork)

### read in stability dataset ----
dat <- read_csv('local-data/key-datasets/resistance-resilience-calcs.csv') |> 
      filter(recovery_months_inflated != "yes")
glimpse(dat)

dat_long <- dat |> 
      select(event_id, event, bay, resistance, resilience_raw) |> 
      pivot_longer(cols = c(resistance, resilience_raw),
                   names_to = 'metric', values_to = 'value')

# test and visualize for differences across full dataset ------------------

### resistance ----
resist_df <- dat_long |> 
      mutate(metric = case_when(
            metric == 'resistance' ~ 'Resistance',
            metric == 'resilience_raw' ~ 'Resilience',
            TRUE ~ metric)) |> 
      filter(metric == "Resistance") |> 
      mutate(event = as.factor(event))

# check normality assumption
shapiro.test(resist_df$value)
hist(resist_df$value) #looks normal, but p-value says otherwise...

# kruskal-wallis test since violates normality assumption
kruskal.test(value ~ event, data = resist_df) 
# kruskal-wallis output: chi-squared = 267.93, df = 2, p-value < 2.2e-16

# dunns post-hoc test
dunnTest(value ~ event, data = resist_df, method = "bonferroni")
# dunns test output: 
# Comparison                 Z       P.unadj       P.adj
# 1 coldsnap - hurricane -1.593189 1.111178e-01 3.333533e-01
# 2       coldsnap - mhw 15.528413 2.228274e-54 6.684821e-54
# 3      hurricane - mhw  8.398789 4.511071e-17 1.353321e-16

ymax_vals <- resist_df |> 
      mutate(event = case_when(
            event == 'coldsnap' ~ 'Cold Snap',
            event == 'hurricane' ~ 'Hurricane',
            event == 'mhw' ~ 'Heat Wave'
      )) |> 
      group_by(event) |> 
      summarize(ymax = max(value, na.rm = TRUE), .groups = "drop")

letter_df <- data.frame(
      event = c('Cold Snap', 'Heat Wave', 'Hurricane'),
      letter = c('a', 'b', 'a')
) |> 
      left_join(ymax_vals)

### visualize resistance effects ----
a <- resist_df |> 
      mutate(event = case_when(
            event == 'coldsnap' ~ 'Cold Snap',
            event == 'hurricane' ~ 'Hurricane',
            event == 'mhw' ~ 'Heat Wave'
      )) |> 
      ggplot(aes(x = event, y = value, group = event)) + 
      geom_boxplot(aes(fill = event), outlier.shape = NA, alpha = 0.8) +
      geom_jitter(color = "black", alpha = 0.3, size = 1) +
      theme_minimal(base_size = 14) +
      theme_bw() +
      labs(x = NULL, y = NULL, fill = "Event", title = "Resistance") + 
      geom_text(data = letter_df, aes(x = event, y = ymax + 0.3, label = letter),
                inherit.aes = FALSE, fontface = "bold", size = 4) +
      scale_fill_manual(values = disturbance_palette) +
      scale_y_continuous(breaks = c(-4,-2,0,2,4,6), limits = c(-4,6.5)) +
      theme(
            strip.text = element_text(size = 16, face = "bold", colour = "black"),
            strip.background = element_blank(),  
            # axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "none",
            legend.text = element_text(face = 'bold'),
            legend.title = element_text(face = 'bold')
      )
a      

### resilience ----
resil_df <- dat_long |> 
      mutate(metric = case_when(
            metric == 'resistance' ~ 'Resistance',
            metric == 'resilience_raw' ~ 'Resilience',
            TRUE ~ metric)) |> 
      filter(metric == "Resilience") |> 
      mutate(event = as.factor(event))

# check normality assumption
shapiro.test(resil_df$value)
hist(resil_df$value) #does not look normal..

# kruskal-wallis test since violates normality assumption
kruskal.test(value ~ event, data = resil_df) 
# kruskal-wallis output: chi-squared = 8.7996, df = 2, p-value = 0.01228

# dunns post-hoc test
dunnTest(value ~ event, data = resil_df, method = "bonferroni")
# dunns test output: 
# Comparison                 Z       P.unadj       P.adj
# 1 coldsnap - hurricane  2.9655350 0.003021569 0.009064708
# 2       coldsnap - mhw  0.7299766 0.465404464 1.000000000
# 3      hurricane - mhw -2.6505513 0.008036052 0.024108156

ymax_vals <- resil_df |> 
      mutate(event = case_when(
            event == 'coldsnap' ~ 'Cold Snap',
            event == 'hurricane' ~ 'Hurricane',
            event == 'mhw' ~ 'Heat Wave'
      )) |> 
      group_by(event) |> 
      summarize(ymax = max(value, na.rm = TRUE), .groups = "drop")

letter_df <- data.frame(
      event = c('Cold Snap', 'Heat Wave', 'Hurricane'),
      letter = c('a', 'a', 'b')
) |> 
      left_join(ymax_vals)

### visualize resilience effects ----
b <- resil_df |> 
      mutate(event = case_when(
            event == 'coldsnap' ~ 'Cold Snap',
            event == 'hurricane' ~ 'Hurricane',
            event == 'mhw' ~ 'Heat Wave'
      )) |> 
      ggplot(aes(x = event, y = value, group = event)) + 
      geom_boxplot(aes(fill = event), outlier.shape = NA, alpha = 0.8) +
      geom_jitter(color = "black", alpha = 0.3, size = 1) +
      theme_minimal(base_size = 14) +
      theme_bw() +
      labs(x = NULL, y = NULL, fill = "Event", title = "Resilience") + 
      geom_text(data = letter_df, aes(x = event, y = ymax + 0.3, label = letter),
                inherit.aes = FALSE, fontface = "bold", size = 4) +
      scale_fill_manual(values = disturbance_palette) +
      scale_y_continuous(breaks = c(-4,-2,0,2,4,6), limits = c(-4,6.5)) +
      theme(
            strip.text = element_text(size = 16, face = "bold", colour = "black"),
            strip.background = element_blank(),  
            axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "none",
            legend.text = element_text(face = 'bold'),
            legend.title = element_text(face = 'bold')
      )
b    
ggpubr::ggarrange(a,b, align = 'h')

# test and visualize for differences across estuaries ------------------

### resistance ----
resist_df <- dat_long |> 
      mutate(metric = case_when(
            metric == 'resistance' ~ 'Resistance',
            metric == 'resilience_raw' ~ 'Resilience',
            TRUE ~ metric)) |> 
      filter(metric == "Resistance") |> 
      mutate(bay = as.factor(bay))

# check normality assumption
shapiro.test(resist_df$value)
hist(resist_df$value) #looks normal, but p-value says otherwise...

# kruskal-wallis test since violates normality assumption
kruskal.test(value ~ bay, data = resist_df) 
# kruskal-wallis output: chi-squared = 17.647, df = 6, p-value = 0.007178

# dunns post-hoc test
dunnTest(value ~ bay, data = resist_df, method = "bonferroni")

ymax_vals <- resist_df |> 
      mutate(event = case_when(
            event == 'coldsnap' ~ 'Cold Snap',
            event == 'hurricane' ~ 'Hurricane',
            event == 'mhw' ~ 'Heat Wave'
      )) |> 
      group_by(bay) |> 
      summarize(ymax = max(value, na.rm = TRUE), .groups = "drop") |> 
      mutate(bay = case_when(
            bay == 'TQ' ~ 'SIR',
            bay == 'IR' ~ 'NIR',
            TRUE ~ bay
      ))

letter_df <- data.frame(
      bay = c('AP', 'CK', 'TB', 'CH', 'SIR', 'NIR', 'JX'),
      letter = c('a', 'a', 'a', 'a', 'a', 'a', 'a')
) |>  
      left_join(ymax_vals)

### visualize resistance effects ----
c <- resist_df |> 
      mutate(event = case_when(
            event == 'coldsnap' ~ 'Cold Snap',
            event == 'hurricane' ~ 'Hurricane',
            event == 'mhw' ~ 'Heat Wave'
      )) |>
      mutate(bay = case_when(
            bay == 'TQ' ~ 'SIR',
            bay == 'IR' ~ 'NIR',
            TRUE ~ bay
      ),
      bay = factor(bay, levels = est_abb)) |> 
      ggplot(aes(x = bay, y = value, group = bay)) + 
      geom_boxplot(aes(fill = bay), outlier.shape = NA, alpha = 0.8) +
      geom_jitter(aes(color = event), alpha = 0.6, size = 1) +
      theme_minimal(base_size = 14) +
      theme_bw() +
      labs(x = NULL, y = NULL, fill = "Estuary", color = "Disturbance", title = "Resistance") + 
      geom_text(data = letter_df, aes(x = bay, y = ymax + 0.3, label = letter),
                inherit.aes = FALSE, fontface = "bold", size = 4) +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = disturbance_palette) +
      guides(fill = "none", color = guide_legend(override.aes = list(size = 3))) +
      scale_y_continuous(breaks = c(-4,-2,0,2,4,6), limits = c(-4,6.5)) +
      theme(
            strip.text = element_text(size = 16, face = "bold", colour = "black"),
            strip.background = element_blank(),  
            # axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "none",
            legend.text = element_text(size = 12, face = "bold", colour = "black"),
            legend.title = element_text(size = 12, face = "bold", colour = "black")
      )
c

### resilience ----
resil_df <- dat_long |> 
      mutate(metric = case_when(
            metric == 'resistance' ~ 'Resistance',
            metric == 'resilience_raw' ~ 'Resilience',
            TRUE ~ metric)) |> 
      filter(metric == "Resilience") |> 
      mutate(bay = as.factor(bay))

# check normality assumption
shapiro.test(resil_df$value)
hist(resil_df$value) #looks nonormal

# kruskal-wallis test since violates normality assumption
kruskal.test(value ~ bay, data = resil_df) 
# kruskal-wallis output: chi-squared = 75.788, df = 6, p-value = 2.641e-14

# dunns post-hoc test
dunnTest(value ~ bay, data = resil_df, method = "bonferroni")

library(multcompView)

# Manually enter adjusted p-values from your results
pvals <- c(
      "AP-CH" = 0.3864491,
      "AP-CK" = 9.432568e-05,
      "CH-CK" = 0.09292182,
      "AP-IR" = 1.000000e+00,
      "CH-IR" = 1.085005e-03,
      "CK-IR" = 8.420333e-09,
      "AP-JX" = 0.9695415,
      "CH-JX" = 1.000000e+00,
      "CK-JX" = 0.08830368,
      "IR-JX" = 0.01370649,
      "AP-TB" = 1.000000e+00,
      "CH-TB" = 0.05964569,
      "CK-TB" = 1.186096e-06,
      "IR-TB" = 1.000000e+00,
      "JX-TB" = 0.3244010,
      "AP-TQ" = 0.001450844,
      "CH-TQ" = 1.614887e-06,
      "CK-TQ" = 7.182236e-10,
      "IR-TQ" = 0.005025002,
      "JX-TQ" = 5.443745e-06,
      "TB-TQ" = 0.0005275303
)

cld <- multcompLetters(pvals, threshold = 0.05)
cld$Letters

ymax_vals <- resil_df |> 
      mutate(event = case_when(
            event == 'coldsnap' ~ 'Cold Snap',
            event == 'hurricane' ~ 'Hurricane',
            event == 'mhw' ~ 'Heat Wave'
      )) |> 
      group_by(bay) |> 
      summarize(ymax = max(value, na.rm = TRUE), .groups = "drop") |> 
      mutate(bay = case_when(
            bay == 'TQ' ~ 'SIR',
            bay == 'IR' ~ 'NIR',
            TRUE ~ bay
      ))

letter_df <- data.frame(
      bay = c('AP', 'CK', 'TB', 'CH', 'SIR', 'NIR', 'JX'),
      letter = c('a,b', 'c', 'a,b', 'a,c', 'd', 'b', 'a,c')
) |>  
      left_join(ymax_vals)

### visualize resistance effects ----
d <- resil_df |> 
      mutate(event = case_when(
            event == 'coldsnap' ~ 'Cold Snap',
            event == 'hurricane' ~ 'Hurricane',
            event == 'mhw' ~ 'Heat Wave'
      )) |>
      mutate(bay = case_when(
            bay == 'TQ' ~ 'SIR',
            bay == 'IR' ~ 'NIR',
            TRUE ~ bay
      ),
      bay = factor(bay, levels = est_abb)) |> 
      ggplot(aes(x = bay, y = value, group = bay)) + 
      geom_boxplot(aes(fill = bay), outlier.shape = NA, alpha = 0.8) +
      geom_jitter(aes(color = event), alpha = 0.6, size = 1) +
      theme_minimal(base_size = 14) +
      theme_bw() +
      labs(x = NULL, y = NULL, fill = "Estuary", color = "Disturbance", title = "Resilience") + 
      geom_text(data = letter_df, aes(x = bay, y = ymax + 0.3, label = letter),
                inherit.aes = FALSE, fontface = "bold", size = 4) +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = disturbance_palette) +
      guides(fill = "none", color = guide_legend(override.aes = list(size = 3))) +
      scale_y_continuous(breaks = c(-4,-2,0,2,4,6), limits = c(-4,6.5)) +
      theme(
            strip.text = element_text(size = 16, face = "bold", colour = "black"),
            strip.background = element_blank(),  
            # axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "bottom",
            legend.text = element_text(size = 12, face = "bold", colour = "black"),
            legend.title = element_text(size = 12, face = "bold", colour = "black")
      )
d

ggpubr::ggarrange(c,d, align = 'h',nrow = 2)

rm(list = setdiff(ls(), c("a", "b", "c", "d", 'estuary_palette_abb', 'est_abb', 'disturbance_palette')))

### patch this together for final four-panel figure ----

a <- a + theme(plot.tag.position = c(0.02, 1), plot.tag = element_text(size = 14, face = "bold"))
b <- b + theme(plot.tag.position = c(0.02, 1), plot.tag = element_text(size = 14, face = "bold"))
c <- c + theme(plot.tag.position = c(0.02, 1), plot.tag = element_text(size = 14, face = "bold"))
d <- d + theme(plot.tag.position = c(0.02, 1), plot.tag = element_text(size = 14, face = "bold"))

top_row <- a + b + plot_layout(nrow = 1)
top_row



final_plot <- (top_row / c / d) +
      plot_layout(heights = c(1,0.5,0.5)) +
      plot_annotation(tag_levels = 'a') +
      theme(legend.position = 'bottom')
final_plot

grid::grid.newpage()
grid::grid.draw(
      patchwork::wrap_elements(full = final_plot) +
            ### inset shared y axis label 
            patchwork::inset_element(
                  grid::textGrob('Response', rot = 90, gp = grid::gpar(fontsize = 14, fontface = 'bold')),
                  left = 0.0, bottom = 0.25, right = 0.019, top = 0.75, align_to = 'full')
)

# ggsave('figs/resistance-resilience-fourpanel.png',
#        dpi = 600, units= 'in', height = 10, width = 10)

