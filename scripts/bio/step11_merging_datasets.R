###project: Forage Fish Resilience
###author(s): MW
###goal(s): merge final datasets
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, FSA, readr, forcats, multcompView, readxl, dplyr, strucchange, mgcv,
                 splitstackshape, purrr, zoo, pracma, vegan, e1071, codyn, lubridate, forecast)

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
stability <- read_csv("local-data/stability-filtered-may2025.csv")
dynamics <- read_csv('local-data/forage-fish-dynamics.csv')

### stability ----
glimpse(stability)
glimpse(dynamics)

stability_final <- stability |> 
      left_join(dynamics)

### read out final datasets ---
# write_csv(stability_final, "local-data/key-datasets/stability_foragefish_final.csv")

glimpse(stability_final)

anova_result <- aov(biomass_stability ~ bay, data = stability_final)
summary(anova_result)
plot(anova_result, 2)  # Q-Q plot
shapiro.test(residuals(anova_result))
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
tukey_letters <- multcompLetters4(anova_result, tukey_result)
print(tukey_letters)
letter_df <- as.data.frame.list(tukey_letters$bay)
letter_df$bay <- rownames(letter_df)
colnames(letter_df)[1] <- "letters"
letter_df <- letter_df |> 
      mutate(bay = case_when(
            bay == 'TQ' ~ 'SIR',
            bay == "IR" ~ 'NIR',
            TRUE ~ bay
      ))

anova_tbl <- summary(anova_result)[[1]]

F_val <- round(anova_tbl["bay", "F value"], 2)
p_val <- anova_tbl["bay", "Pr(>F)"]
df1 <- anova_tbl["bay", "Df"]
df2 <- anova_tbl["Residuals", "Df"]
p_val_text <- ifelse(p_val < 0.001, "< 0.001", paste0("= ", round(p_val, 3)))
annot_text <- paste0("ANOVA: F(", df1, ", ", df2, ") = ", F_val, ", p ", p_val_text)

c <- stability_final |>
      mutate(bay = case_when(
            bay == 'TQ' ~ 'SIR',
            bay == "IR" ~ 'NIR',
            TRUE ~ bay
      )) |> 
      left_join(letter_df) |> 
      mutate(bay = factor(bay, level = est_abb)) |> 
      ggplot(aes(x = bay, y = biomass_stability)) +
      geom_boxplot(aes(fill = bay), alpha = 1, outliers = FALSE) +
      geom_jitter(aes(fill = bay), color = 'black', alpha = 0.2, size = 2) +
      geom_smooth(method = 'lm', se = TRUE) +
      scale_color_manual(values = estuary_palette_abb) +
      scale_fill_manual(values = estuary_palette_abb) +
      geom_text(data = letter_df, aes(x = bay, y = 0.82, label = letters), fontface = 'bold',
                color = 'black',inherit.aes = FALSE, size = 5) +
      labs(y = 'Biomass Stability', title = 'One-way ANOVA', x = NULL) +
      annotate("text", x = 4.9, y = 0.87, label = annot_text, hjust = 0, size = 4, fontface = 'bold') +
      theme_classic()+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 12, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 12),
            axis.title.y = element_text(face = "bold", color = "black", size = 12),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 12))
c
m <- lm(biomass_stability ~ lat, data = stability_final)

a <- stability_final |>
      mutate(bay = case_when(
            bay == 'TQ' ~ 'SIR',
            bay == "IR" ~ 'NIR',
            TRUE ~ bay
      )) |> 
      mutate(bay = factor(bay, level = est_abb)) |> 
      ggplot(aes(x = lat, y = biomass_stability)) +
      geom_jitter(aes(color = bay), alpha = 1, size = 2) +
      geom_smooth(method = 'lm', color = 'black', se = TRUE) +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(y = 'Biomass Stability', title = 'Linear Regression', x = 'Latitude') +
      theme_classic() +
      annotate('text',
               x = 26.85, y = 0.8,
               label = bquote({R^2} == 0.1),
               size = 4.5) +
      annotate('text',
               x = 27, y = 0.75,
               label = bquote(italic(p) < 2 %*% 10^-16),
               size = 4.5) +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 12, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 12),
            axis.title.y = element_text(face = "bold", color = "black", size = 12),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 12))
a
bp <- breakpoints(biomass_stability ~ lat, data = stability_final, breaks = 1)
summary(bp)
bp_lat <- stability_final$lat[bp$breakpoints]
print(bp_lat)
plot(bp)
seg_model <- lm(biomass_stability ~ lat * (lat <= bp_lat) + lat * (lat > bp_lat), 
                data = stability_final)
summary(seg_model)

stability_final <- stability_final |>
      mutate(lat_group = factor(ifelse(lat <= bp_lat, "South", "North")))

seg_model2 <- lm(biomass_stability ~ lat * lat_group, data = stability_final)
summary(seg_model2)

seg_summary <- summary(seg_model2)
r2 <- round(seg_summary$r.squared, 2)

# Extract p-values from coefficients
coefs <- summary(seg_model2)$coefficients
pval_north <- coefs["lat", "Pr(>|t|)"]
pval_south <- coefs["lat:lat_groupSouth", "Pr(>|t|)"]

# Convert p-values to expression-friendly format
pval_south_txt <- ifelse(pval_south < 0.001, "italic(p) < 0.001", paste0("italic(p) == ", signif(pval_south, 2)))
pval_north_txt <- ifelse(pval_north < 0.001, "italic(p) < 0.001", paste0("italic(p) == ", signif(pval_north, 2)))

# Plot with annotation
b <- stability_final |> 
      mutate(bay = case_when(
            bay == 'TQ' ~ 'SIR',
            bay == "IR" ~ 'NIR',
            TRUE ~ bay
      )) |> 
      ggplot(aes(x = lat, y = biomass_stability)) +
      geom_jitter(aes(color = bay), alpha = 1, size = 2) +
      geom_smooth(method = "lm", aes(group = lat_group), se = TRUE, color = "black") +
      geom_vline(xintercept = bp_lat, linetype = "dashed") +
      scale_fill_manual(values = estuary_palette_abb) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(y = 'Biomass Stability', x = 'Latitude', title = 'Segmented Regression') +
      theme_classic() +
      annotate('text',
               x = 26.85, y = 0.8,
               label = bquote({R^2} == 0.1),
               size = 4.5) +
      annotate('text',
               x = 27, y = 0.07,
               label = bquote(italic(p) < 2 %*% 10^-16),
               size = 4.0) +
      annotate('text',
               x = 28.99, y = 0.07,
               label = bquote(italic(p) < 2 %*% 10^-16),
               size = 4.0) +
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 12),
            axis.text.y = element_text(face = "bold", color = "black", size = 12),
            plot.title = element_text(face = "bold", color = "black", size = 12, hjust = 0.5),
            axis.title.x = element_text(face = "bold", color = "black", size = 12),
            axis.title.y = element_text(face = "bold", color = "black", size = 12),
            strip.text = element_blank(),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(face = "bold", color = "black", size = 12),
            legend.title = element_text(face = "bold", color = "black", size = 12))
b
rm(list = setdiff(ls(), c("a", "b", "c", 'estuary_palette', 'est_abb', 'estuary_palette_abb')))
a
b
c

a <- a + labs(y = NULL) + theme(plot.tag = element_text(size = 12, face = "bold"),
                                plot.tag.position = c(0.02, 1),
                                plot.margin = margin(5.5, 5.5, 5.5, 15))
b <- b + labs(y = NULL) + theme(plot.tag = element_text(size = 12, face = "bold"),
                                plot.tag.position = c(0.02, 1),
                                plot.margin = margin(5.5, 5.5, 5.5, 15))
c <- c + labs(y = NULL) + theme(plot.tag = element_text(size = 12, face = "bold"),
                                plot.tag.position = c(0.02, 1),
                                plot.margin = margin(5.5, 5.5, 5.5, 15))

top <- a + b + plot_layout(nrow = 1, guides = "collect") & theme(legend.position = "none")
top

final_plot <- top/c + plot_layout(heights = c(1, 1)) +
      plot_annotation(tag_levels = 'a')
final_plot

grid::grid.newpage()
grid::grid.draw(
      patchwork::wrap_elements(full = final_plot) +
            
            # Y-axis label for the top row (Biomass Stability)
            patchwork::inset_element(
                  grid::textGrob("Biomass Stability", rot = 90, gp = gpar(fontsize = 14, fontface = "bold")),
                  left = 0, bottom = 0.25, right = 0.03, top = 0.75, align_to = "full"
            )
)

ggsave('figs/spatial-patters-stability-threepanel.png',
       dpi = 600, units= 'in', height = 8, width = 8.5)
