# Script Details ----------------------------------------------------------
###project: Forage Fish Resilience
###author(s): MW
###goal(s): generate a map with all study sites
###date(s): 
###note(s): 

# Housekeeping ------------------------------------------------------------
### load necessary libraries ----
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, scales, broom, purrr, dataRetrieval,
                 sf, ggspatial)

### define custom functions ----
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

### load necessary data ----
### proprietary data ---
dat <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |>
      mutate(estuary = as.factor(estuary), bay = as.factor(bay), y = biomass_stability) |> 
      dplyr::select(estuary, bay, zone, grid, lat, long, n_obs) |> 
      distinct()

fl <- st_read('../../shapefiles/fce_shapefiles/Florida_State_Boundary.shp')

coords1 <- dat |>
      dplyr::select(bay, lat, long, n_obs) |>
      mutate(
            bay = case_when(
                  bay == 'TQ' ~ 'SIR',
                  bay == 'IR' ~ 'NIR',
                  TRUE ~ bay
            )
      ) |>
      rename(x = long, y = lat) |>
      st_as_sf(coords = c("x", "y"), crs = 4326) |>
      st_transform(crs = 4326)

coords1$on_land <- lengths(st_intersects(coords1, fl)) > 0

coords1 <- coords1 |>
      mutate(habitat_type = if_else(on_land, "riverine", "estuarine")) |>
      rename(site = geometry)
print(coords1)

estuary_palette_abb = c("AP"="#e6d7b9",
                        "CH"="#a7c4a0",
                        "CK"="#64a988",
                        'NIR'="#89c8d9",
                        "JX"="#f2c6b4",
                        'TB'='#dba5a4',
                        "SIR"="#bfaed9")

est_bb <- dat |> 
      group_by(bay) |>
      summarise(
            xmin = min(long) - 0.05,
            xmax = max(long) + 0.05,
            ymin = min(lat) - 0.05,
            ymax = max(lat) + 0.05
      )
print(est_bb)

MAIN <- ggplot() +   
      geom_sf(data = fl, fill = 'grey', color = NA) +
      geom_sf(data = coords1, color = 'white', size = 2.75) +
      geom_sf(data = coords1, aes(color = bay), size = 2.00) +
      annotation_scale(location = "br", width_hint = 0.3,
                       bar_cols = c("black", "aliceblue"), text_cex = 1.0) +
      annotation_north_arrow(location = "bl", which_north = "true",
                             style = north_arrow_fancy_orienteering(fill = c("black", "aliceblue"),
                                                                    line_col = "black" ),
                             height = unit(2.0, "cm"), width = unit(2.0, "cm")) +
      scale_color_manual(values = estuary_palette_abb) +
      theme_bw() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill="aliceblue"),
            plot.title = element_text(size = 18, hjust=0.5),
            legend.title = element_blank(),
            legend.background = element_rect(fill = 'aliceblue'),
            legend.position = c(0.10,0.50),
            legend.box.background = element_rect(fill = NA, color = 'black', linewidth = 2),
            legend.text = element_text(size = 12, face = 'bold'))
MAIN

print(est_bb)
AP <- ggplot() +   
      geom_sf(data = fl, fill = 'grey', color = NA) +
      geom_sf(data = coords1, color = 'white', size = 2.75) +
      # geom_sf(data = coords1, aes(color = bay, shape = habitat_type), size = 2.00) +
      geom_sf(data = coords1, aes(color = bay), size = 2.00) +
      coord_sf(xlim = c(-85.2, -84.5), ylim = c(29.5, 30.0)) + 
      annotation_scale(location = "br", width_hint = 0.3,
                       bar_cols = c("black", "aliceblue"), text_cex = 1.0) +
      annotation_north_arrow(location = "bl", which_north = "true",
                             style = north_arrow_fancy_orienteering(fill = c("black", "aliceblue"),
                                                                    line_col = "black" ),
                             height = unit(2.0, "cm"), width = unit(2.0, "cm")) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(title = 'Apalachicola Bay') +
      theme_bw() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill="aliceblue"),
            plot.title = element_text(size = 16, hjust=0.5),
            legend.title = element_blank(),
            legend.background = element_rect(fill = 'aliceblue'),
            legend.position = 'none',
            legend.box.background = element_rect(fill = NA, color = 'black', linewidth = 2),
            legend.text = element_text(size = 12, face = 'bold'))
AP

print(est_bb)
CH <- ggplot() +   
      geom_sf(data = fl, fill = 'grey', color = NA) +
      geom_sf(data = coords1, color = 'white', size = 2.75) +
      # geom_sf(data = coords1, aes(color = bay, shape = habitat_type), size = 2.00) +
      geom_sf(data = coords1, aes(color = bay), size = 2.00) +
      coord_sf(xlim = c(-82.3, -81.7), ylim = c(26.5, 27.1)) + 
      annotation_scale(location = "br", width_hint = 0.3,
                       bar_cols = c("black", "aliceblue"), text_cex = 1.0) +
      annotation_north_arrow(location = "bl", which_north = "true",
                             style = north_arrow_fancy_orienteering(fill = c("black", "aliceblue"),
                                                                    line_col = "black" ),
                             height = unit(2.0, "cm"), width = unit(2.0, "cm")) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(title = 'Charlotte Harbor') +
      theme_bw() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill="aliceblue"),
            plot.title = element_text(size = 16, hjust=0.5),
            legend.title = element_blank(),
            legend.background = element_rect(fill = 'aliceblue'),
            legend.position = 'none',
            legend.box.background = element_rect(fill = NA, color = 'black', linewidth = 2),
            legend.text = element_text(size = 12, face = 'bold'))
CH

print(est_bb)
CK <- ggplot() +   
      geom_sf(data = fl, fill = 'grey', color = NA) +
      geom_sf(data = coords1, color = 'white', size = 2.75) +
      # geom_sf(data = coords1, aes(color = bay, shape = habitat_type), size = 2.00) +
      geom_sf(data = coords1, aes(color = bay), size = 2.00) +
      coord_sf(xlim = c(-83.3, -82.9), ylim = c(29.0, 29.4)) + 
      annotation_scale(location = "br", width_hint = 0.3,
                       bar_cols = c("black", "aliceblue"), text_cex = 1.0) +
      annotation_north_arrow(location = "bl", which_north = "true",
                             style = north_arrow_fancy_orienteering(fill = c("black", "aliceblue"),
                                                                    line_col = "black" ),
                             height = unit(2.0, "cm"), width = unit(2.0, "cm")) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(title = 'Cedar Key') +
      theme_bw() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill="aliceblue"),
            plot.title = element_text(size = 16, hjust=0.5),
            legend.title = element_blank(),
            legend.background = element_rect(fill = 'aliceblue'),
            legend.position = 'none',
            legend.box.background = element_rect(fill = NA, color = 'black', linewidth = 2),
            legend.text = element_text(size = 12, face = 'bold'))
CK

print(est_bb)
NIR <- ggplot() +   
      geom_sf(data = fl, fill = 'grey', color = NA) +
      geom_sf(data = coords1, color = 'white', size = 2.75) +
      # geom_sf(data = coords1, aes(color = bay, shape = habitat_type), size = 2.00) +
      geom_sf(data = coords1, aes(color = bay), size = 2.00) +
      coord_sf(xlim = c(-80.8, -80.3), ylim = c(27.6, 28.7)) + 
      annotation_scale(location = "br", width_hint = 0.3,
                       bar_cols = c("black", "aliceblue"), text_cex = 1.0) +
      annotation_north_arrow(location = "bl", which_north = "true",
                             style = north_arrow_fancy_orienteering(fill = c("black", "aliceblue"),
                                                                    line_col = "black" ),
                             height = unit(2.0, "cm"), width = unit(2.0, "cm")) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(title = 'Northern Indian River') +
      theme_bw() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill="aliceblue"),
            plot.title = element_text(size = 16, hjust=0.5),
            legend.title = element_blank(),
            legend.background = element_rect(fill = 'aliceblue'),
            legend.position = 'none',
            legend.box.background = element_rect(fill = NA, color = 'black', linewidth = 2),
            legend.text = element_text(size = 12, face = 'bold'))
NIR

print(est_bb)
JX <- ggplot() +   
      geom_sf(data = fl, fill = 'grey', color = NA) +
      geom_sf(data = coords1, color = 'white', size = 2.75) +
      # geom_sf(data = coords1, aes(color = bay, shape = habitat_type), size = 2.00) +
      geom_sf(data = coords1, aes(color = bay), size = 2.00) +
      coord_sf(xlim = c(-81.8, -81.4), ylim = c(29.6, 30.8)) + 
      annotation_scale(location = "br", width_hint = 0.3,
                       bar_cols = c("black", "aliceblue"), text_cex = 1.0) +
      annotation_north_arrow(location = "bl", which_north = "true",
                             style = north_arrow_fancy_orienteering(fill = c("black", "aliceblue"),
                                                                    line_col = "black" ),
                             height = unit(2.0, "cm"), width = unit(2.0, "cm")) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(title = 'Jacksonville') +
      theme_bw() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill="aliceblue"),
            plot.title = element_text(size = 16, hjust=0.5),
            legend.title = element_blank(),
            legend.background = element_rect(fill = 'aliceblue'),
            legend.position = 'none',
            legend.box.background = element_rect(fill = NA, color = 'black', linewidth = 2),
            legend.text = element_text(size = 12, face = 'bold'))
JX

print(est_bb)
TB <- ggplot() +   
      geom_sf(data = fl, fill = 'grey', color = NA) +
      geom_sf(data = coords1, color = 'white', size = 2.75) +
      # geom_sf(data = coords1, aes(color = bay, shape = habitat_type), size = 2.00) +
      geom_sf(data = coords1, aes(color = bay), size = 2.00) +
      coord_sf(xlim = c(-82.8, -82.3), ylim = c(27.4, 28.1)) + 
      annotation_scale(location = "br", width_hint = 0.3,
                       bar_cols = c("black", "aliceblue"), text_cex = 1.0) +
      annotation_north_arrow(location = "bl", which_north = "true",
                             style = north_arrow_fancy_orienteering(fill = c("black", "aliceblue"),
                                                                    line_col = "black" ),
                             height = unit(2.0, "cm"), width = unit(2.0, "cm")) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(title = 'Tampa Bay') +
      theme_bw() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill="aliceblue"),
            plot.title = element_text(size = 16, hjust=0.5),
            legend.title = element_blank(),
            legend.background = element_rect(fill = 'aliceblue'),
            legend.position = 'none',
            legend.box.background = element_rect(fill = NA, color = 'black', linewidth = 2),
            legend.text = element_text(size = 12, face = 'bold'))
TB

print(est_bb)
SIR <- ggplot() +   
      geom_sf(data = fl, fill = 'grey', color = NA) +
      geom_sf(data = coords1, color = 'white', size = 2.75) +
      # geom_sf(data = coords1, aes(color = bay, shape = habitat_type), size = 2.00) +
      geom_sf(data = coords1, aes(color = bay), size = 2.00) +
      coord_sf(xlim = c(-80.4, -80.0), ylim = c(26.8, 27.4)) + 
      annotation_scale(location = "br", width_hint = 0.3,
                       bar_cols = c("black", "aliceblue"), text_cex = 1.0) +
      annotation_north_arrow(location = "bl", which_north = "true",
                             style = north_arrow_fancy_orienteering(fill = c("black", "aliceblue"),
                                                                    line_col = "black" ),
                             height = unit(2.0, "cm"), width = unit(2.0, "cm")) +
      scale_color_manual(values = estuary_palette_abb) +
      labs(title = 'Southern Indian River') +
      theme_bw() +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill="aliceblue"),
            plot.title = element_text(size = 16, hjust=0.5),
            legend.title = element_blank(),
            legend.background = element_rect(fill = 'aliceblue'),
            legend.position = 'none',
            legend.box.background = element_rect(fill = NA, color = 'black', linewidth = 2),
            legend.text = element_text(size = 12, face = 'bold'))
SIR

