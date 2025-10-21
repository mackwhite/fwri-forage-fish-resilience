# Script Details ----------------------------------------------------------
###project: Forage Fish Resilience
###author(s): MW
###goal(s): creating habitat types
###date(s):
###note(s):

# Housekeeping ------------------------------------------------------------
### load necessary libraries ----
# install.packages("librarian")
librarian::shelf(tidyverse, readxl,scales,broom,purrr,dataRetrieval,
                 sf,ggspatial,factoextra,corrplot,viridis,ggrepel,scales)

### define custom functions ----
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x)
            sum(is.na(x)))
      print(na_count_per_column)
}

### load necessary data ----
### proprietary data ---
sites <- read_csv('local-data/key-datasets/stability_foragefish_final.csv') |>
      dplyr::select(estuary, bay, zone, grid) |>
      distinct() |> 
      mutate(grid = as.character(grid))
glimpse(sites)

gear <- read_csv('local-data/archive/for-joins/Gear.csv') |>
      mutate(Gear = as.character(Gear)) |>
      janitor::clean_names()
glimpse(gear)

hydro <- readRDS('local-data/archive/Hydro.RDS') |>
      janitor::clean_names() |> 
      separate(reference, into = c('bay', 'refrest'), sep = 2, remove = FALSE) |> 
      separate(refrest, into = c('type', "refrester"), sep = 1, remove = TRUE) |> 
      filter(bay %in% c("AP", "TB", "CH", "CK", "TQ", "IR", "JX")) |> 
      filter(type == "M")
glimpse(hydro)

phys_master <- readRDS('local-data/archive/Phys.RDS') |>
      janitor::clean_names() |>
      mutate(gear = as.character(gear)) |>
      left_join(gear) |>
      filter(gear_details == '21.3-m Seine') |> 
      separate(reference, into = c('bay', 'refrest'), sep = 2, remove = FALSE) |> 
      separate(refrest, into = c('type', "refrester"), sep = 1, remove = TRUE) |> 
      filter(bay %in% c("AP", "TB", "CH", "CK", "TQ", "IR", "JX")) |> 
      filter(type == "M")
glimpse(phys_master)

physical_all <- phys_master |> left_join(hydro) |> 
      dplyr::select(bay, zone, grid, temperature, salinity, 
                    dissolved_o2, depth, bottom_veg_cover) |> 
      mutate(zone = as.character(zone),
             grid = as.character(grid)) |> 
      rename(temp = temperature, sal = salinity, do = dissolved_o2,
             cover = bottom_veg_cover)
glimpse(physical_all)

physical_summary <- physical_all |> 
      group_by(bay, zone, grid) |> 
      summarise(
            across(
                  where(is.numeric),
                  list(
                        min = ~min(.x, na.rm = TRUE),
                        mean = ~mean(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)
                  ),
                  .names = "{.col}_{.fn}"
            ),
            .groups = "drop"
      )
glimpse(physical_summary)
glimpse(sites)

habitats <- sites |> left_join(physical_summary) |> 
      mutate(row_id = row_number())
glimpse(habitats)
nacheck(habitats)

write_csv(habitats, 'local-data/habitat-by-site.csv')
monthly_metrics <- read_csv('local-data/habitat-by-site.csv')
### scale and center the dataset with identifying columns retained ---
glimpse(monthly_metrics)
scaled <- monthly_metrics |> 
      mutate(across(temp_min:cover_sd, ~ as.numeric(scale(.x, center = TRUE)))) |> 
      dplyr::select(estuary, bay, zone, grid,
                    temp_min, temp_mean, temp_max, temp_sd,
                    sal_min, sal_mean, sal_max, sal_sd,
                    do_min, do_mean, do_max, do_sd,
                    depth_min, depth_mean, depth_max, depth_sd,
                    cover_mean, cover_sd)
glimpse(scaled)
summary(scaled)
# num <- scaled |> dplyr::select(depth_mean,
#                                sal_mean, sal_sd,
#                                do_mean, do_sd)
# num <- scaled |> dplyr::select(depth_mean,
#                                cover_mean,
#                                sal_mean, sal_sd)
# num <- scaled |> dplyr::select(sal_mean, depth_mean, sal_sd)
num <- scaled |> dplyr::select(sal_min, sal_max, sal_sd)
# num <- scaled |> dplyr::select(sal_mean, sal_min, sal_max, sal_sd)

glimpse(num)
corr <- cor(num, use = "pairwise.complete.obs")
corrplot(corr, method = "color", tl.cex = 0.7, order = "hclust",
         addCoef.col = "black",
         number.cex = 0.6,
         addgrid.col = "white")

pca_output <- prcomp(num, center = TRUE, scale. = TRUE)
pca_summary <- summary(pca_output)
summary(pca_output)
fviz_eig(pca_output)
eig.val<-get_eigenvalue(pca_output)
eig.val
# Kaiser–Guttman criterion method of determining number of eigenvalues to retain ---
subset(eig.val, eigenvalue > 1)

### retained first two PCs (>70% of variance)
pca_data <- pca_output$x[,1:2]
pca_data_dt <- as.data.frame(pca_data)
fviz_nbclust(pca_data, kmeans, method = "wss") + 
      labs(title = "Elbow method for optimal k")
fviz_nbclust(pca_data, kmeans, method = "silhouette") + 
      labs(title = "Silhouette method for optimal k")

pca_results_dt <- pca_data_dt |> 
      mutate(row_id = monthly_metrics$row_id)

set.seed(20)
km_res <- kmeans(pca_data, centers = 3, nstart = 25)
km_res$size
km_res$centers 
pca_results_dt$cluster <- as.factor(km_res$cluster)

monthly_metrics_clustered <- monthly_metrics |> 
      left_join(pca_results_dt |> dplyr::select(row_id, cluster), by = "row_id") |> 
      dplyr::select(-row_id) |> 
      mutate(syndrome = case_when(
            cluster == 1 ~ 'estuarine',
            cluster == 2 ~ 'marine',
            cluster == 3 ~ 'freshwater'))

plasma_colors <- plasma(3, direction = 1)
cluster_palette <- c(
      "freshwater" = plasma_colors[1],  # darkest = low [resident]
      "estuarine" = plasma_colors[2],  # middle = medium [migrant]
      "marine" = plasma_colors[3]   # brightest = high [nomad]
)
show_col(cluster_palette)

loadings_scaled <- as.data.frame(pca_output$rotation[, 1:2]) |> 
      rownames_to_column(var = "label") |> 
      mutate(
            PC1 = PC1 * max(abs(pca_results_dt$PC1)) * 0.8,
            PC2 = PC2 * max(abs(pca_results_dt$PC2)) * 0.8
      )

habitatcluster <- pca_results_dt |> 
      mutate(syndrome = case_when(
            cluster == 1 ~ 'estuarine',
            cluster == 2 ~ 'marine',
            cluster == 3 ~ 'freshwater')
      ) |> 
ggplot(aes(x = PC1, y = PC2)) +
      stat_ellipse(
            aes(color = syndrome),
            size = 1,
            geom = "path",
            type = "norm",
            level = 0.5,
            alpha = 1,
            show.legend = FALSE
      ) +
      geom_point(aes(fill = syndrome), size = 2.2, alpha = 0.6, color = "black", shape = 21) +
      geom_segment(
            data = loadings_scaled,
            aes(x = 0, y = 0, xend = PC1, yend = PC2),
            arrow = arrow(type = "closed", length = unit(0.15, "inches")),
            color = "black",
            linewidth = 1.1
      ) +
      geom_text_repel(
            data = loadings_scaled,
            aes(x = PC1, y = PC2, label = label),
            color = "black",
            size = 4,
            fontface = "bold",
            segment.color = "grey50",
            max.overlaps = 30
      ) +
      xlab(paste0("PC1 (", round(eig.val$variance.percent[1], 1), "%)")) +
      ylab(paste0("PC2 (", round(eig.val$variance.percent[2], 1), "%)")) +
      scale_fill_manual(values = cluster_palette) +
      scale_color_manual(values = cluster_palette) +
      theme_bw() +
      theme(
            strip.text = element_text(size = 16, face = "bold", colour = "black"),
            axis.text = element_text(size = 12, face = "bold", colour = "black"),
            axis.title = element_text(size = 14, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "right"
      )
habitatcluster