###project: Forage Fish Resilience
###author(s): MW
###goal(s): generating figures for forage fish midterm report
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readr, readxl, dplyr, stringr, splitstackshape, purrr, 
                 zoo, pracma)

### set simple workflow functions ---
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

### read in necessary data ---

tbl_corp_hydrolab <- read_rds('local-data/archive/Hydro.RDS') |> 
      select(-Flag) |> 
      janitor::clean_names()
bio <- read_rds("local-data/forage_fish_master1.RDS") |> 
      select(-mean_weight_g, -min_weight_g, -max_weight_g, -tot_n, -lw_a, -lw_b, -lw_r2)
traits <- read_csv("local-data/key-datasets/ff_traits_filled.csv")

df <- bio |> left_join(traits, by = c("common_name", "scientific_name")) |> 
      mutate(mean_length_cm = mean_length/10,
             min_length_cm = min_length/10,
             max_length_cm = max_length/10,
             test = mean_length_cm - tl_max) |> 
      mutate(mean_length_cm = case_when(
            mean_length_cm >= tl_max ~ tl_max,
            TRUE ~ mean_length_cm)) |> 
      mutate(min_length_cm = case_when(
            min_length_cm >= tl_max ~ tl_max,
            TRUE ~ min_length_cm)) |> 
      mutate(max_length_cm = case_when(
            max_length_cm >= tl_max ~ tl_max,
            TRUE ~ max_length_cm)) |> 
      select(-test, -mean_length, -min_length, -max_length, -taxon_group, -depth) |> 
      mutate(mean_weight_g = a * mean_length_cm^b,
             min_weight_g = a * min_length_cm^b,
             max_weight_g = a * max_length_cm^b)

# df_phys <- df |> left_join(tbl_corp_hydrolab, by = "reference") |>  distinct() |> 
#       rename(ph = p_h,
#              do2 = dissolved_o2,
#              temp_c = temperature, 
#              sal_ppt = salinity,
#              cond = conductivity) |> 
#       dplyr::select(-beg_end)
# glimpse(df_phys)
# df <- df_phys
# rm(bio ,df_phys, tbl_corp_hydrolab, traits)

write_rds(df, "local-data/key-datasets/forage_fish_master.RDS")

# comm_bm <- df |> 
#       group_by(bay, gear_details, year, month, zone, subzone, grid) |> 
#       summarize(comm_tot_bm = sum(n*mean_weight_g, na.rm = TRUE),
#                 comm_areal_bm = comm_tot_bm/area_m2) |> 
#       ungroup() |> 
#       distinct() |> 
#       group_by(bay, year, month) |> 
#       summarize(mean_comm_bm = mean(comm_areal_bm, na.rm = TRUE),
#                 sd_comm_bm = sd(comm_areal_bm, na.rm = TRUE)) |> 
#       ungroup() |> 
#       mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |> 
#       group_by(bay, year) |> 
#       mutate(annual_comm_bm = mean(mean_comm_bm, na.rm = TRUE),
#              sd_comm_bm = sd(mean_comm_bm, na.rm = TRUE))
# 
# comm_bm |>
#       ggplot(aes(x = date, y = mean_comm_bm, group = bay, color = bay)) +
#       # geom_ribbon(aes(ymin = mean_comm_bm - sd_comm_bm,
#       #             ymax = mean_comm_bm + sd_comm_bm),
#       #             alpha = 0.2) +
#       geom_line(size = 1) +
#       facet_wrap(~bay, scales = "free") + 
#       theme(axis.text = element_text(size = 14, face = "bold", colour = "black"),
#             axis.title = element_text(size = 16, face = "bold", colour = "black"),
#             plot.title = element_text(size = 16, face = "bold", colour = "black"),
#             panel.grid.major = element_blank(),
#             axis.line = element_line(colour = "black"),
#             panel.grid.minor = element_blank(),
#             panel.border = element_blank(),
#             panel.background = element_blank())