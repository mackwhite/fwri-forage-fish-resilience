###project: Forage Fish Resilience
###author(s): MW
###goal(s): generating figures for forage fish midterm report
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, stringr, splitstackshape, purrr, 
                 zoo, pracma)

### set simple workflow functions ---
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

### read in necessary data ---

tbl_corp_hydrolab <- read_rds('local-data/Hydro.RDS') |> 
      select(-Flag) |> 
      janitor::clean_names()
bio <- read_rds("local-data/forage_fish_master.RDS") |> 
      select(-mean_weight_g, -min_weight_g, -max_weight_g, -tot_n, -lw_a, -lw_b, -lw_r2)
traits <- read_csv("local-data/forage_fish_trait_list_v2.csv") |> 
      select(-l_inf)

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

df_phys <- df |> left_join(tbl_corp_hydrolab, by = "reference") |>  distinct() |> 
      rename(ph = p_h,
             do2 = dissolved_o2,
             temp_c = temperature, 
             sal_ppt = salinity,
             cond = conductivity) |> 
      select(-beg_end)
glimpse(df_phys)
write_rds(df_phys, "local-data/forage_fish_master_v2.RDS")

