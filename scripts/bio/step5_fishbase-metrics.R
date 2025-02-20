###project: Forage Fish Resilience
###author(s): MW
###goal(s): pulling from fishbase
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, stringr, splitstackshape, purrr, 
                 zoo, pracma, rfishbase, readr)

### set simple workflow functions ---
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

### read in necessary data ---

spp <- read_xlsx('local-data/archive/forage_fish_trait_list.xlsx') |> 
      janitor::clean_names() |> 
      select(scientific_name, common_name, family, genus, specific_epithet)

hers <- read_csv("local-data/trait-data/fish-traits-HERS.csv") |> 
      janitor::clean_names() |> 
      select(species, generation_time) |> 
      rename(scientific_name = species)

# test <- rfishbase::diet(spp$scientific_name)
# test1 <- rfishbase::diet_items(spp$scientific_name)
# hers <- read_csv("local-data/archive/fish-traits-HERS.csv") |> 
#       select(troph1 = Troph,
#              scientific_name = Species) 
# 
# join <- hers |> inner_join(hers_estimate)
# 
# hers_estimate <- rfishbase::estimate(hers$scientific_name) |> 
#       select(scientific_name = Species, Troph)
# 
# join |> 
#       ggplot(aes(x= troph1,y=Troph)) +
#       geom_point()+
#       geom_abline()
# 
# test <- lm(join$troph1 ~ join$Troph)
# summary(test)

ests <- rfishbase::estimate(spp$scientific_name) |> 
      janitor::clean_names() |> 
      rename(scientific_name = species,
             tl_max = max_length_tl,
             sl_max = max_length_sl,
             l_inf = linf,
             ppr_min = pred_prey_ratio_min,
             ppr_max = pred_prey_ratio_max) |> 
      select(scientific_name, feeding_path, troph, a, b, k, l_inf, depth_max, depth_min,
             ppr_min, ppr_max, temp_pref_min, temp_pref_mean, temp_pref_max,
             tl_max, sl_max)

ests_1 <- ests |> 
      left_join(hers, by = "scientific_name")

spp_ests <- spp |> 
      left_join(ests_1, by = "scientific_name") |> 
      group_by(genus) |> 
      mutate(across(where(is.numeric),
            ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) |> 
      ungroup() |> 
      group_by(family) |> 
      mutate(across(where(is.numeric),
                    ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) |> 
      ungroup() |> 
      mutate(across(where(is.numeric),
                    ~ifelse(is.nan(.), NA, .))) |> 
      mutate(across(where(is.numeric),
                    ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) |> 
      select(-l_inf)

dom_feeding_path <- spp_ests |> 
      filter(!is.na(feeding_path)) |> 
      group_by(genus, feeding_path) |> 
      summarize(count = n()) |> 
      ungroup() |> 
      group_by(genus) |> 
      slice_max(count, n=1, with_ties = FALSE) |> 
      select(genus, feeding_path)

spp_ests_filled <- spp_ests |> 
      left_join(dom_feeding_path, by = "genus", suffix = c("", "_dominant")) |> 
      mutate(feeding_path = ifelse(is.na(feeding_path), feeding_path_dominant, feeding_path)) |> 
      select(-feeding_path_dominant)

dom_feeding_path1 <- spp_ests_filled |> 
      filter(!is.na(feeding_path)) |> 
      group_by(family, feeding_path) |> 
      summarize(count = n()) |> 
      ungroup() |> 
      group_by(family) |> 
      slice_max(count, n=1, with_ties = FALSE) |> 
      select(family, feeding_path)

spp_ests_filled1 <- spp_ests_filled |> 
      left_join(dom_feeding_path1, by = "family", suffix = c("", "_dominant")) |> 
      mutate(feeding_path = ifelse(is.na(feeding_path), feeding_path_dominant, feeding_path)) |> 
      select(-feeding_path_dominant)

nacheck(spp_ests_filled1)

# writexl::write_xlsx(spp_ests, "local-data/forage_fish_trait_list_v2.xlsx")
writexl::write_xlsx(spp_ests_filled1, "local-data/forage_fish_trait_list_filled.xlsx")
write_csv(spp_ests_filled1, "local-data/key-datasets/ff_traits_filled.csv")
