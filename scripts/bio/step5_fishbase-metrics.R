###project: Forage Fish Resilience
###author(s): MW
###goal(s): pulling from fishbase
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, stringr, splitstackshape, purrr, 
                 zoo, pracma, rfishbase)

### set simple workflow functions ---
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

### read in necessary data ---

spp <- read_xlsx('local-data/archive/forage_fish_trait_list.xlsx') |> 
      janitor::clean_names() |> 
      select(scientific_name, common_name, family, genus, specific_epithet)

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

test <- rfishbase::ecology(spp$scientific_name)

spp_ests <- spp |> 
      left_join(ests, by = "scientific_name") |> 
      group_by(genus) |> 
      mutate(across(where(is.numeric),
            ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) |> 
      ungroup() |> 
      group_by(family) |> 
      mutate(across(where(is.numeric),
                    ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) |> 
      ungroup() |> 
      mutate(across(where(is.numeric),
                    ~ifelse(is.nan(.), NA, .)))

writexl::write_xlsx(spp_ests, "local-data/forage_fish_trait_list_v2.xlsx")
