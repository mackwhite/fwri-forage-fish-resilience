###project: Forage Fish Resilience
###author(s): MW, JR
###goal(s): generating length-weight coefficients for each species
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, stringr, splitstackshape, purrr, zoo, pracma)

### set simple workflow functions ---
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

### read in necessary data ---
dat <- read_rds('local-data/archive/fim-master-short.RDS') |> 
      ### can't take the camel case and capital letters anymore
      janitor::clean_names() |> 
      mutate(taxon_group = case_when(
            str_detect(commonname, regex("shrimp", ignore_case = TRUE)) ~ "shrimps",
            str_detect(commonname, regex("shark", ignore_case = TRUE)) ~ "sharks",
            str_detect(commonname, regex("crab", ignore_case = TRUE)) ~ "crabs",
            str_detect(commonname, regex("shelligs", ignore_case = TRUE)) ~ "crabs",
            str_detect(commonname, regex("turtle", ignore_case = TRUE)) ~ "turtles",
            str_detect(commonname, regex("cooter", ignore_case = TRUE)) ~ "turtles",
            str_detect(commonname, regex("terrapin", ignore_case = TRUE)) ~ "turtles",
            str_detect(commonname, regex("ray", ignore_case = TRUE)) ~ "rays",
            TRUE ~ "fishes")) |> 
      select(reference, sampling_date, bay, gear, gear_details, rep, latitude, longitude, zone, 
             subzone, grid, depth, commonname, scientificname, taxon_group, everything()) |> 
      rename(common_name = commonname, 
             scientific_name = scientificname)
glimpse(dat)
lw_data <- read_rds('local-data/archive/Lengthweight.RDS') 
spp_codes <- read_rds('local-data/archive/Species_Codes.RDS')

### add area and remove useless seining data ---
dat1 <- dat |> 
      # https://www.researchgate.net/publication/377181368_Sampling_design_modifications_to_a_fishery-independent_monitoring_survey_balance_the_maintenance_of_long-term_data_with_emerging_management_needs_and_funding_limitations
      mutate(area_m2 = case_when(
            gear_details == "21.3-m Seine" ~ 140,
            gear_details == "Haul Seine" ~ 4120,
            TRUE ~ -9999)) |> 
      filter(area_m2 >= 0)

species_list <- dat1 |> 
      select(common_name, scientific_name, taxon_group) |> distinct()

lwdat <- lw_data |> filter(!is.na(TL), !is.na(TotalWeight), TL > 0, TotalWeight > 0)

spp_coeff <- lwdat |> 
      group_by(TSN) |> 
      mutate(lw_obs = n()) |> 
      filter(lw_obs >= 50) |> 
      nest() |> 
      mutate(model = map(data, ~lm(log(TotalWeight) ~ log(TL), data = .x)),
             coeff = map(model, ~coef(.x)),
             r_squared = map_dbl(model, ~summary(.x)$r.squared)) |> 
      unnest_wider(coeff, names_sep = "_") |> 
      transmute(TSN, 
                a = exp(`coeff_(Intercept)`), 
                b = `coeff_log(TL)`,
                r2 = r_squared)

spp_final <- spp_coeff |> 
      left_join(spp_codes, by = "TSN") |> 
      janitor::clean_names() |> 
      select(commonname, scientificname, tsn, a, b, r2, nodccode) |> 
      rename(common_name = commonname, 
             scientific_name = scientificname,
             lw_a = a, 
             lw_b = b,
             lw_r2 = r2) 

spp_ff <- species_list |> 
      left_join(spp_final, by = c("common_name", "scientific_name"))
nacheck(spp_ff)

dat2 <- dat1 |> 
      left_join(spp_ff, by = c("common_name", "scientific_name", "taxon_group"))

spp_flag <- dat2 |> 
      group_by(common_name, scientific_name, tsn) |> 
      summarize(tot_n = sum(n)) |> 
      mutate(flag = case_when(
            tot_n >= 100 ~ 'keep',
            tot_n < 100 ~ 'toss'
      ))

dat3 <- dat2 |> 
      left_join(spp_flag, by = c("common_name", "scientific_name", "tsn")) |> 
      filter(flag == "keep") |> 
      select(-flag) |> 
      filter(taxon_group == "fishes")

dat4 <- dat3 |> 
      mutate(mean_weight_g = lw_a * mean_length^lw_b,
             min_weight_g = lw_a * min_length^lw_b,
             max_weight_g = lw_a * max_length^lw_b)

dat5 <- dat4 |> 
      mutate(date = as.Date(sampling_date),
             year = year(date),
             month = month(date),
             day = day(date))

### clean environment ---
keep <- c("dat5", "nacheck")
rm(list = setdiff(ls(), keep))

glimpse(dat5)

comm_bm <- dat5 |> 
      group_by(bay, gear_details, year, month, zone, subzone, grid) |> 
      summarize(comm_tot_bm = sum(n*mean_weight_g, na.rm = TRUE),
                comm_areal_bm = comm_tot_bm/area_m2) |> 
      ungroup() |> 
      distinct() |> 
      group_by(bay, gear_details, year, month, zone) |> 
      summarize(mean_comm_bm = mean(comm_areal_bm, na.rm = TRUE)) |> 
      ungroup() |> 
      mutate(date = as.Date(paste(year, month, "01", sep = "-")))

nacheck(comm_bm)
glimpse(comm_bm)

### Apalachicola Bay ---
comm_bm |>
      filter(gear_details == "21.3-m Seine") |> 
      filter(bay == "AP") |> 
      ggplot(aes(x=date, y = mean_comm_bm, color = zone)) +
      geom_line(linewidth = 1.5) +
      facet_wrap(~zone, scales = "free")

### Charlotte Harbor ---
comm_bm |>
      filter(gear_details == "21.3-m Seine") |> 
      filter(bay == "CH") |> 
      ggplot(aes(x=date, y = mean_comm_bm, color = zone)) +
      geom_line(linewidth = 1.5) +
      facet_wrap(~zone, scales = "free")

### Cedar Key ---
comm_bm |>
      filter(gear_details == "21.3-m Seine") |> 
      filter(bay == "CK") |> 
      ggplot(aes(x=date, y = mean_comm_bm, color = zone)) +
      geom_line(linewidth = 1.5) +
      facet_wrap(~zone, scales = "free")

### Northern Indian River Lagoon ---
comm_bm |>
      filter(gear_details == "21.3-m Seine") |> 
      filter(bay == "IR") |> 
      ggplot(aes(x=date, y = mean_comm_bm, color = zone)) +
      geom_line(linewidth = 1.5) +
      facet_wrap(~zone, scales = "free")

### Northeast Florida ---
comm_bm |>
      filter(gear_details == "21.3-m Seine") |> 
      filter(bay == "JX") |> 
      ggplot(aes(x=date, y = mean_comm_bm, color = zone)) +
      geom_line(linewidth = 1.5) +
      facet_wrap(~zone, scales = "free")

### Tampa Bay ---
comm_bm |>
      filter(gear_details == "21.3-m Seine") |> 
      filter(bay == "TB") |> 
      ggplot(aes(x=date, y = mean_comm_bm, color = zone)) +
      geom_line(linewidth = 1.5) +
      facet_wrap(~zone, scales = "free")

### Southern Indian River Lagoon ----
comm_bm |>
      filter(gear_details == "21.3-m Seine") |>
      filter(bay == "TQ") |> 
      ggplot(aes(x=date, y = mean_comm_bm, color = zone)) +
      geom_line(linewidth = 1.5) +
      facet_wrap(~zone, scales = "free")

### filtering based on period of record by estuary -----
### Apalachicola bay ---
## get rid of NA
## D & E are subset of years, remove
## A,B,C look good

### Charlotte Harbor ---
## get rid of NA
## E,G,H,K,and S are pretty small subsets, probably remove
## A,B,C,D,M,P look good

### Cedar Key ---
## get rid of NA
## D is subset of one year, remove
## B,C, and F look good

### Northern Indian River Lagoon ---
## get rid of NA
## I,J, and T have single obs, remove
## G has large gaps in POR, probably remove
## A,B,C,D,E,F,H look good

### Northeast Florida ---
## get rid of 1, 2 & 3
## AR,MC,OR,TR are small subsets, probably remove
## A,B,C,D,E,F look good

### Tampa Bay ---
## get rid of NA
## H and P small subsets, probably remove
## A,B,C,D,E,K,L,M,N look good

### Southern Indian River Lagoon ---
## get rid of NA
## K,L,T don't quite seem correct - only have data since 2014
## including further look: I, J, T look great in terms of haul seines
## maybe drop 21.3-m seines from TQ? and focus on the haul seine data

# ### looking at each estuary with different gear types -----
# 
# ### Apalachicola Bay ---
# comm_bm |>
#       # filter(gear_details == "21.3-m Seine") |> 
#       filter(bay == "AP") |> 
#       ggplot(aes(x=date, y = mean_comm_bm, color = gear_details)) +
#       geom_line(linewidth = 1.5) +
#       facet_wrap(~zone, scales = "free")
# 
# ### Charlotte Harbor ---
# comm_bm |>
#       # filter(gear_details == "21.3-m Seine") |> 
#       filter(bay == "CH") |> 
#       ggplot(aes(x=date, y = mean_comm_bm, color = gear_details)) +
#       geom_line(linewidth = 1.5) +
#       facet_wrap(~zone, scales = "free")
# 
# ### Cedar Key ---
# comm_bm |>
#       # filter(gear_details == "21.3-m Seine") |> 
#       filter(bay == "CK") |> 
#       ggplot(aes(x=date, y = mean_comm_bm, color = gear_details)) +
#       geom_line(linewidth = 1.5) +
#       facet_wrap(~zone, scales = "free")
# 
# ### Northern Indian River Lagoon ---
# comm_bm |>
#       # filter(gear_details == "21.3-m Seine") |> 
#       filter(bay == "IR") |> 
#       ggplot(aes(x=date, y = mean_comm_bm, color = gear_details)) +
#       geom_line(linewidth = 1.5) +
#       facet_wrap(~zone, scales = "free")
# 
# ### Northeast Florida ---
# comm_bm |>
#       # filter(gear_details == "21.3-m Seine") |> 
#       filter(bay == "JX") |> 
#       ggplot(aes(x=date, y = mean_comm_bm, color = gear_details)) +
#       geom_line(linewidth = 1.5) +
#       facet_wrap(~zone, scales = "free")
# 
# ### Tampa Bay ---
# comm_bm |>
#       # filter(gear_details == "21.3-m Seine") |> 
#       filter(bay == "TB") |> 
#       ggplot(aes(x=date, y = mean_comm_bm, color = gear_details)) +
#       geom_line(linewidth = 1.5) +
#       facet_wrap(~zone, scales = "free")
# 
# ### Southern Indian River Lagoon ----
comm_bm |>
      # filter(gear_details == "21.3-m Seine") |>
      filter(bay == "TQ") |>
      ggplot(aes(x=date, y = mean_comm_bm, color = gear_details)) +
      geom_line(linewidth = 1.5) +
      facet_wrap(~zone, scales = "free")

ts_flags <- read_csv("local-data/archive/flag_report_ts_zones.csv")

dat6 <- dat5 |> 
      left_join(ts_flags, by = c("bay", "zone")) |> 
      filter(!is.na(flag)) |> 
      select(-flag)

grid_summary <- dat6 |> 
      group_by(bay, zone) |> 
      summarize(grids = n_distinct(grid))

### lay out forage fish vs other -----

dat7 <- dat6 |> 
      mutate(forage_fish = case_when(
            str_detect(common_name, regex("herring", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("anchov", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("perch", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("bream", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("blenn", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("bumper", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("minnow", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("pompano", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("sleeper", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("jenny", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("mojarra", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("eucinostomus", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("killifish", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("mummichog", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("mosquito", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("choice", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("sardine", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("silverside", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("croaker", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("herring", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("pigfish", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("spot", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("mullet", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("batfish", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("pinfish", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("menhaden", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("goby", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("gobies", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("whiff", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("skilletfish", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("sunfish", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("leatherjack", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("hogchoker", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("sole", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("shad", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("bluegill", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("shiner", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("warmouth", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("darter", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("chubsucker", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("flagfish", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("tomtate", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("grunt", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("jewelfish", ignore_case = TRUE)) ~ "forage fish",
            str_detect(common_name, regex("cichlid", ignore_case = TRUE)) ~ "forage fish",
            TRUE ~ "other"))

test <- dat7 |> 
      select(forage_fish, common_name, scientific_name) |> 
      distinct() 

forage_fish <- test |> filter(forage_fish == "forage fish")

dat8 <- dat7 |> 
      mutate(forage_fish1 = case_when(
            common_name == "drums and croakers" ~ "other",
            common_name == "Spotted Gar" ~ "other",
            common_name == "Spotted Sucker" ~ "other",
            common_name == "Spotted Seatrout" ~ "other",
            common_name == "Florida Pompano" ~ "other",
            TRUE ~ forage_fish
      )) |> 
      select(-forage_fish) |> 
      rename(forage_fish = forage_fish1)

test <- dat8 |> 
      select(forage_fish, common_name, scientific_name) |> 
      distinct() 

forage_fish <- test |> filter(forage_fish == "forage fish")

### examine body size information for each species ----

ff_body_size <- dat8 |> 
      filter(forage_fish == "forage fish") |> 
      group_by(forage_fish, scientific_name, common_name) |> 
      summarize(mean_length = mean(mean_length, na.rm = TRUE),
                max_length = mean(max_length, na.rm = TRUE)) |> 
      arrange(mean_length)

### clean environment ---
keep <- c("dat8", "nacheck")
rm(list = setdiff(ls(), keep))

forage_fish_filtered <- dat8 |> 
      filter(forage_fish == "forage fish") |> 
      select(-forage_fish)

write_rds(forage_fish_filtered, "local-data/forage_fish_master1.RDS")

forage_fish_list <- forage_fish_filtered |> 
      group_by(common_name, scientific_name) |> 
      summarize(mean_length = mean(mean_length, na.rm = TRUE),
                mean_weight = mean(mean_weight_g, na.rm = TRUE),
                a = mean(lw_a, na.rm = TRUE),
                b = mean(lw_b, na.rm = TRUE)) |> 
      ungroup() |> 
      mutate(a = case_when(
            scientific_name == 'Gobiosoma spp.' ~ NA,
            TRUE ~ a
      )) |> 
      mutate(b = case_when(
            scientific_name == 'Gobiosoma spp.' ~ NA,
            TRUE ~ b
      ))
      
# write_csv(forage_fish_list, "local-data/forage_fish_species_list.csv")

# ### creating monthly biomass for report -----
# monthly_comm_bm <- dat8 |> 
#       filter(forage_fish == "forage fish") |> 
#       group_by(bay, gear_details, year, month, zone, grid) |> 
#       summarize(comm_tot_bm = sum(n*mean_weight_g, na.rm = TRUE),
#                 comm_areal_bm = comm_tot_bm/area_m2) |> 
#       ungroup() |> 
#       distinct() |> 
#       group_by(bay, year, month) |> 
#       summarize(mean_comm_bm = mean(comm_areal_bm, na.rm = TRUE)) |> 
#       ungroup() |> 
#       mutate(date = as.Date(paste(year, month, "01", sep = "-")))
# 
# monthly_comm_bm |> 
#       ggplot(aes(x = date, y = mean_comm_bm, color = bay)) +
#       geom_line() +
#       facet_wrap(~bay, scales = "free")

hers_traits <- read_csv("local-data/fish-traits-HERS.csv") |> 
      rename(scientific_name = Species) |> 
      janitor::clean_names()
glimpse(hers_traits)

ff_traits <- forage_fish_list |> 
      left_join(hers_traits, by = "scientific_name") |> 
      select(common_name, scientific_name, mean_length, mean_weight,
             a, b, class, order, k, lm, qb, troph, depth_max, 
             temp_pref_mean, generation_time)

# writexl::write_xlsx(ff_traits, "local-data/forage_fish_trait_list.xlsx")

tbl_corp_hydrolab <- read_rds('local-data/Hydro.RDS') |> 
      select(-Flag) |> 
      janitor::clean_names()

test <- forage_fish_filtered |> 
      left_join(tbl_corp_hydrolab, by = "reference") |> 
      distinct()

test1 <- forage_fish_filtered |> anti_join(test)

### max, min, mean, weighted_mean, quantiles?