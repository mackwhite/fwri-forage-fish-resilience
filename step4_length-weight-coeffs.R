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
dat <- read_rds('local-data/fim-master-short.RDS') |> 
      ### can't take the camel case and capital letters anymore
      janitor::clean_names() |> 
      mutate(taxon_group = case_when(
            str_detect(commonname, regex("shrimp", ignore_case = TRUE)) ~ "shrimps",
            str_detect(commonname, regex("shark", ignore_case = TRUE)) ~ "sharks",
            str_detect(commonname, regex("crab", ignore_case = TRUE)) ~ "crabs",
            TRUE ~ "fishes")) |> 
      select(reference, sampling_date, bay, gear, gear_details, rep, latitude, longitude, zone, 
             subzone, grid, depth, commonname, scientificname, taxon_group, everything()) |> 
      rename(common_name = commonname, 
             scientific_name = scientificname)
glimpse(dat)
lw_data <- read_rds('local-data/Lengthweight.RDS') 
spp_codes <- read_rds('local-data/Species_Codes.RDS')

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

spp_bm <- dat5 |> 
      group_by(bay, common_name, scientific_name, gear_details, year, month, zone, subzone, grid) |> 
      summarize(spp_tot_bm = n*mean_weight_g,
                spp_areal_bm = spp_tot_bm/area_m2) |> 
      distinct()
nacheck(spp_bm)

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
# comm_bm |>
#       # filter(gear_details == "21.3-m Seine") |>
#       filter(bay == "TQ") |> 
#       ggplot(aes(x=date, y = mean_comm_bm, color = gear_details)) +
#       geom_line(linewidth = 1.5) +
#       facet_wrap(~zone, scales = "free")
