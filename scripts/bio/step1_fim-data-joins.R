###project: Forage Fish Resilience
###author(s): MW
###goal(s): Relating all data tables
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, readxl, dplyr, splitstackshape)

### read in data ---
tbl_corp_physical_master <- read_rds('local-data/Phys.RDS') |> 
      ### change gear to character class for join to biological data
      mutate(Gear = as.character(Gear)) |> 
      select(-Flag)
tbl_corp_habitat <- read_rds('local-data/Habitat.RDS') |> 
      select(-Flag)
tbl_corp_hydrolab <- read_rds('local-data/Hydro.RDS') |> 
      select(-Flag)
tbl_corp_biology_number <- read_rds('local-data/Bio.RDS')
tbl_corp_biology_lengths <- read_rds('local-data/Leng.RDS') |> 
      select(-Flag)
tbl_corp_biology_measurements <- read_rds('local-data/Lengthweight.RDS')
tbl_ref_species_list <- read_rds('local-data/Species_Codes.RDS')
tbl_corp_biology_number <- tbl_corp_biology_number |> 
      left_join(tbl_ref_species_list, by = c("NODCCODE", "Scientificname"), relationship = 'many-to-many')

phys <- tbl_corp_physical_master |> 
      left_join(tbl_corp_habitat, by = "Reference", relationship = 'many-to-many') |>
      left_join(tbl_corp_hydrolab, by = "Reference", relationship = 'many-to-many') |> 
      select(Reference, Sampling_Date, Gear, Rep, Longitude, Latitude, StartDepth,
             Zone, Subzone, Grid,
             BottomVegCover, Seagrass_habitat_descriptor, Dist_to_Shore, BottomVeg, BottomVegRatio,
             BottomType, Depth, Temperature)

bio <- tbl_corp_biology_number|> 
      left_join(tbl_corp_biology_lengths, by = c("Reference", "Species_record_id"), relationship = 'many-to-many') |> 
      left_join(tbl_corp_biology_measurements, by = c("Reference", "Species_record_id", "Length_record_id")) |> 
      select(Reference, Species_record_id, NODCCODE, Scientificname, N, Commonname, Length_record_id, Length,
             Estuary, SL, FL, TL, TotalWeight)

### clean environment ---
keep <- c("phys", "bio")
rm(list = setdiff(ls(), keep))

### join that giant sucker together ---
all <- phys |> left_join(bio, by = "Reference")

### clean environment ---
keep <- c("all")
rm(list = setdiff(ls(), keep))

all1 <- all |> 
      separate(Reference, into = c('Bay', 'RefRest'), sep = 2, remove = FALSE) 

### clean environment ---
keep <- c("all1")
rm(list = setdiff(ls(), keep))

all2 <- all1 |> 
      separate(RefRest, into = c('Type', "RefRester"), sep = 1, remove = TRUE)

### clean environment ---
keep <- c("all2")
rm(list = setdiff(ls(), keep))

all3 <- all2 |> 
      select(Reference, Bay, Type, Sampling_Date, Gear, Rep, Latitude, Longitude, Zone, Subzone, Grid, 
             Depth, Scientificname, Commonname, N, Length)
      
### clean environment ---
keep <- c("all3")
rm(list = setdiff(ls(), keep))

all4 <- all3 |> 
      filter(Type == "M")

### clean environment ---
keep <- c("all4")
rm(list = setdiff(ls(), keep))

all5 <- all4 |> 
      filter(Bay %in% c("AP", "TB", "CH", "CK", "TQ", "IR", "JX"))

### clean environment ---
keep <- c("all5")
rm(list = setdiff(ls(), keep))
# gear <- read_csv('local-data/for-joins/Gear.csv') |> mutate(Gear = as.character(Gear))

all6 <- all5 |> left_join(gear, by = "Gear") |> 
      filter(GearCategory == 'Seine')

### clean environment ---
keep <- c("all6")
rm(list = setdiff(ls(), keep))

all7 <- all6 |> filter(!is.na(GearCategory))

### clean environment ---
keep <- c("all7")
rm(list = setdiff(ls(), keep))

all8 <- all7 |> 
      select(-c(Type, GearCategory))

### clean environment ---
keep <- c("all8")
rm(list = setdiff(ls(), keep))

### check for NAs
na_count_per_column <- sapply(all8, function(x) sum(is.na(x)))
print(na_count_per_column)

### clean environment ---
keep <- c("all8")
rm(list = setdiff(ls(), keep))

### save rds function (think quicker than built-in write_rds) ---
save_rds <- function(data, filename) {
      saveRDS(data, file = filename)
      return(data)
}

save_rds(all8, 'local-data/fim-master.RDS')

species_list <- all8 |> 
      select(Scientificname, Commonname) |> 
      distinct() 
writexl::write_xlsx(species_list, 'local-data/species-list.xlsx')