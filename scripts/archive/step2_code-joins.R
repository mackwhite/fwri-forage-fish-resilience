###project: Forage Fish Resilience
###author(s): MW
###goal(s): Relating FIM Codes
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, vegan, readxl, e1071, dplyr, splitstackshape)

codes <- read_rds('local-data/FIM_Codes.RDS') |> 
      filter(FieldName %in% c('Bay', 'BottomType', 'BottomVeg',
                              'BottomVegRatio', 'Gear', 'Length_meas_type',
                              'Length_measurement_type', 'Project_1', 
                              'Project_2', 'Project_3', 'Projects',
                              'Seagrass_habitat_descriptor', 'Stratum',
                              'Type', 'Zone'
                              )) |> 
      mutate(FieldName = case_when(
            FieldName == "Length_meas_type" ~ "Length_measurement_type",
            TRUE ~ FieldName,
            )) |> 
      mutate(FieldName = case_when(
            FieldName == "Project_1" ~ "Project",
            FieldName == "Project_2" ~ "Project",
            FieldName == "Project_3" ~ "Project",
            FieldName == "Projects" ~ "Project",
            TRUE ~ FieldName,
      )) |> 
      filter(FieldName != 'Project',
             FieldName != 'Zone') |> 
      select(FieldName, Code, Description, Category, Gear_in_use, Bay) |> 
      rename(Bay_original = Bay) |> 
      distinct()

codes_wide <- codes |> 
      # mutate(CodeDescription = paste(Code, Description, sep = '_')) |> 
      pivot_wider(
            names_from = 'FieldName',
            values_from = 'Code'
      )

bay <- codes_wide |> select(Bay, Description) |> 
      filter(Bay != 'NULL') |> 
      mutate(Bay = as.character(Bay))

write_csv(bay, 'local-data/for-joins/Bay.csv')

bottom_type <- codes_wide |> select(BottomType, Description, Category) |> 
      filter(BottomType != 'NULL')
write_csv(bottom_type, 'local-data/for-joins/BottomType.csv')

bottom_veg <- codes_wide |> select(BottomVeg, Description, Category) |> 
      filter(BottomVeg != 'NULL')
write_csv(bottom_veg, 'local-data/for-joins/BottomVeg.csv')

bottom_veg_ratio <- codes_wide |> select(BottomVegRatio, Description, Category) |> 
      filter(BottomVegRatio != 'NULL')
write_csv(bottom_veg_ratio, 'local-data/for-joins/BottomVegRatio.csv')

gear <- codes_wide |> select(Gear, Description, Category) |> 
      filter(Gear != 'NULL') |> 
      filter(!Category %in% c('No gear', '61m Blocknet', 'Purse Seine',
                              'Gillnet', 'Trammel Net', '6.1-m Trawl',
                              'Trawl', 'Dropnet', 'Trap', 'Pound Net', 'Trammel net',
                              'Stop Net', 'Fish Trap', 'Fyke Net', 'Miscellaneous',
                              'Plankton Net', 'Push Net', 'Dip Net', 'Camera', 'Seome'
                              )) |> 
      select(-Description) |> 
      mutate(Gear = as.character(Gear)) |> 
      distinct() |> 
      mutate(GearCategory = "Seine") |> 
      rename(GearDetails = Category)
write_csv(gear, 'local-data/for-joins/Gear.csv')

length_measurement_type <- codes_wide |> select(Length_measurement_type, Description, Category) |> 
      filter(Length_measurement_type != 'NULL')
write_csv(length_measurement_type, 'local-data/for-joins/Length_measurement_type.csv')

seagrass_habitat_descriptor <- codes_wide |> select(Seagrass_habitat_descriptor, Description, Category) |> 
      filter(Seagrass_habitat_descriptor != 'NULL')
write_csv(seagrass_habitat_descriptor, 'local-data/for-joins/Seagrass_habitat_descriptor.csv')

stratum <- codes_wide |> select(Stratum, Description, Category) |> 
      filter(Stratum != 'NULL')
write_csv(stratum, 'local-data/for-joins/Stratum.csv')

type <- codes_wide |> select(Type, Description, Category) |> 
      filter(Type != 'NULL')
write_csv(type, 'local-data/for-joins/Type.csv')