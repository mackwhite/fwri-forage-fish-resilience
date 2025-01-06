# Load packages ####
library(tidyverse)
library(dbplyr)
library(odbc)
library(lubridate)

# Connect to database
Corp <- dbConnect(odbc::odbc(),
                  driver = "SQL Server",
                  server = "", # Enter your local server path here
                  database = "FIMCorpInshore",
                  trusted_connection = TRUE)

# Pull physical data
physmast <- tbl(Corp, in_schema("hsdb","tbl_corp_physical_master")) %>% # The tbl function and in_schema function can be used to select a table from the database
  collect() # The collect function pulls the dataset from the database into your r environment as a tibble

# Pull biological data
Bio <- tbl(Corp, in_schema("hsdb","tbl_corp_biology_number")) %>%
  select(Reference, Species_record_id, Splittype, Splitlevel, Cells, NODCCODE, Number, FHC) %>% # You can use tidyverse dplyr functions to wrangle data prior to pulling it into your environment (from the dbplyr package)
  filter(FHC != "D") %>%
  mutate(Count = case_when(!is.na(as.numeric(Splittype)) ~ Number*(as.numeric(Splittype)^as.numeric(Splitlevel)),
                           TRUE ~ as.numeric(Number))) %>%
  select(Reference, Species_record_id, NODCCODE, Count) %>%
  group_by(Reference, NODCCODE) %>%
  mutate(N = sum(Count)) %>%
  left_join(tbl(Corp, in_schema("hsdb","tbl_corp_ref_species_list")), by = "NODCCODE") %>% # You can also join data from another table in the database
  select(Reference, Species_record_id, NODCCODE, Scientificname, N) %>%
  arrange(Reference, NODCCODE) %>%
  collect()

