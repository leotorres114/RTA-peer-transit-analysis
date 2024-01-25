library(tidyverse)
library(here)
library(readxl)
library(lubridate)

# create peer table
source("src/peer_table.R")

# Constants
FILE_NAME <-  "ntd_monthly_nov_2023.xlsx"
FIRST_MONTH <-  '1/2002'
LAST_MONTH <-  '11/2023'

# Download latest monthly data from the link below and rename
# (file path may be too long with original file name):
# https://www.transit.dot.gov/ntd/data-product/monthly-module-adjusted-data-release

ridership <-
  readxl::read_excel(path = here("raw_data", FILE_NAME),
                     sheet = "UPT")
vrm <-
  readxl::read_excel(path = here("raw_data", FILE_NAME),
                     sheet = "VRM")
vrh <-
  readxl::read_excel(path = here("raw_data", FILE_NAME),
                     sheet = "VRH")

peers <-
  read_csv(file = here("processed_data", "rta_peers_ntd.csv"))

# For looping purposes
names_of_datasets <- c("ridership", "vrm", "vrh")
ntd_list <- list(ridership, vrm, vrh)

# The for loop below:
# 1. Merges each NTD metric with the peer table
# 2. Cleans up formatting (parses month/year, transforms table to long format,
#    and edits column order and naming)
# 3. Exports each NTD metric table

for (i in 1:length(ntd_list)) {
  # 1
  temp = peers %>%
    merge(
      ntd_list[[i]],
      by.x = c("ntdid", "ntd_mode"),
      by.y = c("NTD ID", "Mode"),
      all.x = TRUE
    )
  
  # 2
  temp = temp %>%
    pivot_longer(
      .,
      cols = all_of(FIRST_MONTH):all_of(LAST_MONTH),
      #update months as needed
      names_to = "month_year",
      values_to = names_of_datasets[i]
    ) %>%
    mutate(month = month(lubridate::my(month_year)),
           year = year(lubridate::my(month_year))) %>%
    select(
      ntdid,
      agency,
      acronym,
      mode,
      ntd_mode,
      region = `UZA Name`,
      type_of_svc = TOS,
      month,
      year,
      names_of_datasets[i]
    )
  
  # 3
  write_csv(temp, here(
    "processed_data",
    paste0(names_of_datasets[i], "_peers_monthly.csv")
  ))
}

# clear env
rm(list = ls())