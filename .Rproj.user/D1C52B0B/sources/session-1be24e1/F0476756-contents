####################################################################################
# This script calculates and binds monthly ridership, revenue hour, and revenue    #
# mile projections to official NTD data. It is recommended to not make projections #
# if insufficient monthly data is available (e.g., less than 5 months). Projections#
# are based on monthly average figures for the selected year.                      #
####################################################################################

# constants
PROJECTION_YEAR = 2023
FIRST_MONTH = '1/2023'
LAST_MONTH = '11/2023'

# Revenue miles forecast --------------------------------------------------

# Pivot filtered temp table to wide format for rowMeans calculations
rev_miles_temp <- rev_miles %>%
  filter(year == PROJECTION_YEAR) %>%
  pivot_wider(
    .,
    names_from = c(month, year),
    values_from = vrm,
    names_sep = "/"
  ) %>%
  ungroup()

# Create a column in temp table for each month being forecasted
# uncomment each month as necessary

#rev_miles_temp$`6/2023` <- rowMeans(rev_miles_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#rev_miles_temp$`7/2023` <- rowMeans(rev_miles_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#rev_miles_temp$`8/2023` <- rowMeans(rev_miles_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#rev_miles_temp$`9/2023` <- rowMeans(rev_miles_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#rev_miles_temp$`10/2023` <- rowMeans(rev_miles_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#rev_miles_temp$`11/2023` <- rowMeans(rev_miles_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
rev_miles_temp$`12/2023` <- rowMeans(rev_miles_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))

# Reformat to match official NTD data
rev_miles_temp <- rev_miles_temp %>%
  select(-FIRST_MONTH:-LAST_MONTH) %>%
  pivot_longer(
    .,
    cols = `12/2023`, # Update range of forecasted months to pivot as necessary
    names_to = c("month", "year"),
    values_to = "vrm",
    names_sep = "/",
    names_transform = as.integer
  )

# Bind projections to official NTD data
rev_miles <-
  rbind(rev_miles, rev_miles_temp) %>% 
  arrange(agency, mode, year)

# Revenue hours forecast (see above for documentation) -----------------------------

###################################################################################################
# For Pace and RIDE ON only - Pace and RIDE ON did not report any VRH for November 2023.
# This section manually adds the monthly average for Pace VRH between Jan-Oct 2023 to
# November before making further forecasts.

pace_vrh_nov2023 <- rev_hrs %>%
  filter(ntdid == 50113 & year == 2023 & month != 11) %>%
  pull(vrh) %>%
  mean()

rev_hrs$vrh[rev_hrs$ntdid == 50113 &
              rev_hrs$month == 11 & 
              rev_hrs$year == 2023] <- pace_vrh_nov2023

rideon_vrh_nov2023 <- rev_hrs %>%
  filter(ntdid == 30051 & year == 2023 & month != 11) %>%
  pull(vrh) %>%
  mean()

rev_hrs$vrh[rev_hrs$ntdid == 30051 &
              rev_hrs$month == 11 & 
              rev_hrs$year == 2023] <- rideon_vrh_nov2023

####################################################################################################

rev_hrs_temp <- rev_hrs %>%
  filter(year == PROJECTION_YEAR) %>%
  pivot_wider(
    .,
    names_from = c(month, year),
    values_from = vrh,
    names_sep = "/"
  )  %>%
  ungroup()

#rev_hrs_temp$`6/2023` <- rowMeans(rev_hrs_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#rev_hrs_temp$`7/2023` <- rowMeans(rev_hrs_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#rev_hrs_temp$`8/2023` <- rowMeans(rev_hrs_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#rev_hrs_temp$`9/2023` <- rowMeans(rev_hrs_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#rev_hrs_temp$`10/2023` <- rowMeans(rev_hrs_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#rev_hrs_temp$`11/2023` <- rowMeans(rev_hrs_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
rev_hrs_temp$`12/2023` <- rowMeans(rev_hrs_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))

rev_hrs_temp <- rev_hrs_temp %>%
  select(-FIRST_MONTH:-LAST_MONTH) %>%
  pivot_longer(
    .,
    cols = `12/2023`, # Update range of forecasted months to pivot as necessary
    names_to = c("month", "year"),
    values_to = "vrh",
    names_sep = "/",
    names_transform = as.integer
  )

rev_hrs <-
  rbind(rev_hrs, rev_hrs_temp) %>% 
  arrange(agency, mode, year)

# Ridership (see above for documentation) -----------------------------
ridership_temp <- ridership %>%
  filter(year == PROJECTION_YEAR) %>%
  pivot_wider(
    .,
    names_from = c(month, year),
    values_from = ridership,
    names_sep = "/"
  ) %>%
  ungroup()

#ridership_temp$`6/2023` <- rowMeans(ridership_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#ridership_temp$`7/2023` <- rowMeans(ridership_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#ridership_temp$`8/2023` <- rowMeans(ridership_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#ridership_temp$`9/2023` <- rowMeans(ridership_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#ridership_temp$`10/2023` <- rowMeans(ridership_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
#ridership_temp$`11/2023` <- rowMeans(ridership_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))
ridership_temp$`12/2023` <- rowMeans(ridership_temp %>% select(ends_with(as.character(PROJECTION_YEAR))))

ridership_temp <- ridership_temp %>%
  select(-FIRST_MONTH:-LAST_MONTH) %>%
  pivot_longer(
    .,
    cols = `12/2023`, # Update range of forecasted months to pivot as necessary
    names_to = c("month", "year"),
    values_to = "ridership",
    names_sep = "/",
    names_transform = as.integer
  )

ridership <-
  rbind(ridership, ridership_temp) %>% 
  arrange(agency, mode, year)

# remove temp variables
rm(
  rev_miles_temp,
  rev_hrs_temp,
  ridership_temp,
  FIRST_MONTH,
  LAST_MONTH,
  PROJECTION_YEAR,
  pace_vrh_nov2023,
  rideon_vrh_nov2023
)