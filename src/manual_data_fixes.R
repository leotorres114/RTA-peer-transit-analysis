###################################################################################################
# For RIDE ON only - RIDE ON did not report any VRH for November and December 2023.
# This code manually adds the monthly average for Jan-Oct 2023 to
# November and December 2023.

rideon_vrh_2023_avg <- rev_hrs %>%
  filter(ntdid == 30051 & year == 2023 & month != 11 & month != 12) %>%
  pull(vrh) %>%
  mean()

rev_hrs$vrh[rev_hrs$ntdid == 30051 &
              rev_hrs$month == 11 & 
              rev_hrs$year == 2023] <- rideon_vrh_2023_avg

rev_hrs$vrh[rev_hrs$ntdid == 30051 &
              rev_hrs$month == 12 & 
              rev_hrs$year == 2023] <- rideon_vrh_2023_avg

####################################################################################################