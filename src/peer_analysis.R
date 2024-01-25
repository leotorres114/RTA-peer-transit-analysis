library(tidyverse)
library(here)
library(cmapplot)

# run get_data.R script
source("src/get_data.R")

# Import NTD peer data ---------------------------------------------------------

rev_miles <- read_csv(here("processed_data", "vrm_peers_monthly.csv"))
rev_hrs <- read_csv(here("processed_data", "vrh_peers_monthly.csv"))
ridership <- read_csv(here("processed_data", "ridership_peers_monthly.csv"))

# Clean and re-format for charts -------------------------------------

# Combine directly operated (DO) and purchased transportation (PT) figures
rev_miles <- rev_miles %>%
  group_by(across(c(-type_of_svc,-vrm))) %>%
  summarise(vrm = sum(vrm))

rev_hrs <- rev_hrs %>%
  group_by(across(c(-type_of_svc,-vrh))) %>%
  summarise(vrh = sum(vrh))

ridership <- ridership %>%
  group_by(across(c(-type_of_svc,-ridership))) %>%
  summarise(ridership = sum(ridership))

# projections.R provides a forecast for the remainder of the year
# based on available monthly averages for each metric.

source("src/projections.R")

write_csv(rev_miles,
          here("processed_data", "vrm_peers_monthly_clean.csv"))
write_csv(rev_hrs,
          here("processed_data", "vrh_peers_monthly_clean.csv"))
write_csv(ridership,
          here("processed_data", "ridership_peers_monthly_clean.csv"))

# calculate annual figures for each agency and mode for all years
rev_miles <- rev_miles %>%
  group_by(mode, acronym, year) %>%
  summarise(vrm = sum(vrm)) %>%
  pivot_wider(., names_from = year, values_from = vrm) %>%
  rename(agency = acronym)

rev_hrs <- rev_hrs %>%
  group_by(mode, acronym, year) %>%
  summarise(vrh = sum(vrh)) %>%
  pivot_wider(., names_from = year, values_from = vrh) %>%
  rename(agency = acronym)

ridership <- ridership %>%
  group_by(mode, acronym, year) %>%
  summarise(ridership = sum(ridership)) %>%
  pivot_wider(., names_from = year, values_from = ridership) %>%
  rename(agency = acronym)

write_csv(rev_miles, 
          here("processed_data", "vrm_peers_annual.csv"))
write_csv(rev_hrs, 
          here("processed_data", "vrh_peers_annual.csv"))
write_csv(ridership,
          here("processed_data", "ridership_peers_annual.csv"))

# Filter for 2019 - 2023
rev_miles <- rev_miles %>%
  select(mode, agency, `2019`:`2023`)

rev_hrs <- rev_hrs %>%
  select(mode, agency, `2019`:`2023`)

ridership <- ridership %>%
  select(mode, agency, `2019`:`2023`)

# calculate share of 2019 for each year

rev_miles_precov_share <- rev_miles %>%
  pivot_longer(`2019`:`2023`, names_to = "year", values_to = "rev_miles") %>%
  group_by(mode, agency) %>%
  mutate(share = rev_miles / first(rev_miles)) %>%
  ungroup() %>%
  mutate(across(mode, ~ factor(
    .,
    levels = c("urban_rail", "urban_bus", "commuter_rail", "suburban_bus")
  )))

rev_hrs_precov_share <- rev_hrs %>%
  pivot_longer(`2019`:`2023`, names_to = "year", values_to = "rev_hrs") %>%
  group_by(mode, agency) %>%
  mutate(share = rev_hrs / first(rev_hrs)) %>%
  ungroup() %>%
  mutate(across(mode, ~ factor(
    .,
    levels = c("urban_rail", "urban_bus", "commuter_rail", "suburban_bus")
  )))

ridership_precov_share <- ridership %>%
  pivot_longer(`2019`:`2023`, names_to = "year", values_to = "ridership") %>%
  group_by(mode, agency) %>%
  mutate(share = ridership / first(ridership)) %>%
  ungroup() %>%
  mutate(across(mode, ~ factor(
    .,
    levels = c("urban_rail", "urban_bus", "commuter_rail", "suburban_bus")
  )))

# Helper vectors

modes <- c(
  urban_rail = "Urban Rail",
  urban_bus = "Urban Bus",
  commuter_rail = "Commuter Rail",
  suburban_bus = "Suburban Bus"
)

modes_loop <- c("urban_rail",
                "urban_bus",
                "commuter_rail",
                "suburban_bus")

# Peer summary charts ----------------------------------------------------

# revenue miles
rev_miles_summary <- rev_miles_precov_share %>%
  ggplot(aes(
    x = year,
    y = share,
    group = agency,
    color = agency
  )) +
  geom_line(linewidth = 1.5) +
  labs(y = "Share of 2019 revenue miles",
       x = "Year") +
  gghighlight::gghighlight(
    agency == "CTA" | agency == "Metra" | agency == "Pace",
    calculate_per_facet = TRUE,
    use_direct_label = FALSE,
    use_group_by = FALSE
  ) +
  theme_cmap(
    panel.grid.major.x = element_blank(),
    gridlines = "h",
    xlab = "Year",
    ylab = "Share of 2019 revenue miles"
  ) +
  scale_color_manual(values = c(
    CTA = '#008fd5',
    Metra = '#6dae4f',
    Pace = '#d3b42b'
  )) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  theme(legend.position = "none",
        axis.title = element_text(size = 10)) +
  geom_text_lastonly(
    mapping = aes(label = scales::percent(share, accuracy = 1)),
    add_points = TRUE,
    text_aes = list(size = 2.75, family = "Whitney")
  ) +
  coord_cartesian(clip = "off") +
  facet_wrap( ~ mode,
              labeller = as_labeller(modes))

finalize_plot(
  rev_miles_summary,
  title = 'Revenue miles as a share of 2019 on <span style="color:#008fd5;">CTA</span>, <span style="color:#6dae4f;">Metra</span> and <span style="color:#d3b42b;">Pace</span> relative to peer systems',
  caption = paste0(
    "Peer systems include:<br><br>
              <b>Urban rail</b>: NYC MTA, MBTA, WMATA, SEPTA, and MARTA
              <b>Urban bus</b>: NYC MTA, MBTA, WMATA, SEPTA, and LA Metro
              <b>Commuter rail</b>: MBTA, Metro North, Long Island Railroad, NJ Transit, SEPTA
              <br><b>Suburban bus</b>: AC Transit, Broward County Transit, OCTA, RIDE ON Montgomery, VTA
              <br><br> 2023 forecast is derived from average monthly figures between January and November.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning
              analysis of the National Transit Database."
  ),
  filename = here("charts", "rev_miles_summary"),
  mode = "png",
  overwrite = T
)

#revenue hours
rev_hrs_summary <- rev_hrs_precov_share %>%
  ggplot(aes(
    x = year,
    y = share,
    group = agency,
    color = agency
  )) +
  geom_line(linewidth = 1.5) +
  gghighlight::gghighlight(
    agency == "CTA" | agency == "Metra" | agency == "Pace",
    calculate_per_facet = TRUE,
    use_direct_label = FALSE,
    use_group_by = FALSE
  ) +
  theme_cmap(
    panel.grid.major.x = element_blank(),
    gridlines = "h",
    xlab = "Year",
    ylab = "Share of 2019 revenue hours"
  ) +
  scale_color_manual(values = c(
    CTA = '#008fd5',
    Metra = '#6dae4f',
    Pace = '#d3b42b'
  )) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  theme(legend.position = "none",
        axis.title = element_text(size = 10)) +
  geom_text_lastonly(
    mapping = aes(label = scales::percent(share, accuracy = 1)),
    add_points = TRUE,
    text_aes = list(size = 2.75, family = "Whitney")
  ) +
  coord_cartesian(clip = "off") +
  facet_wrap( ~ mode,
              labeller = as_labeller(modes))

finalize_plot(
  rev_hrs_summary,
  title = 'Revenue hours as a share of 2019 on <span style="color:#008fd5;">CTA</span>, <span style="color:#6dae4f;">Metra</span> and <span style="color:#d3b42b;">Pace</span> relative to peer systems',
  caption = "Peer systems include:<br><br>
              <b>Urban rail</b>: NYC MTA, MBTA, WMATA, SEPTA, and MARTA
              <b>Urban bus</b>: NYC MTA, MBTA, WMATA, SEPTA, and LA Metro
              <b>Commuter rail</b>: MBTA, Metro North, Long Island Railroad, NJ Transit, SEPTA
              <br><b>Suburban bus</b>: AC Transit, Broward County Transit, OCTA, RIDE ON Montgomery, VTA
              <br><br> 2023 forecast is derived from average monthly figures between January and November.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning
              analysis of the National Transit Database.",
  filename = here("charts", "rev_hrs_summary"),
  mode = "png",
  overwrite = T
)

# ridership
ridership_summary <- ridership_precov_share %>%
  ggplot(aes(
    x = year,
    y = share,
    group = agency,
    color = agency
  )) +
  geom_line(linewidth = 1.5) +
  gghighlight::gghighlight(
    agency == "CTA" | agency == "Metra" | agency == "Pace",
    calculate_per_facet = TRUE,
    use_direct_label = FALSE,
    use_group_by = FALSE
  ) +
  theme_cmap(
    panel.grid.major.x = element_blank(),
    gridlines = "h",
    xlab = "Year",
    ylab = "Share of 2019 ridership"
  ) +
  scale_color_manual(values = c(
    CTA = '#008fd5',
    Metra = '#6dae4f',
    Pace = '#d3b42b'
  )) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  theme(legend.position = "none",
        axis.title = element_text(size = 10)) +
  geom_text_lastonly(
    mapping = aes(label = scales::percent(share, accuracy = 1)),
    add_points = TRUE,
    text_aes = list(size = 2.75, family = "Whitney")
  ) +
  coord_cartesian(clip = "off") +
  facet_wrap( ~ mode,
              labeller = as_labeller(modes))

finalize_plot(
  ridership_summary,
  title = 'Ridership as a share of 2019 on <span style="color:#008fd5;">CTA</span>, <span style="color:#6dae4f;">Metra</span> and <span style="color:#d3b42b;">Pace</span> relative to peer systems',
  caption = "Peer systems include:<br><br>
              <b>Urban rail</b>: NYC MTA, MBTA, WMATA, SEPTA, and MARTA
              <b>Urban bus</b>: NYC MTA, MBTA, WMATA, SEPTA, and LA Metro
              <b>Commuter rail</b>: MBTA, Metro North, Long Island Railroad, NJ Transit, SEPTA
              <br><b>Suburban bus</b>: AC Transit, Broward County Transit, OCTA, RIDE ON Montgomery, VTA
              <br><br> 2023 forecast is derived from average monthly figures between January and November.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning
              analysis of the National Transit Database.",
  filename = here("charts", "ridership_summary"),
  mode = "png",
  overwrite = T
)

# Revenue hours by mode - detailed ---------------------------------------------------
for (i in modes_loop) {
  temp_plot <- rev_hrs_precov_share %>% filter(mode == i) %>%
    ggplot(aes(
      x = year,
      y = share,
      group = agency,
      color = agency
    )) +
    geom_line(linewidth = 1.5) +
    theme_cmap(
      panel.grid.major.x = element_blank(),
      gridlines = "h",
      xlab = "Year",
      ylab = "Share of 2019 revenue hours"
    ) +
    scale_color_manual(values = c(
      "#008fd5",
      "#1e478e",
      "#6dae4f",
      "#d3b42b",
      "#1d0d33",
      "#eb6a60"
    )) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(.60, 1.1, by = .1)
    ) +
    scale_x_discrete(expand = expansion(add = c(0.10, .8))) +
    geom_text_lastonly(
      mapping = aes(label = paste0(
        agency, " , ", scales::percent(share, accuracy = 1)
      )),
      add_points = TRUE,
      text_aes = list(size = 2.75, family = "Whitney")
    ) +
    coord_cartesian(clip = "off") +
    theme(legend.position = "none")
  
  finalize_plot(
    temp_plot,
    title = paste0(
      'Revenue hours as a share of 2019 on RTA and peer systems for ',
      modes[i]
    ),
    caption = "2023 forecast is derived from average monthly figures between January and November.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning
              analysis of the National Transit Database.",
    filename = here("charts", paste0(i, "_rev_hrs")),
    mode = "png",
    overwrite = T
  )
}

# Revenue miles by mode - detailed ---------------------------------------------------
for (i in modes_loop) {
  temp_plot <- rev_miles_precov_share %>% filter(mode == i) %>%
    ggplot(aes(
      x = year,
      y = share,
      group = agency,
      color = agency
    )) +
    geom_line(linewidth = 1.5) +
    theme_cmap(
      panel.grid.major.x = element_blank(),
      gridlines = "h",
      xlab = "Year",
      ylab = "Share of 2019 revenue miles"
    ) +
    scale_color_manual(values = c(
      "#008fd5",
      "#1e478e",
      "#6dae4f",
      "#d3b42b",
      "#1d0d33",
      "#eb6a60"
    )) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(.60, 1.1, by = .1)
    ) +
    scale_x_discrete(expand = expansion(add = c(0.10, .8))) +
    geom_text_lastonly(
      mapping = aes(label = paste0(
        agency, " , ", scales::percent(share, accuracy = 1)
      )),
      add_points = TRUE,
      text_aes = list(size = 2.75, family = "Whitney")
    ) +
    coord_cartesian(clip = "off") +
    theme(legend.position = "none")
  
  finalize_plot(
    temp_plot,
    title = paste0(
      'Revenue miles as a share of 2019 on RTA and peer systems for ',
      modes[i]
    ),
    caption = "2023 forecast is derived from average monthly figures between January and November.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning
              analysis of the National Transit Database.",
    filename = here("charts", paste0(i, "_rev_miles")),
    mode = "png",
    overwrite = T
  )
}

# Ridership by mode - detailed -------------------------------------------------------
for (i in modes_loop) {
  temp_plot <- ridership_precov_share %>% filter(mode == i) %>%
    ggplot(aes(
      x = year,
      y = share,
      group = agency,
      color = agency
    )) +
    geom_line(linewidth = 1.5) +
    theme_cmap(
      panel.grid.major.x = element_blank(),
      gridlines = "h",
      xlab = "Year",
      ylab = "Share of 2019 ridership"
    ) +
    scale_color_manual(values = c(
      "#008fd5",
      "#1e478e",
      "#6dae4f",
      "#d3b42b",
      "#1d0d33",
      "#eb6a60"
    )) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                       breaks = seq(.20, 1, by = .2)) +
    scale_x_discrete(expand = expansion(add = c(0.10, .8))) +
    geom_text_lastonly(
      mapping = aes(label = paste0(
        agency, " , ", scales::percent(share, accuracy = 1)
      )),
      add_points = TRUE,
      text_aes = list(size = 2.75, family = "Whitney")
    ) +
    coord_cartesian(clip = "off") +
    theme(legend.position = "none")
  
  finalize_plot(
    temp_plot,
    title = paste0('Ridership as a share of 2019 on RTA and peer systems for ', modes[i]),
    caption = "2023 forecast is derived from average monthly figures between January and November.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning
              analysis of the National Transit Database.",
    filename = here("charts", paste0(i, "_ridership")),
    mode = "png",
    overwrite = T
  )
}