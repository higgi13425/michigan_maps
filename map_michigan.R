# Load required libraries
library(tidycensus)
library(mapboxapi)
library(mapgl)
library(sf)
library(dplyr)
library(RColorBrewer)
library(tidyverse)


# Set your Census API key
# census_api_key("YOUR_API_KEY_HERE", install = TRUE)

# Set your Mapbox access token
mb_access_token("YOUR_MAPBOX_ACCESS_TOKEN_HERE", install = TRUE)

# Get population data by county for Michigan
michigan_population <- get_acs(
  geography = "county",
  variables = "B01003_001", # Total population variable
  state = "MI",
  year = 2022, # Most recent available year
  survey = "acs5",
  geometry = TRUE, # Include geographic boundaries
  cache_table = TRUE
)

# Clean and prepare the data
michigan_population <- michigan_population %>%
  rename(population = estimate) %>%
  mutate(
    county_name = gsub(" County, Michigan", "", NAME),
    # Create population categories for better visualization
    pop_category = case_when(
      population < 50000 ~ "< 50K",
      population < 100000 ~ "50K - 100K",
      population < 250000 ~ "100K - 250K",
      population < 500000 ~ "250K - 500K",
      population >= 500000 ~ "500K+"
    ),
    pop_category = factor(pop_category,
      levels = c(
        "< 50K", "50K - 100K", "100K - 250K",
        "250K - 500K", "500K+"
      )
    )
  ) %>%
  filter(!is.na(population))

# Transform to appropriate CRS for Michigan
michigan_population <- st_transform(michigan_population, crs = 3857)

# Create choropleth map using mapbox

plot(michigan_population["population"])

ggplot(data = michigan_population, aes(fill = population)) +
  geom_sf() +
  scale_fill_distiller(
    palette = "RdPu",
    direction = 1
  ) +
  labs(
    title = "Michigan Population by County, 2022",
    caption = "Data source: 2022 1-year ACS, US Census Bureau",
    fill = "ACS estimate"
  ) +
  theme_void()


# linked maps
library(tidycensus)
library(ggiraph)
library(tidyverse)
library(patchwork)
library(scales)

mi_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "MI",
  year = 2022,
  geometry = TRUE
) %>%
  mutate(NAME = str_remove(NAME, " County, Michigan"))

mi_map <- ggplot(mi_income, aes(fill = estimate)) +
  geom_sf_interactive(aes(data_id = GEOID, tooltip = NAME)) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    guide = "none"
  ) +
  theme_void()

mi_plot <- ggplot(mi_income, aes(
  x = estimate, y = reorder(NAME, estimate),
  fill = estimate
)) +
  geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point_interactive(
    color = "black", size = 4, shape = 21,
    aes(data_id = GEOID)
  ) +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    labels = label_dollar()
  ) +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    title = "Household income by county in Michigan",
    subtitle = "2016-2020 American Community Survey",
    y = "",
    x = "ACS estimate (bars represent margin of error)",
    fill = "ACS estimate"
  ) +
  theme_linedraw(base_size = 8)

girafe(ggobj = mi_map + mi_plot, width_svg = 12, height_svg = 9) %>%
  girafe_options(opts_hover(css = "fill:red;"))
