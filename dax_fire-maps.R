# Dax suggestions for fire ephemeral plot

# packages
library(maps)
library(mapdata)
library(tidyverse)
library(sf)
library(extrafont)
library(here)
library(ozmaps)
library(ggthemes)
library(galah)

# ---

## Shapes

# add nsw map
sf_oz <- ozmap_data("states") |>
  st_transform(crs = 4326)

nsw <- sf_oz %>%
  filter(NAME == 'New South Wales')


# load fire dataset
fire <- st_read(here("NPWS_fire", "NPWSFireHistory.shp")) |> 
  st_transform(crs = 4326) |> 
  st_zm()  # Remove Z or M values

fire <- st_make_valid(fire) # make sure polygons are valid
fire_within_nsw <- st_intersection(fire, nsw) # crop to fire within nsw

# flatten fire shapefile to one single POLYGON layer for mapping
fire_union <- fire_within_nsw |>
  st_combine() |>
  st_cast("POLYGON") |>
  st_union()


# ---

## Species observations

# top counts of ephemeral species observations by genus in NSW
galah_call() |>
  galah_identify('Actinotus forsythii', 
                 'Monotaxis macrophylla', 
                 'Gyrostemon australasicus', 
                 'Gyrostemon thesioides', 
                 'Androcalva procumbens', 
                 'Androcalva rosea', 
                 'Commersonia rugosa') |>
  galah_filter(cl22 == 'New South Wales') |>
  galah_apply_profile(ALA) |> #applies data quality checks
  galah_group_by(genus) |>
  atlas_counts()

#  So we will remove Monotaxis and Commersonia (sadly. They are lovely looking plants)


# download data
galah_config(email = 'adelegemmell@hotmail.com')

fire_ephemerals <- galah_call() |>
  galah_identify('Actinotus forsythii', 
                 # 'Monotaxis macrophylla', 
                 'Gyrostemon australasicus', 
                 'Gyrostemon thesioides', 
                 'Androcalva procumbens', 
                 'Androcalva rosea' 
                 # 'Commersonia rugosa'
  ) |>
  galah_filter(cl22 == 'New South Wales') |>
  galah_select(group = "basic", countryConservation, stateConservation, genus) |>
  galah_apply_profile(ALA) |>
  atlas_occurrences()


# ---

## Calculate concave hulls to find fire overlap

## NOTE: I figured out a way to do this nested and unnested (ie, lists and no lists)
##       I think this is faster, but if it doesn't work for you, let me know :)

library(concaveman)

# Nest plants by genus
fire_ephemerals_nested <- fire_ephemerals |>
  group_by(genus) |>
  nest()

sf_use_s2(FALSE) # turn off spherical geometry to calculate stats

# Compute concave hulls
concave_nested <- fire_ephemerals_nested %>%
  mutate(points_sf = map(.x = data,
                         ~ st_as_sf(.x, 
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)), 
         concave_sf = map(points_sf,
                          ~ concaveman(.x)))

concave_nested |> pluck("concave_sf") # 3 polygons for each genus!

# Compute range overlap and descriptive statistics 
overlap_nested <- concave_nested |> 
  mutate(overlap_sf = map(concave_sf, 
                          possibly(~ st_intersection(fire_union, .x))),
         overlap_area = map(overlap_sf,
                            possibly(~ st_area(.x))),
         percent_overlap = map2(.x = overlap_area,
                                .y = concave_sf,
                                possibly(~ (.x / st_area(.y))*100))) |>
  tidyr::unnest(cols = c(overlap_sf, overlap_area, percent_overlap))

# ---

## MAPS

# Set an experimental, planty colour palette (edit this if you want)
my_colours <- c("#1D2B12", "#3B7009", "#9E701B")

# Map 1: Eastern Australia + fire extent
# NOTE: Maybe it's worth highlighting NSW border?

ggplot() +
  geom_sf(data = ozmap_states,
          fill = 'white',
          colour = "grey50") +
  geom_sf(data = fire_union,
          colour = NA,
          fill = '#FF925C',
          alpha = 0.4) +
  geom_sf(data = overlap_nested,
        aes(geometry = overlap_sf,
            colour = NA,
            ),
        colour = "#FF925C", fill = "#FF925C",
        alpha = 0.8) +
  theme_map() +
  coord_sf(xlim = c(135, 158)) +
  theme(plot.background = element_rect(fill = "white", color = NA))


# Map 2: NSW with observations (split by genus)
# NOTE: Legend should be removed eventually



ggplot() +
  geom_sf(data = nsw,
          fill = 'white',
          colour = "grey50") +
  geom_sf(data = fire_union,
          colour = NA,
          fill = '#ED8147',
          alpha = 0.4) +
  geom_point(data = fire_ephemerals,
             aes(x = decimalLongitude,
                 y = decimalLatitude,
                 color = genus),
             alpha= 0.7,
             size = 3) +
  # labs(title = 'Fire history and fire ephemeral species in NSW', color = 'Species') +
  scale_color_manual(values = my_colours) +
  theme_map() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.position = "right"
        # legend.position = "none" # replace the above eventually
  )

