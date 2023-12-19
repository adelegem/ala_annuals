#-------------------------------------------#
# Title: {galah} intro & quick maps
# Date: 19 October, 2023
#-------------------------------------------#

# load packages
library(tidyverse)
library(galah)


# TIP: This is a base R pipe: |>
# It is almost exactly the same as the tidyverse pipe: %>%
# Pipes can be read literally as "and then"


# Search for taxa
search_taxa("reptilia", "mammalia", "Perameles nasuta")

# See all fields
show_all(fields) |> View()

# Find a field
search_all(fields, "australian states")

search_all(fields, "continent")


# See what values are within a given field
search_all(fields, "cl22") |>
  show_values()

search_all(fields, "continent") |>
  show_values()


## Get data

# Get counts of bandicoots each year since 2011
galah_call() |> 
  galah_identify("perameles") |>
  galah_filter(year > 2010) |>
  galah_group_by(year, cl22) |>
  atlas_counts()
  
# Get counts of bandicoots only in New South Wales
galah_call() |> 
  galah_identify("perameles") |>
  galah_filter(year > 2010,
               cl22 == "New South Wales") |>
  galah_group_by(year) |>
  atlas_counts()

# Get counts by species
galah_call() |> 
  galah_identify("perameles") |>
  galah_group_by(species) |>
  atlas_counts()



# MAP IT
library(sf)
library(ozmaps)

galah_config(email = "dax.kellie@csiro.au")

bandicoots <- galah_call() |>
  galah_identify("perameles") |>
  galah_filter(year == 2020) |>
  galah_apply_profile(ALA) |> # A set of data quality filters to remove spurious records
  atlas_occurrences()

bandicoots

# Load australia map

aus <- ozmaps::ozmap_country |>
  st_transform(crs = st_crs(4326))

# Map
ggplot() +
  geom_sf(data = aus,
          fill = NA,
          colour = "grey60") +
  geom_point(data = bandicoots,
             aes(x = decimalLongitude,
                 y = decimalLatitude),
             colour = "#0f9a32") +
  theme_void()


## TIP: If you are trying to download a lot of occurrences, you can 
#       shrink the amount of data you are downloading by only selecting
#       the columns you need with `galah_select()`. For example:

galah_call() |>
  galah_identify("perameles") |>
  galah_filter(year == 2001) |>
  galah_select(scientificName, decimalLongitude, decimalLatitude) |>
  atlas_occurrences()



# Finding what doesn't match

# Extract species names
galah_names <- galah_call() |>
  galah_identify("perameles") |>
  galah_group_by(scientificName) |>
  atlas_counts() |>
  dplyr::select(scientificName)

galah_names

# Try joining scientific names from austraits with the names in galah
# (you'll need to fill in the correct info when you try to do this :) )
galah_names |>
  dplyr::left_join(austraits_names, by = join_by(col_name_in_galah, col_name_in_austraits))





#just playing with sturts desert pea
library(ozmaps)
library(sf)

galah_config(email = 'adelegemmell@hotmail.com')

sturts_dp <- galah_call() |>
  galah_identify("Swainsona formosa") |>
  galah_apply_profile(ALA) |> # A set of data quality filters to remove spurious records
  atlas_occurrences()

sturts_dp$year <- substr(sturts_dp$eventDate, 1, 4)

sturts_dp$month <- substr(sturts_dp$eventDate, 6, 7)

sturts_dp$month <- month.name[as.numeric(sturts_dp$month)]

aus <- ozmaps::ozmap_country |>
  st_transform(crs = st_crs(4326))

# Map
ggplot() +
  geom_sf(data = aus,
          fill = NA,
          colour = "grey60") +
  geom_point(data = sturts_dp,
             aes(x = decimalLongitude,
                 y = decimalLatitude,
                 color = month)) +
  theme_void()
