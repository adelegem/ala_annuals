install.packages("pacman")
pacman::p_load(tidyverse, here, rmapshaper, sf, ggpointdensity, viridis, ozmaps, concaveman, cowplot, patchwork)
library(maps)
library(mapdata)
library(tidyverse)
library(sf)
library(extrafont)
library(here)
library(ozmaps)
library(ggthemes)
library(galah)

#load the dataset, transforming into WGS84 CRS 
fire <- st_read(here("NPWS_fire", "NPWSFireHistory.shp")) |> 
  st_transform(crs = 4326) |> 
  st_zm()  # Remove Z or M values

#add start year
fire$StartYear <- substr(fire$StartDate, 1, 4)

#Some fires have an arbitary 'start year' of 1899 where the start date is unknown, even if they are labelled as '1996-97 wildfire'. replace start year for these records with the year given in the 'Label' field. The fire dataset begins in 1902

fire <-  fire %>%
  mutate(StartYear = ifelse(is.na(StartYear) | StartYear == '1899', substr(Label, 1, 4), StartYear))


fire <- st_make_valid(fire) # make sure polygons are valid
fire_within_nsw <- st_intersection(fire, nsw) # crop to fire within nsw

# flatten fire shapefile to one single POLYGON layer for mapping
fire_union <- fire_within_nsw |>
  st_combine() |>
  st_cast("POLYGON") |>
  st_union()

#create nswv polygon using ozmaps package
sf_oz <- ozmap_data("states") |>
  st_transform(crs = 4326)

nsw <- sf_oz %>%
  filter(NAME == 'New South Wales')

#configure your email to access atlas occurence data from the galah package
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
my_colours <- c("#d498c9", "#3B7009", "#9E701B")
my_colours <- c("#FFD700", "#FFA500", "#FF0000")
# Map 1: Eastern Australia + fire extent
# NOTE: Maybe it's worth highlighting NSW border?


aus_fire <- ggplot() +
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
  coord_sf(xlim = c(135, 158)) 
#+
#  theme(plot.background = element_rect(fill = "white", color = NA))

aus_fire
# Map 2: NSW with observations (split by genus)
# NOTE: Legend should be removed eventually



eph_map <- ggplot() +
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
                 fill = genus),
             alpha= 0.7,
             size = 3,
             pch = 21) +
  # labs(title = 'Fire history and fire ephemeral species in NSW', color = 'Species') +
  scale_fill_manual(values = my_colours) +
  theme_map() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.position = "right"
        # legend.position = "none" # replace the above eventually
  )
eph_map

#fire ephemeral records by year
obs_year <- fire_ephemerals %>% 
  group_by(scientificName, year) %>%
  summarise(count = n())

obs_year_recent <- obs_year %>%
  filter(year > 1999)
names <- unique(obs_year_recent$scientificName)
years <- 2000:2023

obs <- crossing(scientificName = names,
                year = years)

merged <- merge(obs, obs_year_recent, by = c("scientificName", "year"), all.x = TRUE)
merged$count[is.na(merged$count)] <- 0

merged_obs <- merged %>%
  group_by(scientificName) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  ungroup()

fire_palette <- c("#FF0000", "#FF6600", "#FFCC00", "#FFFF00", "#FFFF99")

fire_palette <- c("#FFD700", "#FFA500", "#FF6347", "#FF4500", "#FF0000")

library(showtext)
font_add_google("Roboto", "Roboto")
showtext_auto()


dens_plot <- ggplot(merged_obs, 
                    aes(x = year,
                        y = count,
                        fill = scientificName)) +
  geom_stream(type = 'ridge', alpha = 0.9) +
  scale_fill_manual(values = fire_palette) +
  labs(x = 'year', y = 'total observations', fill = 'species') +
  theme_classic() +
  theme(text = element_text(family = 'Roboto', size = 25)) +
  theme(plot.background = element_rect(fill = NA, color = NA),
        legend.background = element_blank())

dens_plot


##MAKING THE BIG OL' PAGE OF MAPS AND GRAPHS 
font_add_google(name = "Noto Sans", family = "Noto Sans")
font_add_google(name = 'Montserrat', family = 'montserrat')
font_add_google(name = 'Work Sans', family = 'Work Sans')

showtext_auto()

my_colours <- c("#FFD700", "#FFA500", "#FF0000")
library(ggtext)
library(extrafont)
#main = eph_map + text 
main <- ggplot() +
  geom_sf(data = nsw,
          fill = 'white',
          colour = "grey50") +
  geom_sf(data = fire_union,
          colour = NA,
          fill = '#ED8147',
          alpha = 0.6) +
  geom_point(data = fire_ephemerals,
             aes(x = decimalLongitude,
                 y = decimalLatitude,
                 fill = genus),
             alpha= 0.7,
             size = 3,
             pch = 21) +
  # labs(title = 'Fire history and fire ephemeral species in NSW', color = 'Species') +
  scale_fill_manual(values = my_colours) +
  theme_map() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none"
  ) +
  coord_sf(ylim = c(-24, -41),
            xlim = c(124, 153.6299)) +
  annotate(geom = "text", y = -27, x = 137, 
           label = "Fire Ephemerals in NSW", 
           hjust = "middle", family = 'Work Sans',  size = 45, 
           lineheight = 0.5,
           color = "#65a650") +
  ggtext::geom_textbox(aes(x = 134.25, y = -31.5),
                       label = paste0("Fire ephemerals are elusive species, with short life spans that grow following fire events. In NSW, fire ephemerals include species in the <span style=\"color:#FFD700\">**Actinotus**</span>, <span style=\"color:#FFA500\">**Gyrostemon**</span>, and <span style=\"color:#FF0000\">**Androcalva**</span> genera - but there remain few observations of these species posing difficulties for conservation assessments."),
                       family = 'montserrat',
                       box.color = NA,
                       fill = NA,
                       lineheight = 0.75,
                       size = 12,
                       width = unit(21, "lines"),
                       hjust = 0.5) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

ggsave('maps_graphs/test1.png', width = 12, height = 8, dpi = 320)


## COWPLOT TO ADD STREAMGRAPH AND AUS STATES MAP

library(cowplot)

ggdraw(main) +
  draw_plot(aus_fire, 
            x = 0, y = 0.4, 
            width = 0.25, height = 0.5) 

ggsave('maps_graphs/test2.png', width = 12, height = 8, dpi = 320)

ggdraw(main) +
  draw_plot(aus_fire, 
            x = 0, y = 0.4, 
            width = 0.25, height = 0.5) +
  draw_plot(dens_plot, 
            x = 0.15, y = 0.1,
            width = 0.6, height = 0.3)

ggsave('maps_graphs/test3.png', width = 12, height = 8, dpi = 320)
warnings()
