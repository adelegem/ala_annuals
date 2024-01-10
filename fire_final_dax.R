# install.packages("pacman")
pacman::p_load(tidyverse, here, rmapshaper, sf, ggpointdensity, viridis, ozmaps, concaveman, cowplot, patchwork)
#library(maps)
#library(mapdata)
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

# fire %>%
#   filter(StartYear > 2007) %>%
#   group_by(StartYear) %>%
#   summarise(Total_AreaBurnt = sum(AreaHa)) %>%
#   arrange(desc(Total_AreaBurnt))

#create nswv polygon using ozmaps package
sf_oz <- ozmap_data("states") |>
  st_transform(crs = 4326)

nsw <- sf_oz %>%
  filter(NAME == 'New South Wales')

fire <- st_make_valid(fire) # make sure polygons are valid
fire_within_nsw <- st_intersection(fire, nsw) # crop to fire within nsw

# flatten fire shapefile to one single POLYGON layer for mapping
fire_union <- fire_within_nsw |>
  st_combine() |>
  st_cast("POLYGON") |>
  st_union()


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
  galah_select(group = "basic", genus) |>
  galah_apply_profile(ALA) |>
  atlas_occurrences()

fire_ephemerals <- fire_ephemerals %>%
  filter(!is.na(decimalLatitude) | !is.na(decimalLongitude))

# Nest plants by genus
fire_ephemerals_nested <- fire_ephemerals |>
  group_by(genus) |>
  nest()

sf_use_s2(FALSE) # turn off spherical geometry to calculate stats

# Compute concave hulls
concave_nested <- fire_ephemerals_nested %>%
  mutate(points_sf = purrr::map(.x = data,
                         ~ st_as_sf(.x, 
                                    coords = c("decimalLongitude", "decimalLatitude"),
                                    crs = 4326)), 
         concave_sf = purrr::map(points_sf,
                          ~ concaveman(.x)))

concave_nested |> pluck("concave_sf") # 3 polygons for each genus!


# Compute range overlap and descriptive statistics 
overlap_nested <- concave_nested |> 
  mutate(overlap_sf = purrr::map(concave_sf, 
                          possibly(~ st_intersection(fire_union, .x))),
         overlap_area = purrr::map(overlap_sf,
                            possibly(~ st_area(.x))),
         percent_overlap = purrr::map2(.x = overlap_area,
                                .y = concave_sf,
                                possibly(~ (.x / st_area(.y))*100))) |>
  tidyr::unnest(cols = c(overlap_sf, overlap_area, percent_overlap))

# ---

## MAPS

# Set an experimental, fire colour palette (edit this if you want)
my_colours <- c("#FFD700", "#FFA500", "#FF0000")

# Map 1: Australia + fire extent
# NOTE: Maybe it's worth highlighting NSW border?

nsw_border <- ozmap_states %>%
  filter(NAME == "New South Wales")

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
              colour = NA),
          colour = "#FF925C", fill = "#FF925C",
          alpha = 0.8) +
  geom_sf(data = nsw_border,  
          colour = "grey30",     
          fill = NA,          
          size = 4) +
  theme_map() 
#+
# coord_sf(xlim = c(135, 158)) 
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
                 fill = genus,
                 colour = genus,
                 colour = after_scale(colorspace::darken(colour, .7))),
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

fire_ephemerals$year <- substr(fire_ephemerals$eventDate, 1, 4)

#fire ephemeral records by year
obs_year <- fire_ephemerals %>% 
  group_by(genus, year) %>%
  summarise(count = n())

obs_year_recent <- obs_year %>%
  filter(year > 2007)
names <- unique(obs_year_recent$genus)
years <- 2008:2023

obs <- crossing(genus = names,
                year = years)

merged <- merge(obs, obs_year_recent, by = c("genus", "year"), all.x = TRUE)
merged$count[is.na(merged$count)] <- 0

merged_obs <- merged %>%
  group_by(genus) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  ungroup()


fire_palette <- c("#F3D126", "#FFA500", "#FF6347", "#FF4500", "#9A0707")

library(showtext)
font_add_google("Roboto", "Roboto")
font_add_google("Lato", "lato")
showtext_auto()

library(ggdist)
library(ggstream)

dens_plot <- ggplot(merged_obs, 
                    aes(x = year,
                        y = count,
                        fill = genus)) +
  geom_stream(type = 'ridge', alpha = 0.9) +
  geom_vline(xintercept = 2019, linetype ='longdash', col = 'grey55') +
  annotate("text", x = 2019, y = 200, 
           label = "Black Summer \nbushfires begin", 
           fontface = 'bold', hjust = 1.1, size = 7, col = 'grey55', lineheight = 0.5) +
  annotate("curve", x = 2019, xend = 2018,
           y = 150, yend = 175,
           curvature = -.6,
           color = "grey55",
           size = 0.6,
           alpha = 0.8,
           arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  scale_fill_manual(values = my_colours) +
  labs(x = '', y = 'observations') +
  theme_classic() +
  theme(text = element_text(family = 'lato', size = 28)) +
  theme(plot.background = element_rect(fill = "#F7F4EE", color = "#F7F4EE"),
        panel.background = element_rect(fill = "#F7F4EE", color = NA),
        legend.background = element_blank(),
        legend.position = 'none',
        axis.line = element_blank(),  
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "grey95", linetype = "solid"))

dens_plot

##MAKING THE BIG OL' PAGE OF MAPS AND GRAPHS 
font_add_google(name = "Raleway", family = "Raleway")
font_add_google(name = 'Montserrat', family = 'montserrat')
font_add_google(name = 'Work Sans', family = 'Work Sans')
font_add_google(name = 'Lato', family = 'lato')

showtext_auto()

my_colours <- c("#F3CE15", "#E07400", "#AF0808")

library(ggtext)
library(extrafont)
#main = eph_map + text 
main <- ggplot() +
  geom_sf(data = nsw,
          fill = 'white',
          colour = "grey50") +
  geom_sf(data = fire_union,
          colour = NA,
          fill = '#FF5A00',
          alpha = 0.3) +
  geom_point(data = fire_ephemerals,
             aes(x = decimalLongitude,
                 y = decimalLatitude,
                 fill = genus,
                 colour = genus,
                 colour = after_scale(colorspace::darken(colour, .4))),
             alpha= 0.7,
             size = 3,
             pch = 21) +
  # labs(title = 'Fire history and fire ephemeral species in NSW', color = 'Species') +
  scale_fill_manual(values = my_colours) +
  scale_colour_manual(values = my_colours) +
  theme_map() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none"
  ) +
  coord_sf(ylim = c(-24, -39),
            xlim = c(125, 154)) +
  annotate(geom = "text", y = -25.8, x = 136.4, 
           label = "Fire Ephemerals in NSW", 
           hjust = "middle", 
           # family = 'Raleway',  
           family = 'lato',  
           size = 67.8, 
           lineheight = 0.5,
           color = "#222322") +
  ggtext::geom_textbox(aes(x = 133, y = -30.4),
                       label = paste0(
                         "**Fire ephemerals** are plants that germinate after fire events 
                         and are important for shaping post-fire Australian ecosystems. 
                         However, fire ephemerals are elusive plants with short lifespans, 
                         making them difficult to observe. In NSW some of the most recorded 
                         species are in the 
                         <span style=\"color:#E9C40C\">**Actinotus**</span>, 
                         <span style=\"color:#E07400\">**Gyrostemon**</span>, and 
                         <span style=\"color:#9C0707\">**Androcalva**</span> genera. 
                         There are still few observations of these species, posing 
                         difficulties for conservation assessments."),
                       family = 'lato',
                       box.color = NA,
                       color = "#222322",
                       fill = NA,
                       lineheight = 0.6,
                       size = 14,
                       width = unit(25, "lines"),
                       hjust = 0.6) +
  ggtext::geom_textbox(aes (x = 154, y = -39.5),
                       label = "Data visualisation: Adele Gemmell<br>
                       Source: Atlas of Living Australia via the {galah} package<br>",
                       family = "lato",
                       box.color = NA,
                       fill = NA,
                       lineheight = 0.5,
                       size = 8,
                       width = unit(25, 'lines'),
                       hjust = 0.5) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "#F7F4EE", color = NA))


ggsave('maps_graphs/test4.png', width = 12, height = 8, dpi = 320)


nsw$geometry

## COWPLOT TO ADD STREAMGRAPH AND AUS STATES MAP

library(cowplot)

ggdraw(main) +
  draw_plot(aus_fire, 
            x = 0.74, y = 0.68, 
            width = 0.2, height = 0.2) 

ggsave('maps_graphs/test5.png', width = 12, height = 8, dpi = 320)

ggdraw(main) +
  draw_plot(aus_fire, 
            x = 0.75, y = 0.69, 
            width = 0.2, height = 0.2) +
  draw_plot(dens_plot,
            x = 0.08, y = 0.1,
            width = 0.43, height = 0.3)

ggsave('maps_graphs/final_fire_ephemerals_dk.png', width = 12, height = 7.33, dpi = 320)
warnings()

