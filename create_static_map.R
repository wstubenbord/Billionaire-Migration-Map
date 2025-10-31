# Author: Wesley Stubenbord
# Description: Produces a static visualization of global migration flows from 
# birth to last-known residence (2010-2025) for a subset of billionaires in PDF 
# format.

library(tidyverse) 
library(sf) 
library(geosphere) 
library(rnaturalearth) 
library(here)

# ---- Load data ---- 
df <- read_csv(here("data","migration_map.csv"))


# ---- Flow and city aggregates ---- 
# Calculate migration flows, exclude stationary moves
flows <- df %>% 
  count(birth, res, birth.lat, birth.long, res.lat, res.long, name = "n") %>% 
  filter(birth != res) 

#Calculate totals by residence and birth
cities_by_res <- df %>% 
  count(res, res.lat, res.long, name = "n") %>%
  rename(lat = "res.lat", long = "res.long", city = "res") %>%
  mutate(type = "Residence")

cities_by_birth <- df %>% 
  count(birth, birth.lat, birth.long, name = "n") %>%
  rename(lat = "birth.lat", long = "birth.long", city = "birth") %>%
  anti_join(cities_by_res, by = c('city')) %>%  # Remove cities which are also residence places
  mutate(type = "Birthplace")

cities <- bind_rows(cities_by_res, 
                    cities_by_birth)

rm(cities_by_birth, cities_by_res)


# ---- Create arcs for migration flow ---- 
make_gc <- function(lon1, lat1, lon2, lat2, n = 75) { 
  obj <- gcIntermediate(c(lon1, lat1), c(lon2, lat2), 
                        n = n, addStartEnd = TRUE, breakAtDateLine = TRUE) 
  if (is.list(obj)) {st_multilinestring(lapply(obj, as.matrix))} 
  else {st_linestring(as.matrix(obj))} 
}

sf_arcs <- flows %>% 
  rowwise() %>% 
  mutate(geometry = list(make_gc(birth.long, birth.lat, res.long, res.lat))) %>% 
  ungroup() %>% 
  st_as_sf(crs = 4326) 


# ---- Basemap ---- 
# Set globe projection as Robinson projection 
crs_proj <- "ESRI:54030" 

land <- ne_countries(scale = 50, returnclass = "sf") 
grat <- st_graticule(lat = seq(-80, 80, 20), lon = seq(-180, 180, 20)) 

# Convert coordinates for Robinson projection 
land_p <- st_transform(land, crs_proj) 
grat_p <- st_transform(grat, crs_proj) 
cities_p <- st_as_sf(cities, coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs_proj) %>%
  arrange(factor(type, levels = c("Birthplace", "Residence")))  
arcs_p <- st_transform(sf_arcs, crs_proj)


# ---- Set plot colors ---- 
col_birth <- "#ef4444" 
col_res <- "#2563eb" 
col_arc <- "#3b82f6" 
col_land <- "#f8fafc" 
col_ocean <- "#eef3f8" 
col_border <- "#9ca3af" 
col_grat <- "#d9e2ec" 


# ---- Plot ---- 
p <- ggplot() + 
  geom_sf(data = land_p, fill = col_land, color = col_border, linewidth = 0.2, 
          show.legend = FALSE) + 
  geom_sf(data = grat_p, color = col_grat, linewidth = 0.6, alpha = 0.6, 
          show.legend = FALSE) + 
  geom_sf(data = arcs_p, aes(color = "Migration path", linewidth = n * 0.2), 
          alpha = 0.3, show.legend = FALSE) + 
  scale_linewidth_identity() + 
  geom_sf(data = cities_p, aes(color = type, size = n), alpha = 0.75) + 
  scale_size_area(max_size = 8.5,
                  name = "Billionaires",
                  breaks = c(125, 75, 25),
                  labels = c("125     ", "75      ", "25      ")) +  # An unfortunate work-around to label justifications
  scale_color_manual(name = "Location", 
                     values = c("Birthplace" = col_birth, 
                                "Residence" = col_res, 
                                "Migration path" = col_arc), 
                     breaks = c("Birthplace", "Residence")) +
  coord_sf(crs = crs_proj, expand = FALSE, clip = "on") +
  guides(color = guide_legend(override.aes = c(size = 5)),
         size = guide_legend(override.aes = c(shape = 21),
                             keywidth = unit(0.613, 'in'),
                             label.hjust = 0,
                             label.position = "right")) +
  theme(panel.background = element_rect(fill = col_ocean, color = NA), 
        plot.background = element_rect(fill = "white", color = NA), 
        panel.grid = element_blank(), 
        legend.position = c(0.01, 0.01),
        legend.background = element_rect(fill = col_land, color = "grey70", 
                                         linewidth = 0.4), 
        legend.box.background = element_rect(color = "grey60", linewidth = 0.5, 
                                             fill = col_land),
        legend.key = element_rect(fill = col_land, color = NA), 
        legend.title = element_text(size = 18, hjust = 0.5, 
                                    margin = margin(b = 4.5), family = "Helvetica"), 
        legend.text = element_text(size = 16, family = "Helvetica"),
        legend.direction = "vertical", 
        legend.key.height = unit(25, "pt"),
        legend.spacing = unit(0, "pt"),
        legend.justification = c("left", "bottom"), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        plot.margin = margin(0, 0, 0, 0)) 


# ---- Save ---- 
ggsave(here("figures", "figure1.pdf"), 
       plot = p, 
       device = cairo_pdf, 
       width = 16, 
       height = 8, 
       units = "in")
