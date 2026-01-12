library(openxlsx)
library(dplyr)
library(sf)
library(osmdata)
library(osmactive)


source("../stats19_stats/R/get.R")

# import LSOA geometry (not simplified)
lsoa_geo_gb = st_read("data/LSOA_IMD2025_OSGB1936_-8580071282870403721.gpkg") |> 
  select(lsoa21_code = LSOA21CD,lsoa21_name = LSOA21NM,geometry = SHAPE) |> 
  st_transform(4326)

# create a geo referenced point
area <- st_point(c(-2.285,51.35)) |> 
  st_sfc(crs = 4326) |> 
  st_buffer(3000)

# bb coordinates
area_bb <- st_bbox(area)

##download road data from osm
x <- opq(bbox = area_bb) %>%
  add_osm_feature(key = c('highway')) %>%
  osmdata_sf()

##extract rd data and trim to the main roads
osm_drive = osmactive::get_driving_network(x$osm_lines) |> 
  select(osm_id,name,ref,maxspeed, highway) |> 
  filter(ref %in% c("B3108")) |> 
  mutate(ID = paste0("L",sprintf("%02d",seq(1:n()))))

osm_buff = osm_drive |> 
  st_transform(27700) |> 
  st_union() |> 
  st_buffer(100) |> 
st_transform(4326)


lsoa_rd <- st_intersection(lsoa_geo_gb,osm_buff)

lsoa_area = lsoa_geo_gb |> 
  filter(lsoa21_code %in% lsoa_rd$lsoa21_code)

b_area_bb <- st_bbox(lsoa_area)

##download landuse from osm
x <- opq(bbox = b_area_bb) %>%
  add_osm_feature(key = c('building')) %>%
  osmdata_sf()

buildings_raw = x$osm_polygons

buildings = buildings_raw |> 
  select(osm_id, amenity,building) |> 
  filter(amenity %in% c(NA,"nursing_home", "student_accommodation")) |> 
  filter(building %in% c("residential","hall_of_residence","semidetached_house","terrace","apartments","house","detached","farm","dormitory","bungalow",NA,"assisted_living","yes"))


# lsoa_rd_area = lsoa_rd |> 
#   mutate(area_100m = as.numeric(st_area(geometry))) |>
#   left_join(lsoa_area, by = "lsoa21_code") |> 
#   mutate(area_frac = area_100m/area)


# import LSOA population for GB
gb_pop <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2022revisednov2025tomid2024/sapelsoasyoa20222024.xlsx",
                    sheet = "Mid-2024 LSOA 2021",startRow = 4)|> 
  select(LSOA.2021.Code, Total)

# join to LSOA areas of interest
lsoa_area_pop = lsoa_area |> 
  left_join(gb_pop, by = c("lsoa21_code" = "LSOA.2021.Code"))

buildings_lsoa = buildings[lsoa_area,] |> 
  mutate(building_area = as.numeric(st_area(geometry))) |> 
  st_join(lsoa_area_pop) |> 
  group_by(lsoa21_code) |> 
  mutate(building_frac = building_area/sum(building_area)) |> 
  mutate(building_pop = Total*building_frac) |> 
  mutate(buildings_per_lsoa = n()) |> 
  mutate(building_pop2 = Total/buildings_per_lsoa)

all_rds_union = st_union(all_rds)

rd_buff_L = all_rds |> 
  st_transform(3857) |> 
  st_buffer(100,endCapStyle = "FLAT",singleSide = TRUE) |> 
  st_transform(4326) |> 
  mutate(ID_2 = paste0(ID,"_L"))

rd_buff_R = all_rds |> 
  st_transform(3857) |> 
  st_buffer(-100,endCapStyle = "FLAT",singleSide = TRUE) |> 
  st_transform(4326) |> 
  mutate(ID_2 = paste0(ID,"_R"))

rd_buff_both = rbind(rd_buff_L, rd_buff_R) |> 
  select(ID_2) 



buildings_roadside = buildings_lsoa |> 
  st_join(rd_buff_both) |> 
  st_set_geometry(NULL) |> 
  group_by(ID_2) |> 
  summarise(total_pop = sum(building_pop)) |> 
  left_join(rd_buff_both, by = "ID_2") |> 
  filter(!is.na(ID_2))

st_geometry(buildings_roadside) = buildings_roadside$x

sum(buildings_roadside$total_pop,na.rm = TRUE)


mapview(buildings_roadside['total_pop'])

for (p in rd_buff$ID){
  sf_split = rd_buff |> 
    filter(ID == p) |> 
    st_split(all_rds_union)
  
}

buf <- st_difference(rd_buff, st_buffer(st_startpoint(all_rds), 100))

mapview(rd_buff)  

lsoa_rd_pop = osm_buff |>
  
  st_join(buildings_lsoa)

mapview(buildings_lsoa['building_pop'])

# create scale
pop_bks <- c(0,1,2,4,8,20,40,80, 100,200,500, 1000, 2000,4000, 8000, 20000, 40000, 100000)
#pop_bks <- c(0, 200, 800, 2000, 50000, 100000)

## define palette for population density
pop_pal <- c("#6e9085", "#c7cdb7", "#b8a69a", "#d2b198", "#f69379", "#af2a3b")
pop_pal <- cols4all::c4a("kovesi.rainbow_bgyrm_35_85_c71", n = 20)[4:20]

# get background map
bg_large <- basemaps::basemap_raster(area, map_service = "carto",increase_zoom =2, map_type = "light")

tm_lsoa_pop <- tm_shape(bg_large)+
  tm_rgb()+
  tm_shape(lsoa_area_pop) +
  tm_polygons(fill = "Total",fill_alpha = 0.4,
              fill.scale = tm_scale_intervals(values = "tol.rainbow_wh_br", style = "pretty", n = 8),
              fill.legend = tm_legend("population", frame = FALSE,legend.border.col = NA),
              lwd = 1)+
  tm_shape(all_rds)+
  tm_lines(col = "yellow")+
  tm_title("LSOA population mid 2025",size = 2)+
  tm_layout(frame = FALSE)+
  tm_credits("Source: ONS")

tmap_save(tm_lsoa_pop, "plots/lsoa_pop.png")

# get background map
bg <- basemaps::basemap_raster(osm_buff, map_service = "carto",increase_zoom =2, map_type = "light")

tm_lsoa_pop <- tm_shape(bg)+
  tm_rgb()+
  tm_shape(buildings_roadside) +
  tm_polygons(fill = "total_pop",fill_alpha = 1,
              fill.scale = tm_scale_intervals(values = "tol.rainbow_wh_br", style = "pretty",n = 8),
              fill.legend = tm_legend("population", frame = FALSE,legend.border.col = NA),
              lwd = 0.5)+
  tm_title("Estimated population within 100m of B3108, mid 2025",size = 1)+
  tm_layout(frame = FALSE)

tmap_save(tm_lsoa_pop, "plots/lsoa_pop_roadside.png")

top_pop_100m = sum(buildings_roadside$total_pop, na.rm = TRUE)

# create scale
pop_bks <- c(0,1,2,4,8,20,40,80, 100,200,500, 1000, 2000,4000, 8000, 20000, 40000, 100000)
#pop_bks <- c(0, 200, 800, 2000, 50000, 100000)

## define palette for population density
pop_pal <- c("#6e9085", "#c7cdb7", "#b8a69a", "#d2b198", "#f69379", "#af2a3b")
pop_pal <- cols4all::c4a("kovesi.rainbow_bgyrm_35_85_c71", n = 20)[4:20]
r_b2 <- subset(r_b,1:3)
yrz <- seq(2000,2024)
## create plot using tmap
tm_ws <- tm_shape(r_b) +
  tm_raster(col.scale = tm_scale_intervals(values = pop_pal, breaks = pop_bks), col.legend = tm_legend(title = "persons per km\u00B2"))+
  tm_legend(position = c(0.85,0.988), frame = FALSE)+
  tm_layout(bg.color = "lightskyblue1", frame = FALSE,title.position = c(0,0.98),panel.show = FALSE)+
  tm_title(text = yrz, size = 4)+
  tm_facets(nrow = 1, ncol = 1)+
  tm_credits("Source: Landscan Global, Oak Ridge National Laboratory. https://doi.org/10.48690/1532445")


