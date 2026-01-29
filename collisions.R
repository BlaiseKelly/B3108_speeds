library(stats19)
library(sf)
library(dplyr)
library(eurostat)
library(osmdata)
library(osmactive)
library(tmap)

source("../stats19_stats/R/get.R")
source("../stats19_stats/R/plots.R")
source("../stats19_stats/R/summary.R")
source("../stats19_stats/R/tables.R")
source("../stats19_stats/R/tables.")
devtools::load_all("../stats19_match/")

crashes = get_stats19(year = 2004,type = "collision") |> 
  filter(collision_year >= 2010) |>
  format_sf() |> 
  st_transform(4326)

casualties = get_stats19(year = 2004, type = "casualty") |> 
  filter(collision_index %in% crashes$collision_index)

vehicles = get_stats19(year = 2004, type = "vehicle") |> 
  filter(collision_index %in% crashes$collision_index)

# create a geo referenced point
area <- st_point(c(-2.285,51.35)) |>
  st_sfc(crs = 4326) |>
  st_buffer(3000)

area_bb <- st_bbox(area)

##download landuse from osm
x <- opq(bbox = area_bb) %>%
  add_osm_feature(key = c('highway')) %>%
  osmdata_sf()

##extract rd data and trim to the main roads
osm_drive = osmactive::get_driving_network(x$osm_lines) |>
  select(osm_id,name,ref,maxspeed, highway) |>
  filter(ref %in% c("B3108")) |>
  mutate(ID = paste0("L",sprintf("%02d",seq(1:n()))))

# combine roads into more sensible sections
rd_1 = osm_drive |>
  filter(osm_id %in% c("38140958","1408388584","374506648","38324297", "38324838")) |>
  st_union() |>
  st_as_sf() |>
  st_cast("MULTILINESTRING") |>
  st_line_merge()|>
  transmute(maxspeed = 40,
            scenario_A = 40,
            scenario_B = 30,
            scenario_C = 30,
            scenario_D = 20,
            section = "Lower Stoke Junction")

rd_2 = osm_drive |>
  filter(osm_id %in% c("35391257","4305594")) |>
  st_union() |>
  st_as_sf() |>
  st_cast("MULTILINESTRING") |>
  st_line_merge()|>
  transmute(maxspeed = 40,
            scenario_A = 40,
            scenario_B = 30,
            scenario_C = 30,
            scenario_D = 20,
            section = "Lower Stoke 40mph")

rd_3 = osm_drive |>
  filter(osm_id %in% c("35391256","35391259","35391258", "159153552")) |>
  st_union() |>
  st_as_sf() |>
  st_cast("MULTILINESTRING") |>
  st_line_merge() |>
  transmute(maxspeed = 40,
            scenario_A = 40,
            scenario_B = 30,
            scenario_C = 30,
            scenario_D = 20,
            section = "Winsley Hill 40mph")

rd_4 = osm_drive |>
  filter(osm_id %in% c("567182839","567182838","4449841")) |>
  st_union() |>
  st_as_sf() |>
  st_cast("MULTILINESTRING") |>
  st_line_merge() |>
  transmute(maxspeed = 30,
            scenario_A = 30,
            scenario_B = 30,
            scenario_C = 20,
            scenario_D = 20,
            section = "Winsley Road 30mph")

rd_5 = osm_drive |>
  filter(osm_id %in% c("4305512","1389309800","4329918","238677471")) |>
  st_union() |>
  st_as_sf() |>
  st_cast("MULTILINESTRING") |>
  st_line_merge() |>
  transmute(maxspeed = 50,
            scenario_A = 40,
            scenario_B = 30,
            scenario_C = 30,
            scenario_D = 20,
            section = "Winsley bypass 50mph")

rd_6 = osm_drive |>
  filter(osm_id %in% c("1389309908")) |>
  st_union() |>
  st_as_sf() |>
  st_cast("MULTILINESTRING") |>
  st_line_merge() |>
  transmute(maxspeed = 40,
            scenario_A = 40,
            scenario_B = 30,
            scenario_C = 30,
            scenario_D = 20,
            section = "Winsley Road 40mph")


all_rds = rbind(rd_1,rd_2,rd_3,rd_4,rd_5,rd_6) |>
  mutate(X = st_coordinates(st_centroid(x))[,1]) |>
  arrange(X) |>
  mutate(ID = paste0("L",sprintf("%02d",seq(1:n()))),
         length_m = as.numeric(st_length(x)),
         maxspeed = as.numeric(maxspeed))


osm_buff = osm_drive |> 
  st_union() |>
  st_transform(27700) |> 
  st_buffer(20) |> 
  st_transform(4326)

all_rds_buff = st_buffer(all_rds,100) |>
  st_transform(3857)

crashes_B3108 = crashes[osm_buff,]

crashes_B3108 = match_tag(crashes_B3108)

tot_cost = sum(crashes_B3108$cost_per_collision)/15

crashes_B3108$ID = all_rds$ID[st_nearest_feature(crashes_B3108,all_rds)]

crashes_cost = crashes_B3108 |> 
  st_set_geometry(NULL) |> 
  group_by(ID) |> 
  summarise(total_cost = sum(cost_per_collision)/15) |> 
  left_join(all_rds, by = "ID")

st_geometry(crashes_cost) = crashes_cost$x

# get background map
bg <- basemaps::basemap_raster(osm_buff, map_service = "carto",increase_zoom =2, map_type = "light")

tm_1 <- tm_shape(bg)+
  tm_rgb()+
  tm_shape(crashes_cost) +
  tm_lines(col = "total_cost",col_alpha = 1,
              col.scale = tm_scale_intervals(values = "tol.rainbow_wh_br"),
              col.legend = tm_legend("value (£)", frame = FALSE,legend.border.col = NA,
                                     position = c(0.8,0.93)),
              lwd = 8)+
  tm_title("Value of prevention of collisionss along B3108 per year (Period: 2010-2024)",size = 1.5)+
  tm_layout(frame = FALSE)  

tmap_save(tm_1, "plots/collisions_value.png")

casualties_B3108 = casualties |> 
  filter(collision_index %in% crashes_B3108$collision_index) |> 
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0))

# import LSOA geometry (not simplified)
lsoa_geo_gb = st_read("data/LSOA_IMD2025_OSGB1936_-8580071282870403721.gpkg") |> 
  select(lsoa21_code = LSOA21CD,lsoa21_name = LSOA21NM,geometry = SHAPE) |> 
  st_transform(4326)

lsoa_home = lsoa_home_plot(casualty_df = casualties_B3108, lsoa_geo = lsoa_geo_gb)


## import nuts geo lvel 3 (cities) from eurostat and filter for UK
uk_LA <- get_eurostat_geospatial("sf", resolution = "01", nuts_level = "3", "2021", crs = "4326", update_cache = TRUE) |>
  filter(CNTR_CODE == "UK")

Wiltshire = filter(uk_LA,NAME_LATN == "Wiltshire CC")

mapview(Wiltshire)

crashes_wiltshire = crashes[Wiltshire,] 

crashes_wiltshire = match_tag(crashes_wiltshire)

tot_cost = sum(crashes_wiltshire$cost_per_collision)/15

casualties_wiltshire = casualties |> 
  filter(collision_index %in% crashes_wiltshire$collision_index) |> 
  mutate(fatal_count = if_else(casualty_severity == "Fatal", 1, 0))

Wiltshire_fatal = casualties_wiltshire |> 
  filter(casualty_severity %in% c("Fatal", "Serious")) |> 
  group_by(collision_year) |> 
  summarise(KSI = n())

index_df = casualties_index(casualties = casualties_wilshire, base_year = 2010, end_year = 2024)

p1 = index_plot(indexes = index_df,city = "Wiltshire",plot_dir = "plots/", base_year = 2010, end_year = 2024)

cra_cas_osm <- casualty_osm_link(osm_links = osm_drive, casualties = casualties_B3108, crashes = crashes_B3108,
                                 year_from = 2010, year_to = 2024,
                                 casualties_buffer = 20) |> 
  mutate(sev_plot_size = if_else(Fatal == 1, 1.5, Serious))

cra_cas_osm = inner_join(crashes_B3108, casualties_B3108) 

cas_scale <- c("Car occupant", "Motorcyclist", "Cyclist", "Taxi occupant",
               "Goods vehicle occupant", "Bus occupant", "Other vehicle",
               "Data missing", "Mobility scooter rider",
               "Agricultural vehicle occupant", "Horse rider",
               "E-scooter rider", "Pedestrian")

unique(cra_cas_osm$casualty_type)

cra_cas_osm$casualty_type <- factor(cra_cas_osm$casualty_type, levels = cas_scale)

bm_masked = basemaps::basemap_raster(osm_buff, map_service = "carto",increase_zoom =2, map_type = "light")

tm2 <- tm_shape(bm_masked)+
  tm_rgb()+
  tm_shape(cra_cas_osm)+
  tm_bubbles(fill = "casualty_type",
             shape = "casualty_type",
             size = "casualty_adjusted_severity_serious",
             shape.legend = tm_legend_combine("fill"),
             size.legend = tm_legend(title = "casualty_adjusted_severity_serious")) +
  tm_layout(frame = FALSE)+
  tm_legend(position = c(0.13,0.94))+
  tm_title("Collision location with casualty type represented by shape and colour and severity represented by size. B3108: 2010 and 2024")

tmap_save(tm2, "plots/cas_type_sev_map.png", width = 7090, height = 2500, dpi = 440)
