library(stats19)
library(sf)
library(dplyr)
library(eurostat)
library(osmdata)
library(osmactive)

source("../stats19_stats/R/get.R")
source("../stats19_stats/R/plots.R")
source("../stats19_stats/R/summary.R")
source("../stats19_stats/R/tables.R")

crashes = get_stats19(year = 2004,type = "collision") |> 
  filter(collision_year >= 2010) |>
  format_sf() |> 
  st_transform(4326)

casualties = get_stats19(year = 2004, type = "casualty") |> 
  filter(collision_index == crashes$collision_index)

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

## import nuts geo lvel 3 (cities) from eurostat and filter for UK
uk_LA <- get_eurostat_geospatial("sf", resolution = "01", nuts_level = "3", "2021", crs = "4326", update_cache = TRUE) |>
  filter(CNTR_CODE == "UK")

Wiltshire = filter(uk_LA,NAME_LATN == "Wiltshire CC")

mapview(Wiltshire)

crashes_wiltshire = crashes[Wiltshire,] 

Wiltshire_fatal = crashes_wiltshire |> 
  filter(collision_severity == "Fatal") |> 
  group_by(collision_year) |> 
  summarise(fatal = sum(as.numeric(number_of_casualties)))

