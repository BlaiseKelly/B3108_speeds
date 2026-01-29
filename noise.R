library(dplyr)
library(osmactive)
library(osmdata)
library(sf)
library(reshape2)
library(stringr)
library(lubridate)
library(openair)
library(openxlsx)
library(tidyr)
library(tmap)

#ΔL(5)=−10log⁡10(510)=−10log⁡10(0.5)≈+3.0 dB

# loads in google speed run and d8 data table
load("data/run_03012026.RData")

# tag data book
# https://www.gov.uk/government/publications/tag-data-book

raw_counts = read.csv("https://storage.googleapis.com/dft-statistics/road-traffic/downloads/rawcount/count_point_id/dft_rawcount_count_point_id_947637.csv") |>
  filter(year == "2018") |>
  select(hour,direction_of_travel,pedal_cycles,two_wheeled_motor_vehicles,cars_and_taxis,buses_and_coaches,lgvs,hgvs_2_rigid_axle, hgvs_3_rigid_axle,hgvs_4_or_more_rigid_axle,
         hgvs_3_or_4_articulated_axle,hgvs_5_articulated_axle,hgvs_6_articulated_axle) |>
  melt(c("hour", "direction_of_travel"),variable.name = "mode", value.name = "raw_count")


counts_2019 = read.csv("https://storage.googleapis.com/dft-statistics/road-traffic/downloads/aadfbydirection/count_point_id/dft_aadfbydirection_count_point_id_947637.csv") |>
  filter(year == "2019") |>
  select(direction_of_travel,pedal_cycles,two_wheeled_motor_vehicles,cars_and_taxis,buses_and_coaches,lgvs,hgvs_2_rigid_axle, hgvs_3_rigid_axle,hgvs_4_or_more_rigid_axle,
         hgvs_3_or_4_articulated_axle,hgvs_5_articulated_axle,hgvs_6_articulated_axle) |>
  melt(c("direction_of_travel"),variable.name = "mode", value.name = "adjusted_count")

download.file("https://assets.publishing.service.gov.uk/media/684965113a2aa5ba84d1dee2/tra0307-traffic-distribution-by-time-of-day.ods", destfile = "data/tra0307.ods", mode = "wb")

tra0307 = readODS::read_ods("data/tra0307.ods", sheet = "TRA0307", skip = 4) |> 
  mutate(hour = str_sub(`Time of Day`, 1,-10)) |> 
  filter(Year == "2024") |> 
  select(hour,Monday,Tuesday,Wednesday,Thursday, Friday, Saturday, Sunday) |> 
  melt("hour", value.name = "frac") |> 
  group_by(variable) |> 
  mutate(frac = frac/sum(frac))


kr8d8_days = kr8d8 |> 
  mutate(dow = as.character(wday(date, label = TRUE, abbr = FALSE))) |> 
  mutate(hour = str_sub(format(date),12, -4)) |> 
  left_join(tra0307, by = c("dow" = "variable", "hour"))

raw_tot = raw_counts |>
  group_by(direction_of_travel,mode) |>
  summarise(raw_count = sum(raw_count))

count_tot = counts_2019 |>
  group_by(direction_of_travel,mode) |>
  summarise(adjusted_count = sum(adjusted_count))

inner_join(kr8d8_days) 

#saveRDS(kr8d8, "data/d8s.RDS")

AADT_d8s = expand.grid(hour = unique(tra0307$hour),direction_of_travel = c("E","W")) |>
  left_join(kr8d8_days, by = c("hour")) |> 
  left_join(count_tot, by = "direction_of_travel") |> 
  mutate(count = frac*adjusted_count) |> 
  transmute(date,wday,hour = hour(date),direction_of_travel,mode,raw_count = count)

# AADT_night = AADT |>
#   filter(is.na(raw_count)) |>
#   select(-mode,-raw_count) |>
#   left_join(count_night, by = c("direction_of_travel"))
# 
# AADT_day = AADT |>
#   filter(!is.na(raw_count))
# 
# AADT_all = rbind(AADT_day, AADT_night)
# 
# AADT_d8s = left_join(kr8d8,AADT_all, by = c("hour"))

# create lookup table
lookup_simple = data.frame(dft_vehicle = c("two_wheeled_motor_vehicles","cars_and_taxis","buses_and_coaches","lgvs","hgvs_2_rigid_axle", "hgvs_3_rigid_axle","hgvs_4_or_more_rigid_axle",
                                        "hgvs_3_or_4_articulated_axle","hgvs_5_articulated_axle","hgvs_6_articulated_axle"),
                        TAG_vehicle = c("Car","Car","HGV","LGV","HGV","HGV","HGV", "HGV","HGV","HGV"))

# reshape date into requirements for noise
AADT_noise = AADT_d8s |>
  arrange(hour) |>
  left_join(lookup_simple, by = c("mode" = "dft_vehicle")) |>
  mutate(hour = as.character(hour)) |> 
  filter(!is.na(TAG_vehicle)) |> 
  group_by(date,TAG_vehicle,direction_of_travel) |> 
  summarise(counts = sum(raw_count,na.rm = TRUE))

# how does this look
p1 = timeVariation(AADT_noise, "counts", group = "TAG_vehicle",ylab = "vehicles", main = "Trips by vehicle classification")


filename <- paste0("plots/AADT_noise.png")
png(filename, width=1400, height=500, units="px", res=160)
print(p1, subset = "day.hour")
dev.off()

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

osm_buff = osm_drive |> 
  st_transform(27700) |> 
  st_union() |> 
  st_buffer(100) |> 
  st_transform(4326)

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

# combine link speed data with rd data and costs
link_speeds = link_speed_dat |>
  mutate(direction_of_travel = str_sub(diretion,1,-2)) |>
  left_join(all_rds,by = "ID") |> 
  mutate(speed_ms = osm_length_m/journey_time) |>
  mutate(speed_mph = speed*0.6214) |> 
  mutate(speed_scenario_A = ifelse(speed_mph > scenario_A,scenario_A,speed_mph),
         speed_scenario_B = ifelse(speed_mph > scenario_B,scenario_B,speed_mph),
         speed_scenario_C = ifelse(speed_mph > scenario_C,scenario_C,speed_mph),
         speed_scenario_D = ifelse(speed_mph > scenario_D,scenario_D,speed_mph)) |>
  mutate(jt_scenario_A = osm_length_m/(speed_scenario_A/0.6214/3.6),
         jt_scenario_B = osm_length_m/(speed_scenario_B/0.6214/3.6),
         jt_scenario_C = osm_length_m/(speed_scenario_C/0.6214/3.6),
         jt_scenario_D = osm_length_m/(speed_scenario_D/0.6214/3.6)) |> 
  select(date,
         ID,
         direction_of_travel,
         speed_mph,
         speed_scenario_A,
         speed_scenario_B,
         speed_scenario_C,
         speed_scenario_D)

all_rds_buff = st_buffer(all_rds,200) 

b_area_bb <- st_bbox(all_rds_buff)

##download landuse from osm
x <- opq(bbox = b_area_bb) %>%
  add_osm_feature(key = c('building')) %>%
  osmdata_sf()

buildings_raw = x$osm_polygons

buildings = buildings_raw |> 
  select(osm_id, amenity,building) |> 
  filter(amenity %in% c(NA,"nursing_home", "student_accommodation")) |> 
  filter(building %in% c("residential","hall_of_residence","semidetached_house","terrace","apartments","house","detached","farm","dormitory","bungalow",NA,"assisted_living","yes"))


# import LSOA population for GB
gb_pop <- read.xlsx("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2022revisednov2025tomid2024/sapelsoasyoa20222024.xlsx",
                    sheet = "Mid-2024 LSOA 2021",startRow = 4)|> 
  select(LSOA.2021.Code, Total)

# import LSOA geometry (not simplified)
lsoa_geo_gb = st_read("data/LSOA_IMD2025_OSGB1936_-8580071282870403721.gpkg") |> 
  select(lsoa21_code = LSOA21CD,lsoa21_name = LSOA21NM,geometry = SHAPE) |> 
  st_transform(4326)

lsoa_rd <- st_intersection(lsoa_geo_gb,osm_buff)

lsoa_area = lsoa_geo_gb |> 
  filter(lsoa21_code %in% lsoa_rd$lsoa21_code)

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

rd_dat = link_speeds |> 
  left_join(AADT_noise, by = c("date","direction_of_travel")) |> 
  mutate(L_10 = ifelse(TAG_vehicle == "Car",counts*10^((27.7 + 10 * log10(speed_mph*1.6093))/10),
                           ifelse(TAG_vehicle == "LGV",counts*10^((34.7 + 10 * log10(speed_mph*1.6093))/10),
                                  ifelse(TAG_vehicle == "HGV",counts*10^((39.7 + 10 * log10(speed_mph*1.6093))/10),NA)))) |> 
  mutate(L_10_A = ifelse(TAG_vehicle == "Car",counts*10^((27.7 + 10 * log10(speed_scenario_A*1.6093))/10),
                       ifelse(TAG_vehicle == "LGV",counts*10^((34.7 + 10 * log10(speed_scenario_A*1.6093))/10),
                              ifelse(TAG_vehicle == "HGV",counts*10^((39.7 + 10 * log10(speed_scenario_A*1.6093))/10),NA)))) |> 
  mutate(L_10_B = ifelse(TAG_vehicle == "Car",counts*10^((27.7 + 10 * log10(speed_scenario_B*1.6093))/10),
                         ifelse(TAG_vehicle == "LGV",counts*10^((34.7 + 10 * log10(speed_scenario_B*1.6093))/10),
                                ifelse(TAG_vehicle == "HGV",counts*10^((39.7 + 10 * log10(speed_scenario_B*1.6093))/10),NA)))) |> 
  mutate(L_10_C = ifelse(TAG_vehicle == "Car",counts*10^((27.7 + 10 * log10(speed_scenario_C*1.6093))/10),
                         ifelse(TAG_vehicle == "LGV",counts*10^((34.7 + 10 * log10(speed_scenario_C*1.6093))/10),
                                ifelse(TAG_vehicle == "HGV",counts*10^((39.7 + 10 * log10(speed_scenario_C*1.6093))/10),NA)))) |> 
  mutate(L_10_D = ifelse(TAG_vehicle == "Car",counts*10^((27.7 + 10 * log10(speed_scenario_D*1.6093))/10),
                         ifelse(TAG_vehicle == "LGV",counts*10^((34.7 + 10 * log10(speed_scenario_D*1.6093))/10),
                                ifelse(TAG_vehicle == "HGV",counts*10^((39.7 + 10 * log10(speed_scenario_D*1.6093))/10),NA)))) |> 
  ungroup() |> 
  group_by(date,ID) |> 
  summarise(L_10 = 10*log10(sum(L_10)),
            L_10_A = 10*log10(sum(L_10_A)),
            L_10_B = 10*log10(sum(L_10_B)),
            L_10_C = 10*log10(sum(L_10_C)),
            L_10_D = 10*log10(sum(L_10_D)))

# how does this look
p2 = timeVariation(rd_dat, "L_10", group = "ID",ylab = "L10 (dB)", main = "Trips by vehicle classification")


filename <- paste0("plots/AADT_L10.png")
png(filename, width=1400, height=500, units="px", res=160)
print(p2, subset = "day.hour")
dev.off()

bs = list()
for (i in all_rds$ID){
  
  rd_df = filter(all_rds, ID == i)
  
  
  
  builds = buildings_lsoa |> 
    select(osm_id,geometry) |> 
    mutate(ID = i,
           dist = as.numeric(st_distance(geometry, rd_df))) |> 
    st_set_geometry(NULL) |> 
    filter(dist <=200)
  
  bs[[i]] = builds
  
}

all_builds = do.call(rbind,bs) |> 
  left_join(rd_dat, by = "ID")

# taking the mean of noise level over many hours
laeq_energy_mean <- function(L_dB) {
  10*log10(mean(10^(L_dB/10), na.rm = TRUE))
}

scenarios = c("L_10", "L_10_A", "L_10_B", "L_10_C", "L_10_D")
scenario_list = list()
for(s in scenarios){

  b_df = all_builds |> 
    select(osm_id,date,ID,dist,L_10 = s) |> 
    mutate(L_build = L_10-10*log10(dist/10)) |> 
  dcast(osm_id+date ~ ID,value.var = "L_build", fun.aggregate = mean) |> 
  ungroup() |> 
  rowwise() |> 
  mutate(L_mean = mean(c(L01,L02,L03,L04,L05,L06),na.rm = TRUE)) |> 
  #mutate(L_total = 10*log10(sum((10^(L01/10)),(10^(L02/10)),(10^(L03/10)),(10^(L04/10)),(10^(L05/10)),(10^(L06/10)),na.rm = TRUE)))
  select(osm_id,date,L_mean) |> 
    mutate(scenario = gsub("_10", "_build", s))
  
  scenario_list[[s]] = b_df

}

all_scenarios = do.call(rbind,scenario_list) |> 
  dcast(osm_id+date ~ scenario,value.var = "L_mean")

all_scenarios_periods = all_scenarios |> 
  mutate(hour = hour(date)) |> 
  mutate(period = ifelse(hour >= 7  & hour < 23, "day","night")) |> 
  group_by(osm_id,period) |> 
  summarise(L_mean = laeq_energy_mean(L_build),
            L_mean_A = laeq_energy_mean(L_build_A),
            L_mean_B = laeq_energy_mean(L_build_B),
            L_mean_C = laeq_energy_mean(L_build_C),
            L_mean_D = laeq_energy_mean(L_build_D))

all_scenarios_hour = all_scenarios |> 
  mutate(hour = hour(date)) |> 
  group_by(osm_id,hour) |> 
  summarise(L_mean = laeq_energy_mean(L_build),
            L_mean_A = laeq_energy_mean(L_build_A),
            L_mean_B = laeq_energy_mean(L_build_B),
            L_mean_C = laeq_energy_mean(L_build_C),
            L_mean_D = laeq_energy_mean(L_build_D))



breaks <- c(-Inf, seq(45, 81, by = 3), Inf)

# Generate labels automatically
labels <- c(
  paste0("<", breaks[2]),  # "<45"
  paste0(breaks[2:(length(breaks)-2)], "-", breaks[3:(length(breaks)-1)] - 1),  # "45-47", "48-50", etc.
  paste0(">", breaks[length(breaks)-1])  # ">81"
)



period_matrix = list()
for (s in scenarios[-1]){
  
  b_s = gsub("_10","_mean", s)

df_binned <- all_scenarios_periods |>
  mutate(
    before_bin = cut(L_mean, breaks = breaks, labels = labels, right = FALSE),
    after_bin  = cut(!!sym(b_s),  breaks = breaks, labels = labels, right = FALSE)
  )

hourly_matrices <- df_binned |>
  group_by(period) |>
  summarise(
    matrix = list(
      count(cur_data(), before_bin, after_bin, .drop = FALSE) |>
        pivot_wider(
          names_from = after_bin,
          values_from = n,
          values_fill = 0
        )
    ),
    .groups = "drop"
  )

period_matrix[[b_s]] = hourly_matrices

}

all_ps = do.call(rbind, period_matrix)

all_ps$scenario = paste0(str_sub(row.names(all_ps),1,-3), "_", all_ps$period)

# 1️⃣ Create a new workbook
# wb <- createWorkbook()
# 3️⃣ Loop through the list and add each to the workbook
# for (sheet_name in all_ps$scenario) {
#   addWorksheet(wb, sheetName = sheet_name)
#   writeData(wb, sheet = sheet_name, all_ps$matrix[[which(sheet_name == all_ps$scenario)]])
# }
# 4️⃣ Save the workbook
#saveWorkbook(wb, file = "noise_scenarios.xlsx", overwrite = TRUE)

# 3️⃣ Loop through the list and add each to the workbook
for (sheet_name in all_ps$scenario) {
  write_sheet(
    data = all_ps$matrix[[which(sheet_name == all_ps$scenario)]],
    ss   = "https://docs.google.com/spreadsheets/d/1ye5mGe87G8MOEXqLE1CDZjKf7IeSbZD_Zn87ZR_CpzE/edit?gid=0#gid=0",
    sheet = sheet_name
  )
  
}

models = c("A", "B", "C", "D")

cost_df = googlesheets4::range_read(
  ss    = "https://docs.google.com/spreadsheets/d/1ucvtuX5xq0Dq6FAKIRIEckvFTT6Pji7p90nkgnsfb7Y",
  sheet = "Output - worksheet A",
  range = "B19:B23",
  col_names = FALSE
)

names(cost_df) = "cost"

for (m in models){
  
  s_nam = paste0("Scenario ", m)
  
  sht = googlesheets4::range_read(
    ss    = "https://docs.google.com/spreadsheets/d/1ucvtuX5xq0Dq6FAKIRIEckvFTT6Pji7p90nkgnsfb7Y",
    sheet = paste0("Output - worksheet ",m),
    range = "H19:H23",
    col_names = FALSE
  )

 cost_df[[s_nam]] = sht$...1
  
}

cost_df <- cost_df |>
  bind_rows(
    cost_df |>
      summarise(
        !!names(cost_df)[1] := "Total",
        across(-1, ~ sum(.x, na.rm = TRUE))
      )
  )

write_clip(houses_df)


houses_df = googlesheets4::range_read(
  ss    = "https://docs.google.com/spreadsheets/d/1ucvtuX5xq0Dq6FAKIRIEckvFTT6Pji7p90nkgnsfb7Y",
  sheet = "Output - worksheet A",
  range = "B28:B31",
  col_names = FALSE
)

names(houses_df) = "description"

for (m in models){
  
  s_nam = paste0("Scenario ", m)
  
  sht = googlesheets4::range_read(
    ss    = "https://docs.google.com/spreadsheets/d/1ucvtuX5xq0Dq6FAKIRIEckvFTT6Pji7p90nkgnsfb7Y",
    sheet = paste0("Output - worksheet ",m),
    range = "H28:H31",
    col_names = FALSE
  )
  
  houses_df[[s_nam]] = sht$...1
  
}

save(houses_df, cost_df, file = "data/results.RData")


## NOISE
build_binned = buildings_lsoa |> 
  st_set_geometry(NULL)
  filter(dist2rd <= 200) |> 
  #mutate(dist = cut_width(dist2rd, width = 50, center = 25))
  mutate(dist = cut(dist2rd, breaks = c(0,10,20,30,60,90,120,150,180,200))) |> 
  group_by(dist) |> 
  summarise(dwellings = n())

all_scenarios_build = left_join(all_scenarios_hour,buildings, by = "osm_id")

st_geometry(all_scenarios_build) = all_scenarios_build$geometry
r = all_rds$ID[5]
for (r in all_rds$ID){
  
  rd2plot = filter(all_rds,ID == r) |> 
    st_centroid() |> 
    st_buffer(600)
  
  pos_lookup <- list(
    L01 = c(0.85, 0.52),
    L02 = c(0.85, 0.52),
    L03 = c(0.85, 0.96),
    L04 = c(0.05, 0.95),
    L05 = c(0.04, 0.51),
    L06 = c(0.85, 0.52)
  )
  
  pos <- pos_lookup[[r]]
  

# get background map
bg <- basemaps::basemap_raster(rd2plot, map_service = "carto", map_type = "light")

cenarios = names(all_scenarios_build2)[3:7]

for (c in cenarios){
  
  title_nam = gsub("_", " ",c)
  
  all_scenarios_build2 = all_scenarios_build[rd2plot,]
  
  tm1 <- tm_shape(bg)+
    tm_rgb(col_alpha = 1)+
    tm_shape(all_scenarios_build2) +
    tm_polygons(
      fill = c,
      fill.scale = tm_scale_intervals(values = "tol.rainbow_wh_br", breaks = seq(40,70,2)),
      fill.legend = tm_legend(show = TRUE,title = "Noise (dB)", position = pos, frame = FALSE)
    )+
    tm_animate(frames = "hour", fps = 2)
  # tm_layout(frame = FALSE, panel.show = FALSE)+
  # tm_animate(by = "date")
  #tm_components(c("tm_legend", "tm_credits"), position = c("left", "top"), bg.color = "grey95")
  
  tmap_animation(tm1, filename = paste0("plots/",r,"_",c,"_noise.gif"), dpi = 350)
  
  if(c == "L_mean"){
    
    assign(r,tm1)
  } else {
    assign(c,tm1)
  }
  

}

tm_all = tmap_arrange(L_mean_A,L_mean_B,L_mean_C,L_mean_D)

tmap_animation(tm_all, filename = paste0("plots/",r,"_noise.gif"),width = 2000,height = 2000, dpi = 350)

}


tm_rd_all = tmap_arrange(L01,L02,L03,L04,L05,L06)

tmap_animation(tm_all, filename = paste0("plots/",r,"_noise.gif"),width = 7000,height = 8000, dpi = 600)

all_scenarios_build = left_join(all_scenarios_periods,buildings, by = "osm_id") 

st_geometry(all_scenarios_build) = all_scenarios_build$geometry
r = all_rds$ID[5]
for (r in all_rds$ID){
  
  rd2plot = filter(all_rds,ID == r) |> 
    st_centroid() |> 
    st_buffer(600)
  
  pos_lookup <- list(
    L01 = c(0.85, 0.56),
    L02 = c(0.85, 0.56),
    L03 = c(0.85, 0.96),
    L04 = c(0.05, 0.95),
    L05 = c(0.04, 0.55),
    L06 = c(0.85, 0.56)
  )
  
  pos <- pos_lookup[[r]]
  
  # get background map
  bg <- basemaps::basemap_raster(rd2plot, map_service = "carto", map_type = "light")
  
  cenarios = names(all_scenarios_build)[3:7]
  
  for (p in periods){
  
  for (c in cenarios){
    
    title_nam = gsub("_", " ",c)
    
    periods = unique(all_scenarios_periods$period)
  
    all_scenarios_build2 = all_scenarios_build[rd2plot,] |> 
      filter(period == p) |> 
      mutate(diff = L_mean-!!sym(c))
    
    tm1 <- tm_shape(bg)+
      tm_rgb(col_alpha = 1)+
      tm_shape(all_scenarios_build2) +
      tm_polygons(
        fill = "diff",
        fill.scale = tm_scale_intervals(values = "tol.rainbow_wh_br", breaks = seq(0,3,0.4)),
        fill.legend = tm_legend(show = TRUE,title = "Noise (dB)", position = pos, frame = FALSE)
      )+
      tm_title(text = paste0(gsub("L_mean_", "Scenario ",c), " change from baseline at each property during ",p))
    # tm_layout(frame = FALSE, panel.show = FALSE)+
    # tm_animate(by = "date")
    #tm_components(c("tm_legend", "tm_credits"), position = c("left", "top"), bg.color = "grey95")
    
    tmap_save(tm1, filename = paste0("plots/",r,"_",c,"_noise_",p,".png"), dpi = 350)
    
    if(c == "L_mean"){
      
      assign(r,tm1)
    } else {
      assign(c,tm1)
    }
    
  }
    tm_all = tmap_arrange(L_mean_A,L_mean_B,L_mean_C,L_mean_D)
    
    tmap_save(tm_all, filename = paste0("plots/",r,"_noise_",p,".png"),width = 7000,height = 8000, dpi = 600)
    
  }
  
 
  
}
