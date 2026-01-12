library(gmapsdistance)
library(lubridate)
library(tidyverse)
library(googleway)
library(reshape2)
library(openair)
library(lwgeom)
library(sf)
library(dplyr)
library(mapview)
library(mapedit)
library(stplanr)
library(osmactive)
library(osmdata)
library(tmap)
library(tmap.networks)

source("../stats19_stats/R/get.R")

select <- dplyr::select

## enter google console distance api key
api_key <- NULL

library(openxlsx)

load("../ data/run_03012026.RData")

# tag data book
# https://www.gov.uk/government/publications/tag-data-book

# vehicle occupancy
TAG_A1.3.3 = read.xlsx("https://assets.publishing.service.gov.uk/media/694a907b72075a1d4a508a58/tag-data-book-v2-02.xlsm",
                       sheet = "A1.3.3", startRow = 25)


TAG_A1.3.4 = read.xlsx("https://assets.publishing.service.gov.uk/media/694a907b72075a1d4a508a58/tag-data-book-v2-02.xlsm",
                       sheet = "A1.3.4", startRow = 25)

TAG_A1.3.4_2 = TAG_A1.3.4[c(2:8,13:15),c(1:2,10:15)]

names(TAG_A1.3.4_2) = c("vehicle", "journey", "7","10","16","19","week_average", "weekend")

TAG_A1.3.4_3 = TAG_A1.3.4_2 |>
  melt(c("vehicle", "journey"),variable.name = "hour",value.name = "pc_trips") |>
  fill("vehicle", .direction = "down") |>
  filter(!hour == "week_average")

hour_lookup = data.frame(hour = as.character(seq(1,24)-1),
                         tag_hour = as.character(c(19,19,19,19,19,19,19,7,7,7,10,10,10,10,10,10,16,16,16,19,19,19,19,19)))

TAG_A1.3.4_4 = expand.grid(day = c("weekday","weekend"), hour = as.character(seq(1,24)-1),direction_of_travel = c("E","W")) |>
  left_join(hour_lookup, by = "hour") |>
  mutate(tag_hour = ifelse(day == "weekend", "weekend",tag_hour)) |>
  left_join(TAG_A1.3.4_3, by = c("tag_hour" = "hour")) |>
  select(-tag_hour) |>
  mutate(journey = gsub(" ", "",journey)) |>
  mutate(journey = gsub("Non–Work", "Commuting&Other",journey))

TAG_A1.3.5 = read.xlsx("https://assets.publishing.service.gov.uk/media/694a907b72075a1d4a508a58/tag-data-book-v2-02.xlsm",
                       sheet = "A1.3.5", startRow = 26) |>
  fill("Type", .direction = "down")

names(TAG_A1.3.5) = c("vehicle", "journey", "7","10","16","19","week_average", "weekend", "All")

TAG_A1.3.5_2 = TAG_A1.3.5 |>
  melt(c("vehicle", "journey"),variable.name = "hour",value.name = "costs") |>
  fill("vehicle", .direction = "down") |>
  filter(!hour %in% c("week_average", "All"))

TAG_A1.3.5_3 = expand.grid(day = c("weekday","weekend"), hour = as.character(seq(1,24)-1),direction_of_travel = c("E","W")) |>
  left_join(hour_lookup, by = "hour") |>
  mutate(tag_hour = ifelse(day == "weekend", "weekend",tag_hour)) |>
  left_join(TAG_A1.3.5_2, by = c("tag_hour" = "hour")) |>
  select(-tag_hour) |>
  mutate(journey = gsub(" ", "",journey)) |>
  mutate(journey = gsub("Working", "Work",journey))

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

raw_tot = raw_counts |>
  group_by(direction_of_travel,mode) |>
  summarise(raw_count = sum(raw_count))

count_tot = counts_2019 |>
  group_by(direction_of_travel,mode) |>
  summarise(adjusted_count = sum(adjusted_count)) |>
  inner_join(raw_tot) |>
  mutate(night_time = adjusted_count-raw_count) |>
  mutate(night_time = ifelse(night_time < 0,0,night_time))

count_night = count_tot |> transmute(direction_of_travel, mode, raw_count = round(night_time/12,1))

# create data frame for next week
#kr8d8 = create_dates_next_week()
# add in columns for weekend and weekday
kr8d8_2 = cutData(kr8d8, type = "weekend") |>
  transmute(date,hour,wday = as.character(weekend))

#saveRDS(kr8d8, "data/d8s.RDS")

AADT = expand.grid(hour = seq(1,24)-1,direction_of_travel = c("E","W")) |>
  left_join(raw_counts, by = c("hour", "direction_of_travel"))

AADT_night = AADT |>
  filter(is.na(raw_count)) |>
  select(-mode,-raw_count) |>
  left_join(count_night, by = c("direction_of_travel"))

AADT_day = AADT |>
  filter(!is.na(raw_count))

AADT_all = rbind(AADT_day, AADT_night)

AADT_d8s = left_join(kr8d8,AADT_all, by = c("hour"))

lookup_veh = data.frame(dft_vehicle = c("two_wheeled_motor_vehicles","cars_and_taxis","buses_and_coaches","lgvs","hgvs_2_rigid_axle", "hgvs_3_rigid_axle","hgvs_4_or_more_rigid_axle",
                                        "hgvs_3_or_4_articulated_axle","hgvs_5_articulated_axle","hgvs_6_articulated_axle"),
                        TAG_vehicle = c("","Car","PSV","LGV","OGV1","OGV1","OGV1", "OGV2","OGV2","OGV2"))

AADT_all_tot = AADT_d8s |>
  mutate(mode = ifelse(grepl("hgv", mode),"hgvs",mode)) |>
  group_by(date,mode) |>
  summarise(count = sum(raw_count))

AADT_diurnal_direction = AADT_d8s |>
  arrange(hour) |>
  left_join(lookup_veh, by = c("mode" = "dft_vehicle")) |>
  mutate(hour = as.character(hour)) |>
  left_join(TAG_A1.3.4_4, by = c("hour","TAG_vehicle" = "vehicle", "direction_of_travel", "wday" = "day")) |>
  mutate(frac_trips = as.numeric(pc_trips)/100) |>
  mutate(purpose_count = raw_count*(frac_trips)) |>
  mutate(purpose_count = ifelse(is.na(purpose_count),raw_count,purpose_count)) |>
  left_join(TAG_A1.3.5_3, by = c("TAG_vehicle" = "vehicle","direction_of_travel", "wday" = "day", "journey", "hour"))

AADT_dir = AADT_d8s |>
  group_by(date,direction_of_travel) |>
  summarise(raw_count = sum(raw_count,na.rm = TRUE))

p1 = timeVariation(AADT_dir, "raw_count", group = "direction_of_travel", name.pol = namez,ylab = "vehicles", main = "Trips by direction")

AADT_tots = AADT_dir |>
  mutate(hour = hour(date)) |>
  group_by(direction_of_travel,hour) |>
  summarise(tot = sum(raw_count)/7) |>
  ungroup() |>
  group_by(hour) |>
  mutate(pc = tot/sum(tot)*100)

filename <- paste0("plots/AADT_direction.png")
png(filename, width=800, height=800, units="px", res=160)
print(p1, subset = "hour")
dev.off()

AADT_dd = filter(AADT_diurnal_direction, !is.na(costs))

p2 = timeVariation(AADT_dd, "raw_count", group = "TAG_vehicle",ylab = "vehicles",key.columns = 2)

filename <- paste0("plots/AADT_mode.png")
png(filename, width=800, height=800, units="px", res=250)
print(p2, subset = "hour")
dev.off()

AADT_tots = AADT_dir |>
  mutate(hour = hour(date)) |>
  group_by(direction_of_travel,hour) |>
  summarise(tot = sum(raw_count)/7) |>
  ungroup() |>
  group_by(hour) |>
  mutate(pc = tot/sum(tot)*100)

AADT_costs = AADT_diurnal_direction |>
  transmute(date,mode,direction_of_travel, cost = purpose_count*costs) |>
  mutate(mode = ifelse(grepl("hgv",mode),"hgvs",mode),
         mode = ifelse(grepl("lgv",mode),"lgvs",mode)) |>
  group_by(date,direction_of_travel,mode) |>
  summarise(cost = sum(cost,na.rm = TRUE)) |>
  dcast(date+direction_of_travel~mode,fun.aggregate = sum)

AADT_counts = AADT_diurnal_direction |>
  transmute(date,mode,direction_of_travel,purpose_count) |>
  mutate(mode = ifelse(grepl("hgv",mode),"hgvs",mode),
         mode = ifelse(grepl("lgv",mode),"lgvs",mode)) |>
  group_by(date,mode,direction_of_travel) |>
  summarise(count = sum(purpose_count,na.rm = TRUE)) |>
  dcast(date+direction_of_travel~mode,fun.aggregate = sum)



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

all_rds_buff = st_buffer(all_rds,100) |>
  st_transform(3857)

# get background map
bg <- basemaps::basemap_raster(all_rds_buff, map_service = "carto", map_type = "light")

cols = c("maxspeed", "scenario_A", "scenario_B", "scenario_C", "scenario_D")

for (c in cols){

  title_nam = gsub("_", " ",c)

#assign(paste0(c,"_map"), cycle_net_c)
tmap_mode("plot")
tm1 <- tm_shape(bg)+
  tm_rgb(col_alpha = 1)+
  tm_shape(all_rds) +
  tm_edges(
    col = c,col.scale = tm_scale_categorical(values = c("red","orange","green", "blue"),levels = c(20,30,40,50)),
    col_alpha = 0.6,
    lwd = 5,
    col.legend = tm_legend(show = TRUE,title = "Speed Limit (mph)", position = c(0.8,0.90), frame = FALSE)
  )+
  tm_text("ID",ymod = -1,xmod =-0.5)+
tm_title(text = paste0("B3108 Between Warminster Rd and Bath Road ",title_nam), position = c(0.2,0.96))
# tm_layout(frame = FALSE, panel.show = FALSE)+
# tm_animate(by = "date")
#tm_components(c("tm_legend", "tm_credits"), position = c("left", "top"), bg.color = "grey95")

#tmap_animation(tm1, filename = paste0("plots/speeds.gif"), width =1700, height = 3000, dpi = 300, delay = 40)

tmap_save(tm1, paste0("plots/B3108_speeds_",c,".png"))
}

# get speed data using Google API
#link_speed_dat <- get_link_speeds(all_rds,api_key = api_key,both_directions = TRUE)

#lsd_nams = colsplit(row.names(link_speed_dat),"_",c("L","direction","other"))

#link_speed_dat$direction = lsd_nams

#save(link_speed_dat,kr8d8, file = "data/run_03012026.RData")

# combine link speed data with rd data and costs
journey_times = link_speed_dat |>
  mutate(direction = diretion) |>
  left_join(all_rds,by = "ID") |>
  mutate(speed_ms = osm_length_m/journey_time) |>
  mutate(speed_kph = speed_ms*3.6) |>
  mutate(speed_mph = speed_kph*0.6214) |>
  mutate(speed_scenario_A = ifelse(speed_mph > scenario_A,scenario_A,speed_mph),
         speed_scenario_B = ifelse(speed_mph > scenario_B,scenario_B,speed_mph),
         speed_scenario_C = ifelse(speed_mph > scenario_C,scenario_C,speed_mph),
         speed_scenario_D = ifelse(speed_mph > scenario_D,scenario_D,speed_mph)) |>
  mutate(jt_scenario_A = osm_length_m/(speed_scenario_A/0.6214/3.6),
         jt_scenario_B = osm_length_m/(speed_scenario_B/0.6214/3.6),
         jt_scenario_C = osm_length_m/(speed_scenario_C/0.6214/3.6),
         jt_scenario_D = osm_length_m/(speed_scenario_D/0.6214/3.6)) |>
  group_by(date,direction) |>
  summarise(current_jt = sum(journey_time),
            scenario_A_jt = sum(jt_scenario_A),
            scenario_B_jt = sum(jt_scenario_B),
            scenario_C_jt = sum(jt_scenario_C),
            scenario_D_jt = sum(jt_scenario_D)) |>
  mutate(hour = hour(date),
         direction_of_travel = str_sub(direction,2,-1)) |>
  left_join(AADT_costs, by = c("direction_of_travel", "date")) |>
  mutate(total_cost = sum(buses_and_coaches+cars_and_taxis+hgvs+lgvs)) |>
  mutate(A_vs_current = scenario_A_jt-current_jt,
         B_vs_current = scenario_B_jt-current_jt,
         C_vs_current = scenario_C_jt-current_jt,
         D_vs_current = scenario_D_jt-current_jt) |>
  mutate(A_cost = (A_vs_current/3600)*total_cost,
         B_cost = (B_vs_current/3600)*total_cost,
         C_cost = (C_vs_current/3600)*total_cost,
         D_cost = (D_vs_current/3600)*total_cost)

sum(journey_times$A_cost,na.rm = TRUE)*52
sum(journey_times$B_cost,na.rm = TRUE)*52
sum(journey_times$C_cost,na.rm = TRUE)*52
sum(journey_times$D_cost,na.rm = TRUE)*52

journey_times_summary = journey_times |>
  group_by(direction) |>
  summarise(mean_jt = mean(current_jt),
            mean_jt_A = mean(scenario_A_jt),
            mean_jt_B = mean(scenario_B_jt),
            mean_jt_C = mean(scenario_C_jt),
            mean_jt_D = mean(scenario_D_jt))

mean(journey_times$current_jt)
mean(journey_times$scenario_A_jt)
mean(journey_times$scenario_B_jt)
mean(journey_times$scenario_C_jt)
mean(journey_times$scenario_D_jt)

EW_jt = filter(journey_times, direction == "EW")
WE_jt = filter(journey_times, direction == "WE")

jt_ew = timeVariation(EW_jt, c("current_jt", "scenario_A_jt", "scenario_B_jt","scenario_C_jt","scenario_D_jt"),
                      name.pol = c("Current", "Scenario A", "Scenario B", "Scenario C", "Scenario D"),ylab = "Total journey time (seconds)",
                      main = "Westbound journey time estimate for each scenario")

filename <- paste0("plots/JT_EW_dayhour.png")
png(filename, width=2000, height=500, units="px", res=160)
print(jt_ew$plot$day.hour)
dev.off()

filename <- paste0("plots/JT_EW.png")
png(filename, width=2000, height=1300, units="px", res=200)
print(jt_ew)
dev.off()

jt_we = timeVariation(WE_jt, c("current_jt", "scenario_A_jt", "scenario_B_jt","scenario_C_jt","scenario_D_jt"),
                      name.pol = c("Current", "Scenario A", "Scenario B", "Scenario C", "Scenario D"), ylab = "Total journey time (seconds)",
                      main = "Eastbound journey time estimate for each scenario")

filename <- paste0("plots/JT_WE_dayhour.png")
png(filename, width=2000, height=500, units="px", res=160)
print(jt_we$plot$day.hour)
dev.off()

filename <- paste0("plots/JT_WE.png")
png(filename, width=2000, height=1300, units="px", res=200)
print(jt_we)
dev.off()

cost_ew = timeVariation(EW_jt, c("A_cost", "B_cost", "C_cost","D_cost"),
                        name.pol = c("Scenario A", "Scenario B", "Scenario C", "Scenario D"),ylab = "Increase in cost (pounds)",
                        main = "Westbound market loss estimates for each scenario")

filename <- paste0("plots/CST_EW_dayhour.png")
png(filename, width=2000, height=500, units="px", res=160)
print(cost_ew$plot$day.hour)
dev.off()

filename <- paste0("plots/CST_EW.png")
png(filename, width=2000, height=1300, units="px", res=200)
print(cost_ew)
dev.off()

cost_we = timeVariation(WE_jt, c("A_cost", "B_cost", "C_cost","D_cost"),
                        name.pol = c("Scenario A", "Scenario B", "Scenario C", "Scenario D"),ylab = "Increase in cost (pounds)",
                        main = "Eastbound market loss estimates for each scenario")

filename <- paste0("plots/CST_WE_dayhour.png")
png(filename, width=2000, height=500, units="px", res=160)
print(cost_we$plot$day.hour)
dev.off()

filename <- paste0("plots/CST_WE.png")
png(filename, width=2000, height=1300, units="px", res=200)
print(cost_we)
dev.off()

# diurnal_profile = data.frame(dp = full_road_speeds$speed/mean(full_road_speeds$speed),
#                              direction = full_road_speeds$direction,
#                              hour = hour(full_road_speeds$date),
#                              wday = wday(full_road_speeds$date))
# saveRDS(diurnal_profile, "data/diurnal_profile.RDS")

journey_times_link = link_speed_dat |>
  mutate(direction = diretion) |>
  left_join(all_rds,by = "ID") |>
  mutate(speed_ms = osm_length_m/journey_time) |>
  mutate(speed_kph = speed_ms*3.6) |>
  mutate(speed_mph = speed_kph*0.6214) |>
  mutate(speed_scenario_A = ifelse(speed_mph > scenario_A,scenario_A,speed_mph),
         speed_scenario_B = ifelse(speed_mph > scenario_B,scenario_B,speed_mph),
         speed_scenario_C = ifelse(speed_mph > scenario_C,scenario_C,speed_mph),
         speed_scenario_D = ifelse(speed_mph > scenario_D,scenario_D,speed_mph)) |>
  mutate(jt_scenario_A = osm_length_m/(speed_scenario_A/0.6214/3.6),
         jt_scenario_B = osm_length_m/(speed_scenario_B/0.6214/3.6),
         jt_scenario_C = osm_length_m/(speed_scenario_C/0.6214/3.6),
         jt_scenario_D = osm_length_m/(speed_scenario_D/0.6214/3.6)) |>
  group_by(date,ID, direction) |>
  summarise(current_jt = sum(journey_time),
            scenario_A_jt = sum(jt_scenario_A),
            scenario_B_jt = sum(jt_scenario_B),
            scenario_C_jt = sum(jt_scenario_C),
            scenario_D_jt = sum(jt_scenario_D)) |>
  mutate(hour = hour(date),
         direction_of_travel = str_sub(direction,2,-1)) |>
  left_join(AADT_costs, by = c("direction_of_travel", "date")) |>
  mutate(total_cost = sum(buses_and_coaches+cars_and_taxis+hgvs+lgvs)) |>
  mutate(A_vs_current = scenario_A_jt-current_jt,
         B_vs_current = scenario_B_jt-current_jt,
         C_vs_current = scenario_C_jt-current_jt,
         D_vs_current = scenario_D_jt-current_jt) |>
  mutate(A_cost = (A_vs_current/3600)*total_cost,
         B_cost = (B_vs_current/3600)*total_cost,
         C_cost = (C_vs_current/3600)*total_cost,
         D_cost = (D_vs_current/3600)*total_cost)

link_cost = journey_times_link |>
  group_by(ID, hour) |>
  summarise(A_cost = round(sum(A_cost,na.rm = TRUE)),
            B_cost = round(sum(B_cost,na.rm = TRUE)),
            C_cost = round(sum(C_cost,na.rm = TRUE)),
            D_cost = round(sum(D_cost,na.rm = TRUE)),
            A_time = round(mean(A_vs_current)),
            B_time = round(mean(B_vs_current)),
            C_time = round(mean(C_vs_current)),
            D_time = round(mean(D_vs_current)))

cycle_speed_ms = 5

journey_trips = link_speed_dat |>
  mutate(direction = diretion) |>
  left_join(all_rds,by = "ID") |>
  mutate(cycle_time = osm_length_m/cycle_speed_ms) |>
  mutate(direction = str_sub(direction,2,-1)) |>
  left_join(AADT_counts, by = c("direction" = "direction_of_travel", "date")) |>
  mutate(cycle_time_tot = cycle_time*pedal_cycles)

onroad_cycle_track_p_min = 5.22

cycle_path_benefit_pence = (sum(journey_trips$cycle_time_tot,na.rm = TRUE)/60)*onroad_cycle_track_p_min*52


save(AADT_diurnal_direction,journey_times_link, file = "data/table_dat.RData")

# combine into one link
full_rd = osm_drive |>
  st_union() |>
  st_line_merge() |>
  st_as_sf() |>
  mutate(ID = "B3108")

# pick out one end
full_rd_start = data.frame(st_coordinates(full_rd)) |>
  filter(X == min(X)) |>
  st_as_sf(coords = c("X", "Y"), crs = 4326)
# and the other
full_rd_end = data.frame(st_coordinates(full_rd)) |>
  filter(X == max(X)) |>
  st_as_sf(coords = c("X", "Y"), crs = 4326)


mapview(full_rd_end)+full_rd_start

l = "L05"

for (l in all_rds$ID){

  link_sf = link_speed_dat |>
    filter(ID == l) |>
    group_by(diretion,ID) |>
    summarise(mean_speed = round(mean(speed),1)*0.6214) |>
    left_join(all_rds, by = "ID")

  st_geometry(link_sf) = link_sf$x

  # generate buffer around the link for the background map
  link_buff = st_buffer(link_sf,40) |>
    st_transform(3857)

  # get background map
  bg <- basemaps::basemap_raster(link_buff, map_service = "carto", map_type = "light")

  # plot bg map
  tm1 <- tm_shape(bg)+
    tm_rgb(col_alpha = 1)

  geo_list <- list()
  W_list = list()
  E_list = list()
  for (d in link_sf$diretion){

  link_sf_dir = link_sf |>
    filter(diretion == d) |>
    st_transform(27700)

  if(d == "WE"){
  l_geo = link_sf_dir$x + c(2,2)
  } else {
    l_geo = link_sf_dir$x + c(-2,-2)
  }

  l_geo_plot = l_geo |>
    st_set_crs(27700) |>
    st_as_sf() |>
    mutate(mean_speed = as.numeric(link_sf_dir$mean_speed)) |>
    st_transform(4326)


  link_coords = data.frame(st_coordinates(l_geo_plot))

  link_W = rbind(link_coords[1,],link_coords[NROW(link_coords),]) |>
    filter(X == min(X)) |>
    st_as_sf(coords = c("X", "Y"), crs = 4326)
  link_E = rbind(link_coords[1,],link_coords[NROW(link_coords),]) |>
    filter(X == max(X)) |>
    st_as_sf(coords = c("X", "Y"), crs = 4326)

  geo_list[[d]] <- l_geo_plot
  W_list[[d]] = link_W
  E_list[[d]] = link_E

  }

  geo_all = do.call(rbind,geo_list)
  W_all = do.call(rbind, W_list) |>
    mutate(col = c("red", "green"))
  E_all = do.call(rbind,E_list) |>
    mutate(col = c("green", "red"))

  tm1 = tm1 +
    tm_shape(geo_all) +
    tm_lines(
      col = "mean_speed",col.scale = tm_scale_continuous(values = "-gmt.seis",limits = c(10,50)),
      col_alpha = 0.6,
      lwd = 5
    )+
    tm_shape(W_all)+
    tm_dots(fill = "col", size = 1)+
    tm_shape(E_all)+
    tm_dots(fill = "col", size = 1)



  # generate a date string for the plot
  # d8s2plot <- speed_dat |>
  #   filter(ID == speed_plot$ID[1]) |>
  #   mutate(day = wday(date_new,label = TRUE,abbr = FALSE)) |>
  #   transmute(date_day = paste0(format(date_new), "\n", day))

  # set to override limit of 64 frames
  #tmap_options(facet.max = 200)

  #assign(paste0(c,"_map"), cycle_net_c)


   tm1 = tm1 + tm_legend(show = TRUE,title = "mean speed (mph)", position = c(0,0.40))
    # tm_title(text = d8s2plot$date_day, position = c(0.05,0.96))+
    # tm_layout(frame = FALSE, panel.show = FALSE)+
    # tm_animate(by = "date")
  #tm_components(c("tm_legend", "tm_credits"), position = c("left", "top"), bg.color = "grey95")

  #tmap_animation(tm1, filename = paste0("plots/speeds.gif"), width =1700, height = 3000, dpi = 300, delay = 40)

   tmap_save(tm1, paste0("plots/", l, "_direction.png"))

  link_sf_both = link_sf |>
    summarise(`mean speed (mph)` = mean(mean_speed))

  link_coords = data.frame(st_coordinates(link_sf_both))

  link_W = rbind(link_coords[1,],link_coords[NROW(link_coords),]) |>
    filter(X == min(X)) |>
    st_as_sf(coords = c("X", "Y"), crs = 4326)
  link_E = rbind(link_coords[1,],link_coords[NROW(link_coords),]) |>
    filter(X == max(X)) |>
    st_as_sf(coords = c("X", "Y"), crs = 4326)

  tm2 = tm_shape(bg)+
    tm_rgb(col_alpha = 1)+
    tm_shape(link_sf_both) +
    tm_lines(
      col = "mean speed (mph)",col.scale = tm_scale_continuous(values = "-gmt.seis",limits = c(10,50)),
      col_alpha = 0.4,
      lwd = 7
    )+
    tm_text(l,ymod = 0.5)+
    tm_shape(link_W)+
    tm_dots(fill = "black", size = 1)+
    tm_text("West",ymod = 1)+
    tm_shape(link_E)+
    tm_dots(fill = "black", size = 1)+
    tm_text("East", ymod = -1)
  if(l == "L06"){
   tm2 = tm2 + tm_legend(show = TRUE,position = c(0,1), frame = FALSE)
  } else {
    tm2 = tm2 + tm_legend(show = FALSE,title = "mean speed (mph)")
  }

  tmap_save(tm2, paste0("plots/", l, ".png"))

  assign(l,tm2)

}

link_plots = tmap_arrange(L01,L02,L03,L04,L05,L06, ncol = 3,nrow = 2)

tmap_save(link_plots, paste0("plots/link_plots.png"),width = 3000, height = 3000)

link_cost_sf = link_cost |>
  left_join(all_rds, by = "ID") |>
  ungroup()

st_geometry(link_cost_sf) <- link_cost_sf$x

# generate buffer around the link for the background map
link_buff = st_buffer(link_cost_sf,100) |>
  st_transform(3857)

max(link_cost_sf$D_time)

# get background map
bg <- basemaps::basemap_raster(link_buff, map_service = "carto",increase_zoom =2, map_type = "light")

hrz = paste0(sprintf("%02d", unique(link_cost_sf$hour)),":00")

scenarios = expand_grid(scenario = c("A", "B", "C", "D"),
                        parameter = c("time", "cost"))
for (s in unique(scenarios$scenario)){

  for (p in unique(scenarios$parameter)){

  if(p == "time"){
    bks = seq(0,35,by = 5)
    title = "increase in\ntime (seconds)"
  } else {
    bks = seq(0,1400, by = 100)
    title = "TAG cost\n(pounds)"
  }

    col2plot = paste0(s,"_",p)

tm2 = tm_shape(bg,raster.downsample = FALSE)+
  tm_rgb()+
  tm_shape(link_cost_sf)+
  tm_lines(
    col = col2plot,col.scale = tm_scale_intervals(values = "-gmt.seis",breaks = bks),
    col_alpha = 0.4,
    lwd = 6,
    col.legend = tm_legend(title = title, frame = FALSE)
  )+
  tm_animate("hour",fps = 1)+
  tm_credits(
    text = paste0(hrz),  # This will use the facet variable values
    position = c("center", "top"),  # or "left", "top"
    size = 1.5,
    fontface = "bold"
  ) +
  tm_layout(panel.show = FALSE, frame = FALSE)+
  tm_shape(link_cost_sf)+
  tm_text("ID",ymod = -1,xmod =-0.5)

tmap_animation(tm2, filename = paste0("plots/scenario_", s,"_",p, ".gif"))

  }

}

for (L in unique(link_speed_dat$ID)){

  l_df = link_speed_dat |>
    filter(ID == L) |>
    mutate(speed_mph = speed*0.6214)

  # plot mean for all links
  p1 <- timeVariation(l_df, pollutant = "speed_mph", group = "diretion", ylab = "speed (mph)", main = paste0("Estimated speed profile of ",L, " for week beginning 05/01/2026 based on historical data"))


  filename <- paste0("plots/speed_profile_",L,"_dayhour.png")
  png(filename, width=2000, height=500, units="px", res=160)
  print(p1, subset = "day.hour")
  dev.off()

  filename <- paste0("plots/speed_profile_",L,".png")
  png(filename, width=2000, height=1300, units="px", res=200)
  print(p1)
  dev.off()

}





#full_road_speeds = get_start_end_speeds(start_point = full_rd_start, end_point = full_rd_end, api_key = api_key, both_directions = TRUE)




for (l in )


# plot mean for all links
p1 <- timeVariation(link_speed_dat, pollutant = "speed", group = "bearing", ylab = "speed (kph)", main = paste0("speed profile of B3108 Lower Stoke road to junction with Warminster Road"))


saveRDS(link_speed_dat, "data/lower_stoke.RDS")

speed_dat = readRDS("data/B3108.RDS")







#plot for each link
dir.create("plots")
for (eL in unique(speed_dat$ID)){
  df2 <- speed_dat |>
    filter(ID == eL)
  d = unique(df2$angle)
  for ()
  tryCatch({

      select(date = date_new, speed)
    p2 <- timeVariation(df2, pollutant = "speed", main = paste0(eL, " Weekly Speeds (kph)"))
    filename <- paste0("plots/", eL, "_", Sys.Date(), ".png")
    png(filename, width=2000, height=500, units="px", res=160)
    print(p2$plot$day.hour)
    dev.off()
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# plot mean for all links
p2 <- timeVariation(speed_dat, pollutant = "speed", main = paste0("speed profile of area central Ulaanbaatar, Mongolia (kph)"))

# save plot
filename <- paste0("plots/avg_profile.png")
png(filename, width=2000, height=1500, units="px", res=180)
print(p2)
dev.off()


# join spatial data to speeds and adjust date format for tmap
speed_plot <- speed_dat |>
  transmute(date = as.factor(date_new),day, ID, speed) |>
  left_join(osm_drive, by = "ID") |>
  st_set_geometry("geometry")






