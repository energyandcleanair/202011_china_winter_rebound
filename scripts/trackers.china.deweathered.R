# This is the script to be run by CREA scraper to produce deweathered trackers
# for key regions on a regular basis
# It should be self-sufficient

library(dplyr)
library(remotes)
library(tidyverse)
remotes::install_github("energyandcleanair/rcrea", force=F, upgrade=F)
remotes::install_github("energyandcleanair/creadeweather", force=F, upgrade=F)

library(rcrea)
library(creadeweather)
library(openxlsx)
library(lubridate)

source("https://github.com/energyandcleanair/202011_china_winter_rebound/raw/main/plots.R")
source("https://github.com/energyandcleanair/202011_china_winter_rebound/raw/main/utils.R")


# Parameters (filled in python engine)  -----------------------------------
folder <- {tmp_dir}
folder_city <- file.path(folder, "city")
folder_regional <- file.path(folder, "regional")
dir.create(folder_city, showWarnings = F, recursive = T)
dir.create(folder_regional, showWarnings = F, recursive = T)


# Deweathering parameters -------------------------------------------------
filename <- "m.dew.endq3.pbl.RDS"
training_start <- "2017-04-01"
training_end <- "2019-09-30"
add_pbl <- T
config <- list("filename"=filename,
               "training_start"=training_start,
               "training_end"=training_end,
               "add_pbl"=add_pbl)


# Getting data ------------------------------------------------------------
stations <- utils.read_stations(local=F)
cities<- stations %>%
  filter(!is.na(keyregion2018),
         CityEN!="<NA>") %>%
  distinct(CityEN)
duplicated_cities <- utils.check_cities_unique(cities, stations)
cities <- cities %>% filter(!CityEN %in% duplicated_cities)

m.dew.city.raw <- creadeweather::deweather(source="mee",
                                           city=cities$CityEN,
                                           poll="pm25",
                                           output="anomaly",
                                           upload_results = F,
                                           add_pbl=add_pbl,
                                           training_start_anomaly = training_start,
                                           training_end_anomaly = training_end)

m.dew.city <- m.dew.city.raw %>%
  tidyr::unnest(normalised) %>%
  select(-c(process_deweather, process_id, predicted)) %>%
  left_join(stations %>% mutate(region_id=tolower(CityEN))) %>%
  select(region_id, date, poll, unit, process_id=output, CityZH, Province, keyregion2018, value)


m.dew.station.raw <- creadeweather::deweather(source="mee",
                                              city=duplicated_cities,
                                              aggregate_level = "station",
                                              poll="pm25",
                                              output="anomaly",
                                              upload_results = F,
                                              add_pbl=add_pbl,
                                              training_start_anomaly = training_start,
                                              training_end_anomaly = training_end)

m.dew.station <-  m.dew.station.raw %>%
  tidyr::unnest(normalised) %>%
  select(-c(process_deweather, process_id, predicted)) %>%
  left_join(stations %>% mutate(region_id=tolower(station_code))) %>%
  mutate(region_id=tolower(CityEN)) %>%
  group_by(region_id, date, poll, unit, process_id=output, CityZH, Province, keyregion2018) %>%
  summarize_at("value", mean, na.rm=T)

m.dew <- bind_rows(m.dew.city, m.dew.station)
m.dew.regional <- m.dew %>%
  # Average
  group_by(poll, unit, date, process_id, region_id=keyregion2018, region_name=keyregion2018, country="CN") %>%
  summarise_at("value", mean, na.rm=T) %>%
  filter(!is.na(region_id))

jsonlite::write_json(config, file.path(folder_regional, "deweathered_mee_keyregions.json"), auto_unbox = T)
saveRDS(m.dew.regional, file.path(folder_regional, "deweathered_mee_keyregions.RDS"))

# Plotting ----------------------------------------------------------------
# City level
rcrea::plot_recents(meas_raw=m.dew %>% filter(process_id=="anomaly_percent") %>%
                      mutate(region_name=paste0("[",keyregion2018,"] ", region_name)) %>%
                      arrange(region_name),
                    running_days = 30,
                    aggregate_level = "city",
                    source="mee",
                    range="full",
                    size="l",
                    color_by="value",
                    subplot_by = "region_id",
                    subfile_by = "poll",
                    folder=folder_city)
# Regional level
rcrea::plot_recents(meas_raw=m.dew.regional %>% filter(process_id=="anomaly_percent"),
                    running_days = 30,
                    aggregate_level = "region",
                    source="mee",
                    range="full",
                    size="l",
                    color_by="value",
                    subplot_by = "region_id",
                    subfile_by = "poll",
                    folder=folder_regional,
                    add_to_ggplot = labs(title="Weather-corrected anomalies of PM2.5 levels",
                                         y="Anomaly [%]"))


plots.quarter_anomalies(m.dew.regional, "absolute", folder=folder_regional)
plots.quarter_anomalies(m.dew.regional, "relative", folder=folder_regional)


ggplot(m.sanity %>% rcrea::utils.running_average(30), aes(date,value)) +
  geom_line(aes(col=type,linetype=type)) +
  facet_wrap(~region_id, scales="free_y")

ggsave(file.path(folder_regional,,  "sanity.png"))

