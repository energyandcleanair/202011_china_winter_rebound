install.packages(c("tidyverse","remotes","openxlsx"))
remotes::install_github("energyandcleanair/rcrea", force=F, upgrade=F)
Sys.setenv("TZ"="Etc/UTC"); #https://github.com/rocker-org/rocker-versioned/issues/89

library(dplyr)
library(remotes)
library(tidyverse)

# remotes::install_github("energyandcleanair/creadeweather", force=F, upgrade=F)

library(rcrea)
# library(creadeweather)
library(openxlsx)
library(lubridate)

source("https://github.com/energyandcleanair/202011_china_winter_rebound/raw/main/plots.R")
source("https://github.com/energyandcleanair/202011_china_winter_rebound/raw/main/utils.R")


# Parameters (filled in python engine)  -----------------------------------
folder <- {tmp_dir}
folder_regional <- file.path(folder, "regional")
folder_national <- file.path(folder, "national")


# Getting data ------------------------------------------------------------
print("READING STATIONS============")
stations <- utils.read_stations(local=F)
station_ids <- stations %>% filter(!is.na(keyregion2018)) %>% pull(station_code)

print("GETTING MEASUREMENTS============")
m.station.obs <- rcrea::measurements(source="mee",
                                     location_id=station_ids,
                                     process_id="station_day_mad",
                                     date_from="2018-10-01",
                                     poll=c(rcrea::PM25),
                                     deweathered = F)
print("ENRICHING MEASUREMENTS============")
m.station.obs.rich <- m.station.obs %>%
  mutate(region_id=toupper(region_id)) %>%
  left_join(stations %>% select(station_code, keyregion2018), by=c("region_id"="station_code"))

print("MERGING PER REGION============")
m.region <- m.station.obs.rich %>%
  filter(!is.na(keyregion2018)) %>%
  group_by(date=lubridate::date(date), poll, unit, region_id=tolower(keyregion2018), process_id, source, timezone) %>%
  dplyr::summarise(value=mean(value, na.rm=T)) %>%
  mutate(country="CN",
         region_name=tools::toTitleCase(region_id))


# YOY 30 day  -------------------------------------------------------------------
print("PLOTTING YOY 30 DAY============")
rcrea::plot_recents(meas_raw=m.region %>% filter(date>="2018-12-01"),
                    type="yoy-relative",
                    subfile_by = "poll",
                    running_days = c(30),
                    subplot_by = "region_id",
                    color_by="value",
                    size="l",
                    range="full",
                    # For naming only
                    aggregate_level = "region",
                    source="mee",
                    folder = folder_regional
)


# Key region targets ------------------------------------------------------
print("PREPARING TARGETS [1]============")
targets = read.csv("http://github.com/energyandcleanair/202011_china_winter_rebound/raw/main/data/winter_targets_2020_2021.csv")%>%
  rename(keyregion2018=keyRegion2018) %>%
  mutate_at(c('target_period', 'base_period'), function(x) x %>% gsub('Q', '\\.', .) %>% as.numeric)

print("PREPARING TARGETS [2]============")
#daily average PM2.5 by station
m.station.obs %>% filter(date>='2019-01-01') %>%
  mutate(date=as.Date(date), region_id=toupper(region_id)) %>%
  group_by(stationID=region_id, poll, date, process_id, timezone, unit, source) %>%
  summarise_at('value', mean, na.rm=T) %>%
  left_join(stations) %>%
  mutate(Q=lubridate::quarter(date, with_year = T)) ->
  daily

print("PREPARING TARGETS [3]============")
#quarterly averages by city
daily %>% filter(poll=='pm25') %>%
  group_by(Province, CityEN, keyregion2018, poll, Q) %>% summarise_at('value', mean, na.rm=T) ->
  m.quarterly

print("PREPARING TARGETS [4]============")
#quarterly averages by key region
daily %>% filter(poll=='pm25') %>%
  group_by(keyregion2018, poll, Q) %>% summarise_at('value', mean, na.rm=T) %>%
  bind_rows(m.quarterly) %>%
  filter(!is.na(keyregion2018)) %>%
  mutate(source='hourly') -> m.quarterly

print("PREPARING TARGETS [5]============")
#add targets for winter 2020-2021
targets %>% select(keyregion2018, Province, CityEN, Q=target_period, base_period, value=PM25_target) %>%
  mutate(source='target') %>% bind_rows(m.quarterly) -> m.quarterly

print("PREPARING TARGETS [6]============")
#add official monthly averaged data from MEE
targets %>% select(keyregion2018, Province, CityEN, Q=base_period, value=base_PM25) %>%
  mutate(source='monthly') %>% bind_rows(m.quarterly) -> m.quarterly

print("PREPARING TARGETS [7]============")
#calculate Q4 and Q1 target reductions
targets %>% mutate(PM25_target = ifelse(PM25_target==0, base_PM25, PM25_target), #replace 0's with base period values
                   target_reduction = PM25_target / base_PM25 - 1) %>%
  select(CityEN, Province, keyregion2018, Q = target_period, target_reduction) %>% left_join(m.quarterly, .) ->
  m.quarterly

print("PREPARING TARGETS [8]============")
#calculate annual means consistent with targets
m.quarterly %<>% filter(source=='hourly') %>%
  select(CityEN, Province, keyregion2018, base_period=Q, value_base = value) %>%
  left_join(m.quarterly, .) %>%
  mutate(value = ifelse(source == 'target',
                        (1 + target_reduction) * value_base,
                        value))

print("PREPARING TARGETS [9]============")
mean4=function(x) ifelse(length(x)==4,mean(x), NA)
targetmean=function(df) {
  df %>% group_by(CityEN, Province, keyregion2018) %>% summarise_at('value', mean4) %>%
    filter(!is.na(value))
}

bind_rows(m.quarterly %>% filter(source == 'hourly', Q %in% c(2020.1, 2020.2, 2020.3)),
          m.quarterly %>% filter(source == 'target', Q %in% c(2020.4))) %>% targetmean -> targetmeans.Q4

bind_rows(m.quarterly %>% filter(source == 'hourly', Q %in% c(2020.2, 2020.3)),
          m.quarterly %>% filter(source == 'target', Q %in% c(2020.4, 2021.1))) %>% targetmean -> targetmeans.Q1

print("PREPARING TARGETS [10]============")
max.date <- max(daily$date)
m.qtd = daily %>% ungroup %>% filter(Q %in% c(2019.4, 2020.4), yday(date)<=yday(max(daily$date)))
means.qtd = m.qtd %>% group_by(keyregion2018, Province, CityEN, poll, Q) %>% summarise_at('value', mean, na.rm=T)
means.qtd = m.qtd %>% group_by(keyregion2018, poll, Q) %>% summarise_at('value', mean, na.rm=T) %>% bind_rows(means.qtd)
means.qtd = m.qtd %>% group_by(poll, Q) %>% summarise_at('value', mean, na.rm=T) %>%
  mutate(keyregion2018='National') %>% bind_rows(means.qtd)
means.qtd$keyregion2018[is.na(means.qtd$keyregion2018)] <- 'Other regions'

means.qtd %>%
  mutate_at('Q', make.names) %>% spread(Q, value) %>%
  mutate(QTD_reduction = X2020.4/X2019.4-1) %>% select(-starts_with('X')) -> qtd.yoy

print("PREPARING TARGETS [11]============")
t.keyregions <- qtd.yoy %>%
  full_join(m.quarterly %>% filter(source=='target', Q==2020.4) %>%
              select(CityEN, Province, keyregion2018, target_reduction), .) %>%
  mutate(Q=2020.4) %>%
  bind_rows(m.quarterly %>%
              filter(source=='target', Q==2021.1) %>%
              select(CityEN, Province, keyregion2018, target_reduction, Q)) %>%
  filter(is.na(CityEN), !is.na(target_reduction))

print("PREPARING TARGETS [12]============")
m.keyregions <- m.station.obs %>%
  left_join(stations %>% mutate(region_id=tolower(stationID)) %>% select(region_id, keyregion2018)) %>%
  filter(!is.na(keyregion2018),
         date >= "2018-10-01") %>%
  group_by(region_id=keyregion2018, process_id, date=as.Date(date), poll, timezone, unit, source) %>%
  summarise_at('value', mean, na.rm=T)

#One year running mean plots with the target for end of Q4 and Q1 marked as points, and linear path from latest value to targets
print("PLOTTING TARGETS============")
plots.targets_yoyts_vs_targets(m.keyregions, t.keyregions, folder=folder_regional)


# # National ----------------------------------------------------------------
# print("NATIONAL ============")
# if(length(unique(m.station.obs$region_id)) < 1385){
#   stop("Missing stations. You probably queried those of key control regions only")
# }
# m.national <- m.station.obs %>%
#   group_by(date, poll, unit, region_id="china", process_id, source, timezone) %>%
#   summarise(value=mean(value, na.rm=T)) %>%
#   mutate(country="CN",
#          region_name=tools::toTitleCase(region_id))
#
#
# rcrea::plot_recents(meas_raw=m.national %>% filter(date>="2018-11-30"),
#                     type="yoy-relative",
#                     subplot_by = "poll",
#                     running_days = c(30),
#                     subfile_by = "country",
#                     aggregate_level = "country",
#                     color_by="value",
#                     size="l",
#                     source="mee",
#                     folder = folder_national
# )


# Deweathered quarters ----------------------------------------------------
# m.dew.city.raw <- creadeweather::deweather(source="mee",
#                                            city=cities$CityEN,
#                                            poll="pm25",
#                                            output="anomaly",
#                                            upload_results = F,
#                                            years_force_refresh = NULL,
#                                            training_start_anomaly = "2017-04-01",
#                                            training_end_anomaly = "2019-06-30")
#
# m.dew.city <- m.dew.city.raw %>%
#   tidyr::unnest(normalised) %>%
#   select(-c(process_deweather, process_id, predicted)) %>%
#   left_join(stations %>% mutate(region_id=tolower(CityEN))) %>%
#   select(region_id, date, poll, unit, process_id=output, CityZH, Province, keyregion2018, value)
#
#
# m.dew.station.raw <- creadeweather::deweather(source="mee",
#                                               city=duplicated_cities,
#                                               aggregate_level = "station",
#                                               poll="pm25",
#                                               output="anomaly",
#                                               upload_results = F,
#                                               years_force_refresh = NULL,
#                                               training_start_anomaly = "2017-04-01",
#                                               training_end_anomaly = "2019-06-30")
#
# m.dew.station <-  m.dew.station.raw %>%
#   tidyr::unnest(normalised) %>%
#   select(-c(process_deweather, process_id, predicted)) %>%
#   left_join(stations %>% mutate(region_id=tolower(station_code))) %>%
#   mutate(region_id=tolower(CityEN)) %>%
#   group_by(region_id, date, poll, unit, process_id=output, CityZH, Province, keyregion2018) %>%
#   summarize_at("value", mean, na.rm=T)
#
# m.dew <- bind_rows(m.dew.city, m.dew.station)

