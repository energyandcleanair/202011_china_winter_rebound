if(!require(remotes)){install.packages("remotes"); require(remotes)}
if(!require(tidyverse)){install.packages("tidyverse"); require(tidyverse)}
require(lubridate)

remotes::install_github("energyandcleanair/rcrea", force=T, upgrade=T, dependencies=F)
library(rcrea)
Sys.setenv("TZ"="Etc/UTC"); #https://github.com/rocker-org/rocker-versioned/issues/89


source("https://github.com/energyandcleanair/202011_china_winter_rebound/raw/main/plots.R")
source("https://github.com/energyandcleanair/202011_china_winter_rebound/raw/main/utils.R")


# Parameters (filled in python engine)  -----------------------------------
folder <- {tmp_dir} # Used in rpy2, replaced by a temporary folder
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
                                     poll=c(rcrea::PM25, rcrea::PM10),
                                     deweathered = F)

print("ENRICHING MEASUREMENTS============")
m.station.obs.rich <- m.station.obs %>%
  mutate(location_id=toupper(location_id)) %>%
  left_join(stations %>% select(station_code, keyregion2018), by=c("location_id"="station_code"))


print("REMOVING SANDSTORMS============")
m.station.obs.rich <- m.station.obs.rich %>%
  utils.add_sandstorm() %>%
  filter(!sand_storm) %>%
  filter(poll==rcrea::PM25) # Don't need PM10 anymore


print("MERGING PER REGION============")
m.region <- m.station.obs.rich %>%
  filter(!is.na(keyregion2018)) %>%
  group_by(date=lubridate::date(date), poll, unit, location_id=tolower(keyregion2018), process_id, source, timezone) %>%
  dplyr::summarise(value=mean(value, na.rm=T)) %>%
  mutate(country="CN",
         location_name=tools::toTitleCase(location_id))


# YOY 30 day  -------------------------------------------------------------------
print("PLOTTING YOY 30 DAY============")
rcrea::plot_recents(meas_raw=m.region %>% filter(date>="2018-12-01"),
                    type="yoy-relative",
                    subfile_by = "poll",
                    running_days = c(30),
                    subplot_by = "location_id",
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
targets = read_csv("http://github.com/energyandcleanair/202011_china_winter_rebound/raw/main/data/winter_targets_2020_2021.csv")%>%
  rename(keyregion2018=keyRegion2018) %>%
  mutate_at(c('target_period', 'base_period', 'base_PM25', '2019Q4','2019Q1'), function(x) x %>% gsub('Q', '\\.', .) %>% as.numeric) %>%
  mutate(poll="pm25")

print("PREPARING TARGETS [2]============")
#daily average PM2.5 by station
m.station.obs %>% filter(date>='2019-01-01') %>%
  mutate(date=as.Date(date), location_id=toupper(location_id)) %>%
  group_by(stationID=location_id, poll, date, process_id, timezone, unit, source) %>%
  summarise_at('value', mean, na.rm=T) %>%
  left_join(stations) %>%
  mutate(Q=lubridate::quarter(date, with_year = T)) ->
  daily

print("PREPARING TARGETS [3]============")
#quarterly averages by city
daily %>% filter(poll=='pm25') %>%
  group_by(Province, CityEN, keyregion2018, poll, Q) %>%
  summarise_at('value', mean, na.rm=T) -> m.quarterly

print("PREPARING TARGETS [4]============")
#quarterly averages by key region
daily %>% filter(poll=='pm25') %>%
  group_by(keyregion2018, poll, Q) %>%
  summarise_at('value', mean, na.rm=T) %>%
  bind_rows(m.quarterly) %>%
  filter(!is.na(keyregion2018)) %>%
  mutate(source='hourly') -> m.quarterly

print("PREPARING TARGETS [5]============")
#add targets for winter 2020-2021
targets %>% select(keyregion2018, poll, Province, CityEN, Q=target_period, base_period, value=PM25_target) %>%
  mutate(source='target') %>% bind_rows(m.quarterly) -> m.quarterly

print("PREPARING TARGETS [6]============")
#add official monthly averaged data from MEE
targets %>% select(keyregion2018, poll, Province, CityEN, Q=base_period, value=base_PM25) %>%
  mutate(source='monthly') %>% bind_rows(m.quarterly) -> m.quarterly

print("PREPARING TARGETS [7]============")
#calculate Q4 and Q1 target reductions
targets %>% mutate(PM25_target = ifelse(PM25_target==0, base_PM25, PM25_target), #replace 0's with base period values
                   target_reduction = PM25_target / base_PM25 - 1) %>%
  select(CityEN, Province, poll, keyregion2018, Q = target_period, target_reduction) %>%
  left_join(m.quarterly, .) ->
  m.quarterly

# Adding a target with 2020Q1 as base period since the original base period is 2019Q1, but we plot yoy
m.quarterly %>%
  filter(source=='target',
         is.na(CityEN),
         base_period==2019.1) %>%
  mutate(target=value_base * (1+target_reduction)) %>%
  select(CityEN, Province, poll, keyregion2018, target) %>%
  left_join(
    m.quarterly %>%
      filter(source=="hourly",
             is.na(CityEN),
             Q==2020.1) %>%
      select(CityEN, Province, poll, keyregion2018, value)
  ) %>%
  mutate(
    target_reduction=target/value -1,
    Q=2021.1,
    base_period=2020.1,
    value_base=value,
    value=target,
    source="target"
  ) %>%
  bind_rows(m.quarterly, .) ->
  m.quarterly


print("PREPARING TARGETS [8]============")
#calculate annual means consistent with targets
m.quarterly %>% filter(source=='hourly') %>%
  select(CityEN, poll, Province, keyregion2018, base_period=Q, value_base = value) %>%
  left_join(m.quarterly, .) %>%
  mutate(value = ifelse(source == 'target',
                        (1 + target_reduction) * value_base,
                        value)) -> m.quarterly

print("PREPARING TARGETS [9]============")
mean4=function(x) ifelse(length(x)==4,mean(x), NA)
targetmean=function(df) {
  df %>% group_by(CityEN, poll, Province, keyregion2018) %>% summarise_at('value', mean4) %>%
    filter(!is.na(value))
}

bind_rows(m.quarterly %>% filter(source == 'hourly', Q %in% c(2020.1, 2020.2, 2020.3)),
          m.quarterly %>% filter(source == 'target', Q %in% c(2020.4))) %>% targetmean -> targetmeans.Q4

bind_rows(m.quarterly %>% filter(source == 'hourly', Q %in% c(2020.2, 2020.3)),
          m.quarterly %>% filter(source == 'target', Q %in% c(2020.4, 2021.1))) %>% targetmean -> targetmeans.Q1

print("PREPARING TARGETS [10]============")
max.date <- max(daily$date)
m.qtd = daily %>% ungroup %>% filter(Q %in% c(2019.4, 2020.4))
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
              select(CityEN, poll, Province, keyregion2018, target_reduction), .) %>%
  mutate(Q=2020.4) %>%
  bind_rows(m.quarterly %>%
              filter(source=='target', Q==2021.1, base_period==2019.1) %>%
              select(CityEN, poll, Province, keyregion2018, target_reduction, Q)) %>%
  filter(is.na(CityEN), !is.na(target_reduction))


t.keyregions.abs <- m.quarterly %>%
  filter(source=='target', Q %in% c(2021.1, 2020.4), is.na(CityEN)) %>%
              select(CityEN, poll, Province, keyregion2018, target=value, Q)

print("PREPARING TARGETS [12]============")
m.keyregions <- m.station.obs %>%
  left_join(stations %>% mutate(location_id=tolower(stationID)) %>% select(location_id, keyregion2018)) %>%
  filter(!is.na(keyregion2018),
         date >= "2018-10-01") %>%
  group_by(location_id=keyregion2018, process_id, date=as.Date(date), poll, timezone, unit, source) %>%
  summarise_at('value', mean, na.rm=T)

#One year running mean plots with the target for end of Q4 and Q1 marked as points, and linear path from latest value to targets
print("PLOTTING TARGETS============")
plots.targets_yoyts_vs_targets(m.keyregions, t.keyregions, folder=folder_regional, en_or_zh="en")
plots.targets_yoyts_vs_targets(m.keyregions, t.keyregions, folder=folder_regional, en_or_zh="zh")



plots.observed_vs_targets(m.keyregions, t.keyregions.abs, folder=folder_regional, en_or_zh="en")
plots.observed_vs_targets(m.keyregions, t.keyregions.abs, folder=folder_regional, en_or_zh="zh")

