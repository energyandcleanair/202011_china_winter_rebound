source('./config.R')
source('./utils.R')
source('./plots.R')

stations <- utils.read_stations()
cities<- stations %>%
    filter(!is.na(keyregion2018),
           CityEN!="<NA>") %>%
    distinct(CityEN)

d.obs <- rcrea::measurements(city=cities$CityEN, source="mee",
                             poll=c(rcrea::PM25,rcrea::PM10, rcrea::NO2, rcrea::SO2, rcrea::O3),
                             deweathered = T,
                             with_metadata = T)

rcrea::plot_recents(meas_raw=d.obs %>% filter(process_id=="anomaly_percent_gbm_lag1_city_mad"),
                    running_days = 30,
                    aggregate_level = "city",
                    source="mee",
                    range="full",
                    size="l",
                    color_by="value",
                    subplot_by = "region_id",
                    subfile_by = "poll",
                    folder=file.path(dir_results_plots, "deweathered"))


# Regional ----------------------------------------------------------------
l <- rcrea::locations(source="mee")

# Which cities are not unique
duplicated_cities <- stations %>%
  distinct(CityEN, Province) %>%
  group_by(CityEN) %>%
  summarise(count=n()) %>%
  filter(count>1) %>%
  pull(CityEN)

# These cities have been wrongly averaged together during scraping,
# so we exclude them from the region averaging
d.obs.regional <- d.obs %>%
  left_join(stations %>% select(CityEN, keyregion2018) %>% mutate(region_id=tolower(CityEN)) ,
            by=c("region_id")) %>%
  filter(!is.na(keyregion2018),
         !tolower(CityEN) %in% tolower(duplicated_cities)) %>%
  # Add weight: 'one station, one vote'
  left_join( stations %>%
               group_by(CityEN, keyregion2018) %>%
               summarise(weight=n())) %>%
  # Average
  group_by(poll, unit, date, process_id, source, timezone, country, keyregion2018, region_name=keyregion2018) %>%
  summarise(value=weighted.mean(value, weight, na.rm=T)) %>%
  rename(region_id=keyregion2018)


rcrea::plot_recents(meas_raw=d.obs.regional %>% filter(process_id=="anomaly_percent_gbm_lag1_city_mad"),
                    running_days = 30,
                    aggregate_level = "region",
                    source="mee",
                    range="full",
                    size="l",
                    color_by="value",
                    subplot_by = "region_id",
                    subfile_by = "poll",
                    folder=file.path(dir_results_plots, "deweathered", "regional"))


# Custom deweathering ------------------------------------------------------
require(creadeweather)
d <- creadeweather::deweather(source="mee",
                              poll="pm25",
                              output="anomaly",
                              upload_results = F,
                              training_start_anomaly = "2017-03-01")

