source('./config.R')
source('./utils.R')
source('./plots.R')

stations <- utils.read_stations()
cities<- stations %>%
    filter(!is.na(keyregion2018),
           CityEN!="<NA>") %>%
    distinct(CityEN)
duplicated_cities <- utils.check_cities_unique(cities, stations)
cities <- cities %>% filter(!CityEN %in% duplicated_cities)


if(T){
  # To be run on server ideally.

  # Unique identifier of city-level data is city and country... which is an issue given
  # the duplicated cities.
  # As a fix, we're using city-level data for non-duplicated cities
  # and station level for duplicated cities.
  # NOTE: station level deweathering is time-consuming, that's why we've run it only on duplicated cities

  require(creadeweather)
  m.dew.city.raw <- creadeweather::deweather(source="mee",
                                                     city=cities$CityEN,
                                                     poll="pm25",
                                                     output="anomaly",
                                                     upload_results = F,
                                                     years_force_refresh = NULL,
                                                     training_start_anomaly = "2017-04-01",
                                                     training_end_anomaly = "2019-09-30")

  m.dew.city <- m.dew.city.raw %>%
    tidyr::unnest(normalised) %>%
    select(-c(process_deweather, process_id, predicted)) %>%
    left_join(stations %>% mutate(region_id=tolower(CityEN))) %>%
    select(region_id, date, poll, unit, process_id=output, CityZH, keyregion2018, value)


  m.dew.station.raw <- creadeweather::deweather(source="mee",
                                                        city=duplicated_cities,
                                                        aggregate_level = "station",
                                                        poll="pm25",
                                                        output="anomaly",
                                                        upload_results = F,
                                                        years_force_refresh = NULL,
                                                        training_start_anomaly = "2017-04-01",
                                                        training_end_anomaly = "2019-09-30")

  m.dew.station <-  m.dew.station.raw %>%
    tidyr::unnest(normalised) %>%
    select(-c(process_deweather, process_id, predicted)) %>%
    left_join(stations %>% mutate(region_id=tolower(station_code))) %>%
    mutate(region_id=tolower(CityEN)) %>%
    group_by(region_id, date, poll, unit, process_id=output, CityZH, keyregion2018) %>%
    summarize_at("value", mean, na.rm=T)

  m.dew <- bind_rows(m.dew.city, m.dew.station)
  saveRDS(m.dew, file.path(dir_results, "m.dew.RDS"))
}else{
  m.dew <- readRDS(file.path(dir_results, "m.dew.RDS"))
}


# Plot 1: City level
rcrea::plot_recents(meas_raw=m.dew %>% filter(process_id=="anomaly_percent_gbm_lag1_city_mad"),
                    running_days = 30,
                    aggregate_level = "city",
                    source="mee",
                    range="full",
                    size="l",
                    color_by="value",
                    subplot_by = "region_id",
                    subfile_by = "poll",
                    folder=file.path(dir_results_plots, "deweathered", "city"))

# Regional ----------------------------------------------------------------
m.dew.regional <- m.dew %>%
  # Average
  group_by(poll, unit, date, process_id, region_id=keyregion2018, region_name=keyregion2018, country="CN") %>%
  summarise_at("value", mean, na.rm=T) %>%
  filter(!is.na(region_id))


rcrea::plot_recents(meas_raw=m.dew.regional %>% filter(process_id=="anomaly_percent_gbm_lag1_city_mad"),
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

