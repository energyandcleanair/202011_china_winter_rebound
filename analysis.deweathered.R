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


if(F){
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
                                                     training_end_anomaly = "2019-06-30")

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
                                                        years_force_refresh = NULL,
                                                        training_start_anomaly = "2017-04-01",
                                                        training_end_anomaly = "2019-06-30")

  m.dew.station <-  m.dew.station.raw %>%
    tidyr::unnest(normalised) %>%
    select(-c(process_deweather, process_id, predicted)) %>%
    left_join(stations %>% mutate(region_id=tolower(station_code))) %>%
    mutate(region_id=tolower(CityEN)) %>%
    group_by(region_id, date, poll, unit, process_id=output, CityZH, Province, keyregion2018) %>%
    summarize_at("value", mean, na.rm=T)

  m.dew <- bind_rows(m.dew.city, m.dew.station)
  saveRDS(m.dew, file.path(dir_results, "m.dew.RDS"))
}else{
  m.dew <- readRDS(file.path(dir_results, "m.dew.RDS"))
}




# Plot 1: City level

rcrea::plot_recents(meas_raw=m.dew %>% filter(process_id=="anomaly_percent"),
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

m.regions.obs <- m.obs %>%
  left_join(stations %>% mutate(region_id=tolower(CityEN)) %>% distinct(region_id, keyregion2018)) %>%
  filter(!is.na(keyregion2018)) %>%
  group_by(region_id=keyregion2018, process_id, date=as.Date(date), poll, timezone, unit, source) %>%
  summarise_at('value', mean, na.rm=T)

# Sanity check

m.sanity <-
  bind_rows(
    m.regions.obs %>% mutate(type="Observations"),
    m.dew.regional %>% filter(process_id=="anomaly_offsetted") %>% mutate(type="Deweathered"),
    m.dew.regional %>% filter(process_id=="anomaly_counterfactual") %>% mutate(type="Counterfactual")) %>%
  filter(poll=="pm25")

ggplot(m.sanity %>% rcrea::utils.running_average(30), aes(date,value)) +
  geom_line(aes(col=type,linetype=type)) +
  facet_wrap(~region_id, scales="free_y")


m.equivalent <-
  bind_rows(
    m.keyregions %>% filter(date<=min(m.dew$date)),
    m.dew.regional %>% filter(process_id=="anomaly_percent"))


rcrea::plot_recents(meas_raw=m.equivalent,
                    running_days = 30,
                    aggregate_level = "region",
                    source="mee",
                    range="full",
                    size="l",
                    color_by="value",
                    subplot_by = "region_id",
                    subfile_by = "poll",
                    folder=file.path(dir_results_plots, "deweathered", "regional"))



plots.quarter_anomalies(m.dew.regional, "absolute", folder=file.path(dir_results_plots, "deweathered", "regional"))
plots.quarter_anomalies(m.dew.regional, "relative", folder=file.path(dir_results_plots, "deweathered", "regional"))


# Regional with targets ---------------------------------------------------

plots.targets_yoyts_vs_targets(
  m.keyregions=m.dew.regional %>%
    filter(process_id=="anomaly_offsetted") %>%
    mutate(date=as.Date(date)) %>%
    ungroup(),
  t.keyregions,
  folder=file.path(dir_results_plots, "deweathered", "regional"))
