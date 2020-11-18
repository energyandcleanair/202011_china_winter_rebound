source('./config.R')
source('./utils.R')
source('./plots.R')

stations <- utils.read_stations()

(cities <- stations %>%
    filter(!is.na(keyregion2018),
           CityEN!="<NA>") %>%
    distinct(CityEN))

duplicated_cities <- utils.check_cities_unique(cities, stations)
cities <- cities %>% filter(!CityEN %in% duplicated_cities)



# Observations: City level ------------------------------------------------------------

if(F){
  m.obs <- readRDS(file.path(dir_results, "m.obs.RDS"))
}else{
  m.obs <- rcrea::measurements(city=cities$CityEN, source="mee",
                               date_from="2017-01-01",
                               poll=c(rcrea::PM25, rcrea::NO2, rcrea::SO2, rcrea::O3),
                               deweathered = F,
                               with_metadata = T)

  saveRDS(m.obs, file.path(dir_results, "m.obs.RDS"))
}

# Plot 1: TS City 30-day
rcrea::plot_recents(meas_raw=m.obs %>% filter(date>="2018-01-01"),
                    color_by="year",
                    subfile_by = "poll",
                    running_days = 30,
                    subplot_by = "region_id",
                    size="l",
                    range="full",
                    source="mee",
                    folder = file.path(dir_results_plots, "city", "EN")
)

rcrea::plot_recents(meas_raw=m.obs %>% filter(date>="2018-01-01") %>% utils.replace_w_chinese(stations),
                    color_by="year",
                    subfile_by = "poll",
                    running_days = 30,
                    subplot_by = "region_id",
                    size="l",
                    range="full",
                    source="mee",
                    add_to_ggplot=theme(text=element_text(family="STFangsong")),
                    folder = file.path(dir_results_plots, "city", "ZH")
)

# Plot 2: TS City 365-day
rcrea::plot_recents(meas_raw=m.obs %>% filter(date>="2017-01-02"),
                    color_by="year",
                    subfile_by = "poll",
                    running_days = 365,
                    subplot_by = "region_id",
                    size="l",
                    range="full",
                    source="mee",
                    folder = file.path(dir_results_plots, "city", "EN")
)

rcrea::plot_recents(meas_raw=m.obs %>% filter(date>="2017-01-02")%>% utils.replace_w_chinese(stations),
                    color_by="year",
                    subfile_by = "poll",
                    running_days = 365,
                    subplot_by = "region_id",
                    size="l",
                    range="full",
                    source="mee",
                    add_to_ggplot=theme(text=element_text(family="STFangsong")),
                    folder = file.path(dir_results_plots, "city", "ZH")
)

# Plot 3: YOY City 30-day
rcrea::plot_recents(meas_raw=m.obs %>% filter(date>="2018-12-01"),
                    type="yoy-relative",
                    subfile_by = "poll",
                    running_days = 30,
                    subplot_by = "region_id",
                    color_by="value",
                    size="l",
                    range="full",
                    source="mee",
                    folder = file.path(dir_results_plots, "city", "EN")
)

rcrea::plot_recents(meas_raw=m.obs %>% filter(date>="2018-12-01"),
                    type="yoy-relative",
                    subfile_by = "poll",
                    running_days = 30,
                    subplot_by = "region_id",
                    color_by="value",
                    size="l",
                    range="full",
                    source="mee",
                    add_to_ggplot=theme(text=element_text(family="STFangsong")),
                    folder = file.path(dir_results_plots, "city", "ZH")
)

plots.compare_past_years(m.obs,
                         poll=rcrea::PM25,
                         folder = file.path(dir_results_plots, "city")
)


# Regional --------------------------------------------
if(F){
  m.station.obs <- readRDS(file.path(dir_results, "m.station.obs.RDS"))
}else{
  station_ids <- stations %>% filter(!is.na(keyregion2018)) %>% pull(station_code)
  m.station.obs <- rcrea::measurements(source="mee",
                                       location_id=station_ids,
                                       process_id="station_day_mad",
                                       date_from="2018-12-01",
                                       poll=c(rcrea::PM25, rcrea::NO2, rcrea::SO2, rcrea::O3),
                                       deweathered = F)

  saveRDS(m.station.obs, file.path(dir_results, "m.station.obs.RDS"))
}

m.station.obs.rich <- m.station.obs %>%
  mutate(region_id=toupper(region_id)) %>%
  left_join(stations %>% select(station_code, keyregion2018), by=c("region_id"="station_code")) %>%
  filter(!is.na(keyregion2018))


m.region <- m.station.obs.rich %>%
  group_by(date=lubridate::date(date), poll, unit, region_id=tolower(keyregion2018), process_id, source, timezone) %>%
  dplyr::summarise(value=mean(value, na.rm=T)) %>%
  mutate(country="CN",
         region_name=tools::toTitleCase(region_id))


# Plot 1: YOY 30-day
rcrea::plot_recents(meas_raw=m.region %>% filter(date>="2018-12-01"),
                    type="yoy-relative",
                    subfile_by = "poll",
                    running_days = c(30),
                    subplot_by = "region_id",
                    color_by="value",
                    size="l",
                    range="full",
                    source="mee",
                    folder = file.path(dir_results_plots, "regional")
)


# National ----------------------------------------------------------------

m.national <- m.station.obs %>%
  group_by(date, poll, unit, region_id="china", process_id, source, timezone) %>%
  summarise(value=mean(value, na.rm=T)) %>%
  mutate(country="CN",
         region_name=tools::toTitleCase(region_id))


rcrea::plot_recents(meas_raw=m.national %>% filter(date>="2018-11-30"),
                    type="yoy-relative",
                    subplot_by = "poll",
                    running_days = c(30),
                    subfile_by = "country",
                    color_by="value",
                    size="l",
                    source="mee",
                    folder = file.path(dir_results_plots, "national")
)
