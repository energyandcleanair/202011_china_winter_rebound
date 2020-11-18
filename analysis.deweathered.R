source('./config.R')
source('./utils.R')
source('./plots.R')

stations <- utils.read_stations()
cities <- stations %>%
    filter(keyregion=="Jingjinji" |
             keyregion2018=="2+26" |
             Fenwei,
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


# Custom deweathering ------------------------------------------------------
require(creadeweather)
d <- creadeweather::deweather(source="mee",
                              poll="pm25",
                              output="anomaly",
                              upload_results = F,
                              training_start_anomaly = "2017-03-01")

