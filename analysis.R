source('./config.R')
source('./utils.R')
source('./plots.R')

stations <- utils.read_stations()

(cities <- keys %>%
    filter(keyregion=="Jingjinji" |
             keyregion2018=="2+26" |
             Fenwei,
             CityEN!="<NA>") %>%
  distinct(CityEN))

utils.check_cities_unique(cities)



# Observations ------------------------------------------------------------

m.obs <- rcrea::measurements(city=cities$CityEN, source="mee",
                             date_from="2017-01-01",
                             poll=c(rcrea::PM25,rcrea::PM10),
                             deweathered = F,
                             with_metadata = T)

saveRDS(m.obs, file.path(dir_results, "m.obs.RDS"))

rcrea::plot_recents(meas_raw=m.obs %>% filter(date>="2018-01-01"),
                    color_by="year",
                    subfile_by = "poll",
                    running_days = c(14,30),
                    subplot_by = "region_id",
                    size="l",
                    folder = dir_results_plots
                    )

rcrea::plot_recents(meas_raw=m.obs %>% filter(date>="2017-01-02"),
                    color_by="year",
                    subfile_by = "poll",
                    running_days = 365,
                    subplot_by = "region_id",
                    size="l",
                    folder = dir_results_plots
)

plots.compare_past_years(m.obs,
                         poll=rcrea::PM25,
                         folder=dir_results_plots
                         )
