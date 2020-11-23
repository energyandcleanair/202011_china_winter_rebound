library(dplyr)
library(remotes)
library(tidyverse)
remotes::install_github("energyandcleanair/rcrea")
library(rcrea)


# Parameters (filled in python engine)  -----------------------------------
folder <- {folder}



# Getting data ------------------------------------------------------------
stations <- read.csv("https://github.com/energyandcleanair/202011_china_winter_rebound/raw/main/data/station_key2018.csv")
# station_ids <- stations %>% filter(!is.na(keyregion2018)) %>% pull(station_code)
m.station.obs <- rcrea::measurements(source="mee",
                                     # location_id=station_ids,
                                     process_id="station_day_mad",
                                     date_from="2018-10-01",
                                     poll=c(rcrea::PM25, rcrea::NO2, rcrea::SO2, rcrea::O3),
                                     deweathered = F)

m.station.obs.rich <- m.station.obs %>%
  mutate(region_id=toupper(region_id)) %>%
  left_join(stations %>% select(station_code, keyregion2018), by=c("region_id"="station_code"))


m.region <- m.station.obs.rich %>%
  filter(!is.na(keyregion2018)) %>%
  group_by(date=lubridate::date(date), poll, unit, region_id=tolower(keyregion2018), process_id, source, timezone) %>%
  dplyr::summarise(value=mean(value, na.rm=T)) %>%
  mutate(country="CN",
         region_name=tools::toTitleCase(region_id))


# Plots -------------------------------------------------------------------



# Plot 1: YOY 30-day
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
                    folder = folder
)


