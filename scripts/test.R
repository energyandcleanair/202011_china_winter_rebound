if(!require(remotes)){install.packages("remotes"); require(remotes)}
if(!require(tidyverse)){install.packages("tidyverse"); require(tidyverse)}

remotes::install_github("energyandcleanair/rcrea", force=F, upgrade=T)
library(rcrea)
Sys.setenv("TZ"="Etc/UTC"); #https://github.com/rocker-org/rocker-versioned/issues/89


print("GETTING MEASUREMENTS CITIES============")
m.station.obs <- rcrea::measurements(source="mee",
                                     location_id="beijing",
                                     process_id="city_day_mad",
                                     date_from="2020-10-01",
                                     poll=c(rcrea::PM25),
                                     deweathered = F)

print("GETTING MEASUREMENTS STATIONS============")
m.station.obs <- rcrea::measurements(source="mee",
                                     location_id=c("1001A","1002A"),
                                     process_id="station_day_mad",
                                     date_from="2020-10-01",
                                     poll=c(rcrea::PM25),
                                     deweathered = F)
