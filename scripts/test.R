if(!require(remotes)){install.packages("remotes"); require(remotes)}
if(!require(tidyverse)){install.packages("tidyverse"); require(tidyverse)}

remotes::install_github("energyandcleanair/rcrea", force=F, upgrade=T)
library(rcrea)
Sys.setenv("TZ"="Etc/UTC"); #https://github.com/rocker-org/rocker-versioned/issues/89


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
