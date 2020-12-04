if(!require(remotes)){install.packages("remotes"); require(remotes)}
if(!require(tidyverse)){install.packages("tidyverse"); require(tidyverse)}
remotes::install_github("energyandcleanair/rcrea", force=F, upgrade=T)
require(rcrea)

rcrea::measurements(source="mee", city="beijing", poll="pm25")


