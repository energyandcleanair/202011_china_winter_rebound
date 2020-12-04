
remotes::install_github("energyandcleanair/rcrea", force=F, upgrade=T)
require(rcrea)

rcrea::measurements(source="mee", city="beijing", poll="pm25")


