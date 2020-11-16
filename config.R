require(dplyr)
require(remotes)
remotes::install_github("energyandcleanair/rcrea")
require(rcrea)


dir_results <- "results"
dir_results_plots <- "results/plots"

dir.create(dir_results, showWarnings = F, recursive = T)
dir.create(dir_results_plots, showWarnings = F, recursive = T)
