library(dplyr)
library(remotes)
library(tidyverse)
remotes::install_github("energyandcleanair/rcrea")
library(rcrea)
librqry(lubridate)

library(showtext)
# showtext_auto()

mu <- stringi::stri_unescape_unicode("\U00B5")

dir_results <- "results"
dir_results_plots <- "results/plots"

dir.create(dir_results, showWarnings = F, recursive = T)
dir.create(dir_results_plots, showWarnings = F, recursive = T)
