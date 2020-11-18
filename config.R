library(dplyr)
library(remotes)
library(tidyverse)
remotes::install_github("energyandcleanair/rcrea")
library(rcrea)

library(showtext)
showtext_auto()

dir_results <- "results"
dir_results_plots <- "results/plots"

dir.create(dir_results, showWarnings = F, recursive = T)
dir.create(dir_results_plots, showWarnings = F, recursive = T)
