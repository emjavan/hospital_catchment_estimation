#/////////////////////////////////
# Load all the required R packages
#/////////////////////////////////

# On Lonestar6 corral-protected using the container r_tidycensus_jags_geo.sif
# These packages were downloaded into container on Frontera
#  then copied over. Cannot access protected data on dev node

# Packages here confirmed in container 2024-10-5 by Emily Javan

# Download the preliminary library
if (!require("pacman")){
  install.packages("pacman")
  }

# Dplyover is experimental, so must get from github
# Current code doesn't use this package but I've had good success with it in past
# if(!require("dplyover")){
#   remotes::install_github("TimTeaFan/dplyover")
# }

# Load libraries
pacman::p_load(
  tidyverse, # The tidy universe has packages like ggplot, dplyr, etc
  icecream, # allows print statements that will not print when deactivated
  testthat, # ensure functions actually work
  tidycensus, # download ACS data and geometries
  sf, # for spatial plotting and st_drop_geometry
  stringdist # needed to cluster similar strings
)

