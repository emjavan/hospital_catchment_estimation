#/////////////////////////////////
# Load all the required R packages
#/////////////////////////////////

# On Lonestar6 corral-protected using the container r_tidycensus_jags_geo.sif
# These packages were downloaded into container on Frontera
#  then copied over. Cannot access protected data on dev node

# Packages here confirmed in container 2024-10-5 by Emily Javan

# Download the preliminary library
#if (!require("pacman")){
#  install.packages("pacman")
#  }

# Dplyover is experimental, so must get from github
# Current code doesn't use this package but I've had good success with it in past
# if(!require("dplyover")){
#   remotes::install_github("TimTeaFan/dplyover")
# }

# Load libraries
#pacman::p_load(
#  tidyverse, # The tidy universe has packages like ggplot, dplyr, etc
#  icecream, # allows print statements that will not print when deactivated
#  testthat, # ensure functions actually work
#  tidycensus, # download ACS data and geometries
#  sf, # for spatial plotting and st_drop_geometry
#  pdftools # extract data from PDF
#)







# Define writable local library path
local_lib <- Sys.getenv("HOME") # Or explicitly set: "/work2/06778/emj976/Rlibs"
writable_lib <- file.path(local_lib, "Rlibs")

# Ensure local library path exists
dir.create(writable_lib, showWarnings = FALSE, recursive = TRUE)

# Set R to use only container libraries first (avoids Intel MKL errors)
.libPaths(c("/usr/local/lib/R/site-library", "/usr/local/lib/R/library"))

# Function to install missing packages in the writable library
install_if_missing <- function(pkgs) {
  missing_pkgs <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if (length(missing_pkgs) > 0) {
    message("Installing missing packages to writable library: ", writable_lib)
    install.packages(missing_pkgs, lib = writable_lib, repos = "https://cran.r-project.org")
  }
}

# List required packages
required_pkgs <- c("pacman", "tidyverse", "icecream", "testthat", "tidycensus", "sf",
                   "broom", "emmeans", "ggrepel", "boot", "gt")

# Install missing packages in writable directory
install_if_missing(required_pkgs)

# Now allow R to load both the writable directory and container libraries
.libPaths(c(writable_lib, .libPaths()))



# Load libraries
pacman::p_load(
  tidyverse, # The tidy universe has packages like ggplot, dplyr, etc
  icecream, # allows print statements that will not print when deactivated
  testthat, # ensure functions actually work
  tidycensus, # download ACS data and geometries
  sf, # for spatial plotting and st_drop_geometry
  broom, # makes things tidy
  emmeans, # means of data
  ggrepel, # repel point labels from each other
  boot, # bootstrap data
  gt # for the graphical table
)





