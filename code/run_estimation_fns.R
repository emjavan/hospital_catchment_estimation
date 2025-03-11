#////////////////////////////////////////////////////////////////////////////
# Calculate a hospital's catchment population based on the proportion of
#  ZCTAs that visit it for a given set of ICD-10 codes
# The functions assume you are providing DSHS PUDF data 
#  or the provided synthetic data
# Emily Javan - ATX - 2025-02-24
#////////////////////////////////////////////////////////////////////////////

#////////////////////////////////////
#### STANDARDIZE PUDF FILE NAMES ####
#////////////////////////////////////
stand_files_names = FALSE # set to true if using original PUDF files from DSHS
if(stand_files_names){
  # Run bash script to clean input file names
  bash_script = "standardize_PUDF_file_names.sh"
  result <- system2("bash", args = bash_script, stdout = TRUE, stderr = TRUE)
  # They'll all say "Skipping: ..." if file was already cleaned or not matched
  cat(result, sep = "\n") 
}

#////////////////////////
#### SOURCE FUNCTIONS ###
#////////////////////////
# List of file paths to source
files_to_source <- c(
  "get_packages_used.R",
  "estimation_fns.R",
  # need to add function tests once I have them
  "../private_input_data/api_keys.R"
)
# Source each file
purrr::walk(files_to_source, source)

#////////////////////////
#### ICD-10 DATA ####
#////////////////////////
# ICD-10 code disease categorization
# This file is created by the user following the appropriate format
icd10_df = read_csv("../input_data/icd10_disease_category_list.csv")

#////////////////////////////////////////
#### CATEGORIZE PUDF DATA BY DISEASE ####
#////////////////////////////////////////
LS6 = TRUE
if(LS6){
  # This takes so long to run had to change to parallel script on LS6
  args       = commandArgs(TRUE)
  date       = as.character(args[1])
  date_range = c(date, date) # e.g. "2023Q1", "2023Q1"
  cat_pudf_from_fn = measure_run_time(
    categorize_patients_by_disease,
    date_range = date_range,
    icd10_df = icd10_df
  )
} # end if running locally or on TACC LS6

#/////////////////////
#### ZCTA POP ####
#/////////////////////
# P_i population per ZIP code from 2018-2022 ACS w/ geometry=T default
# You can group by ZIPName (default) or PlaceName to assign ZIPs to cities
#  but be aware many ZCTA are "not a place" i.e. not a Census Designated Place
#tx_zcta_city_pop = get_zcta_acs_pop(state="TX")
us_zcta_city_pop = get_zcta_acs_pop(state="US") # takes awhile to do all US first time 

# Should not be greater than 1 for any ZCTA
zcta_unqiue = us_zcta_city_pop %>%
  sf::st_drop_geometry() %>% # geometry col really slows this down
  group_by(ZCTA) %>%
  summarise(times_zcta_occurs = n()) %>%
  ungroup() %>%
  arrange(desc(times_zcta_occurs))
if(sum(zcta_unqiue$times_zcta_occurs) > length(zcta_unqiue$times_zcta_occurs)){
  rlang::warn("Expect one city name per ZCTA")
}

# ZCTA count seem close and maybe a little under what you'd Google given we
#  force a ZCTA to belong to only one city
zcta_per_city = us_zcta_city_pop %>%
  sf::st_drop_geometry() %>% # geometry col really slows this down
  group_by(CITY_NAME) %>%
  summarise(total_zcta_per_city = n()) %>%
  ungroup()

#////////////////////////////
#### ZCTA-HOSP PAIRS ####
#////////////////////////////
# Example if you wish to pass multiple ICD-10 code folders
# The output path gives diseases alphabetically A-B-C

# R crashes when I run over long time period which I assume is due to 
#  needing distinct RECORD_ID's +
#  too much data for local machine??? Will try on LS6

input_folder_path <- c(
  "../private_input_data/PUDF_FLU/",
  "../private_input_data/PUDF_RSV/",
  "../private_input_data/PUDF_ILI/"
  #"../private_input_data/PUDF_COV/"
  )
# input_folder_path = "../private_input_data/PUDF_FLU/"
output_folder_path = "../private_results/"
if(!dir.exists(output_folder_path)){dir.create(output_folder_path)}
zcta_hosp_pairs = 
  count_patients_zcta_hosp_pairs(
    date_range = c("2022Q3", "2023Q2"),
    input_folder_path = input_folder_path,
    output_folder_path = output_folder_path
  )

#////////////////////////////
#### ESTIMATE CATCHMENTS ####
#////////////////////////////
catchment_output_folder = paste0(output_folder_path, "HOSP_CATCHMENTS/")
if(!dir.exists(catchment_output_folder)){dir.create(catchment_output_folder)}
file_suffix = paste0(zcta_hosp_pairs$DISEASE_INCLUDED[1], "_", 
                     zcta_hosp_pairs$DATE_RANGE[1], ".csv")
calculation_outputfile_path = paste0(catchment_output_folder, "HOSP-CATCH-CALC_", file_suffix)
catchment_outputfile_path = paste0(catchment_output_folder, "HOSP-POP-CATCH_", file_suffix)

# remove polygons for ZCTA when writing a csv
us_zcta_city_pop_no_geo = us_zcta_city_pop %>%
  sf::st_drop_geometry()

hosp_catchments = 
  calculate_hospital_catchments(
    geom_hosp_df = zcta_hosp_pairs, 
    geom_col = "PAT_ZCTA", 
    hosp_col = "THCIC_ID", 
    pat_count_col = "PAT_COUNT",
    date_col = "DATE_RANGE", # optional but recommended
    disease_col = "DISEASE_INCLUDED", # optional but recommended
    population_df = us_zcta_city_pop_no_geo, 
    pop_geom_col = "ZCTA",
    pop_col = "estimate",
    calculation_outputfile_path = calculation_outputfile_path,
    catchment_outputfile_path   = catchment_outputfile_path
  )



  




