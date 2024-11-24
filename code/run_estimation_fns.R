#////////////////////////////////////////////////////////////////////////////
# Calculate a hospital's catchment population based on the proportion of
#  ZCTAs that visit it for a given set of ICD-10 codes
# The functions assume you are providing DSHS PUDF data 
#  or the provided synthetic data
# Emily Javan - ATX - 2024-11-23
#////////////////////////////////////////////////////////////////////////////

#////////////////////////////////////
#### STANDARDIZE PUDF FILE NAMES ####
#////////////////////////////////////
stand_files_names = FALSE # set to true if you using original PUDF files from DSHS
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
walk(files_to_source, source)

# Make dir for figs if it doesn't exist
fig_dir="../figures/"
if(!dir.exists(fig_dir)){
  dir.create(fig_dir)
} # end if fig dir not made

#////////////////////////
#### GET ICD-10 DATA ####
#////////////////////////
# ICD-10 code disease categorization
# This file is created by the user following the appropriate format
icd10_df = read_csv("../input_data/icd10_disease_category_list.csv")

#////////////////////////////////////////
#### CATEGORIZE PUDF DATA BY DISEASE ####
#////////////////////////////////////////
date_range = c("2022Q3", "2023Q2")
cat_pudf_from_fn = measure_run_time(
  categorize_patients_by_disease,
  date_range = date_range,
  icd10_df = icd10_df
)

#////////////////////
#### GET ZIP POP ####
#////////////////////
# P_i population per ZIP code from 2018-2022 ACS
tx_zcta_city_pop = get_zcta_acs_pop()

#////////////////////////////
#### GET ZCTA-HOSP PAIRS ####
#////////////////////////////
output_folder_path = "../private_input_data/ICD10_CAT_PUDF_DATA/"
zcta_hosp_pairs = 
  count_patients_zcta_hosp_pairs(
    date_range = c("2022Q3", "2023Q2"),
    icd10_df
  )

catchment_output_folder = "../produced_data/HOSP_CATCHMENTS"
  calculate_hospital_catchments(
    geom_hosp_df, 
    geom_col, 
    hosp_col, 
    pat_count_col
  )



