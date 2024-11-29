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
frontera = FALSE
if(frontera){
  # This takes so long to run had to change to parallel script on Frontera
  args       = commandArgs(TRUE)
  date       = as.character(args[1])
  date_range = c(date, date) # e.g. "2023Q1", "2023Q1"
  cat_pudf_from_fn = measure_run_time(
    categorize_patients_by_disease,
    date_range = date_range,
    icd10_df = icd10_df
  )
} # end if running locally or on TACC Frontera

#////////////////////
#### GET ZIP POP ####
#////////////////////
# P_i population per ZIP code from 2018-2022 ACS w/ geometry=T default
# You can group by ZIPName (default) or PlaceName to assign ZIPs to cities
#  but be aware many ZCTA are "not a place" i.e. not a Census Designated Place
tx_zcta_city_pop = get_zcta_acs_pop()

# Should not be greater than 1
zcta_unqiue = tx_zcta_city_pop %>%
  sf::st_drop_geometry() %>% # geometry col really slows this down
  group_by(ZCTA) %>%
  summarise(times_zcta_occurs = n()) %>%
  ungroup() %>%
  arrange(desc(times_zcta_occurs))

# These seem close and maybe a little under what you'd google given we
#  force a ZCTA to belong to only one city
zcta_per_city = tx_zcta_city_pop %>%
  sf::st_drop_geometry() %>% # geometry col really slows this down
  group_by(CITY_NAME) %>%
  summarise(total_zcta_per_city = n()) %>%
  ungroup()

#////////////////////////////
#### GET ZCTA-HOSP PAIRS ####
#////////////////////////////
# Example if you wish to pass multiple ICD-10 code folders
# input_folder_path <- c("../private_input_data/PUDF_FLU/",
#                    "../private_input_data/PUDF_COV/")
input_folder_path = "../private_input_data/PUDF_FLU/"
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
tx_zcta_city_pop_no_geo = tx_zcta_city_pop %>%
  sf::st_drop_geometry()

hosp_catchments = 
  calculate_hospital_catchments(
    geom_hosp_df = zcta_hosp_pairs, 
    geom_col = "PAT_ZIP", 
    hosp_col = "THCIC_ID", 
    pat_count_col = "PAT_COUNT",
    date_col = "DATE_RANGE", # optional but recommended
    disease_col = "DISEASE_INCLUDED", # optional but recommended
    population_df = tx_zcta_city_pop_no_geo, 
    pop_geom_col = "ZCTA",
    pop_col = "estimate",
    calculation_outputfile_path = calculation_outputfile_path,
    catchment_outputfile_path   = catchment_outputfile_path
  )

#////////////////////////////////////
#### GET HOSP LOCATION FROM NAME ####
#////////////////////////////////////

hosp_thcic_to_ccn = read_csv("../private_input_data/thcic_id_to_ccn_hospitals_through_time.csv") %>%
  mutate(THCIC_ID = as.character(THCIC_ID),
         THCIC_ID_pad = str_pad(THCIC_ID, width=6, side="left", pad="0")) %>%
  select(THCIC_ID_pad, everything())

hosp_catchments$HOSPITAL[which(!(hosp_catchments$HOSPITAL %in% hosp_thcic_to_ccn$THCIC_ID_pad))]


#### Hosp Data from CMS ####

# Hand-made dictionary of replacements
str_name_dict = read_csv("../input_data/StreetName_Replacements.csv")

hosp_pos_df = 
  read_csv("../input_data/ProviderOfService_Files/POS_File_Hospital_Non_Hospital_Facilities_Q4_2023.csv") %>%
  filter(STATE_CD == "TX")


cms_hosp_df = hosp_pos_df %>% # 10532 rows
  mutate(
    FAC_NAME_clean = clean_text_column(FAC_NAME)
  ) %>%
  dplyr::select(STATE_CD, CITY_NAME, ST_ADR, ZIP_CD, FAC_NAME_clean, everything()) %>%
  mutate(across(STATE_CD:FAC_NAME_clean, as.character),
         ST_ADR_clean = norm_street_address(clean_text_column(ST_ADR), str_name_dict),
         ST_ADR_rm_suffix = remove_street_address_suffix(ST_ADR_clean)
         ) %>%
  # Not removing any unique facility names even at same address bc
  #  those names may match the PUDF facility file
  group_by(STATE_CD, CITY_NAME, ST_ADR_rm_suffix, ZIP_CD, FAC_NAME_clean) %>%
  arrange(desc(CRTFCTN_DT), .by_group = T) %>% 
  slice(1) %>% # take only most recent certification date if all other location details the same
  ungroup() %>% # 9691 rows
  dplyr::select(STATE_CD, CITY_NAME, ST_ADR_rm_suffix, ZIP_CD, FAC_NAME_clean)
  arrange(STATE_CD, CITY_NAME, ST_ADR, ST_ADR_clean, ST_ADR_rm_suffix, ZIP_CD, FAC_NAME_clean, everything())
  
  
#### Hosp Data from PUDF ####
facility_df = 
  read_delim("../private_input_data/OG_PUDF_DATA/PUDF_2023Q4_tab-delimited/IP_PUDF_FACILITY_TYPE_2023Q4_tab.txt",
             delim="\t")
pudf_hosp_df = facility_df %>%
  dplyr::select(-starts_with("FAC_"), -starts_with("POA_"), -starts_with("CERT_")) %>%
  mutate(
    PROVIDER_NAME_clean = PROVIDER_NAME %>%
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = "") %>% # Remove non-ASCII characters
      gsub(" - ", " ", .) %>% # Replace hyphen with spaces around it
      gsub("-", " ", .) %>% # Replace hyphen without spaces with a space
      gsub("[&,'\\.]", "", .) %>% # Remove &, commas, apostrophes, and periods
      gsub("  ", " ", .) %>% # Replace any double spaces with single
      toupper() # Convert to uppercase
  )  %>%
  dplyr::select(PROVIDER_NAME_clean, PROVIDER_NAME, everything())


match_facility_names = pudf_hosp_df %>%
  select(THCIC_ID, PROVIDER_NAME_clean, FACILITY_TYPE) %>%
  left_join(cms_hosp_df, by=c("PROVIDER_NAME_clean"="FAC_NAME_clean")) %>%
  # Choose match with most recent certification number date
  group_by(THCIC_ID) %>%
  arrange(desc(CRTFCTN_DT), .by_group = T) %>%
  slice(1) %>%
  ungroup() 

not_found_fac = match_facility_names %>%
  filter(is.na(FAC_NAME)) %>%
  dplyr::select(THCIC_ID, PROVIDER_NAME_clean, FACILITY_TYPE) %>%
  mutate(PROVIDER_NAME_clean = gsub("UT ", "UNIVERSITY OF TEXAS ", PROVIDER_NAME_clean)   )


hosp_catchments$HOSPITAL[which((hosp_catchments$HOSPITAL %in% not_found_fac$THCIC_ID))]








