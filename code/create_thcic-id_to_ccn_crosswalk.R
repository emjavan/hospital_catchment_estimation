#////////////////////////////////////////////////////////////////////////////
# The PUDF facility file only gives hospital name to identify THCIC_ID's
# This file uses TX hospital name datasets from multiple sources to find
#  the best match to PUDF names listed
# Names change as hospitals are bought or close, etc.so do not expect a new
#  year of data to have the same hospitals
# => re-make crosswalk with new data as time goes on
# Emily Javan - 2024-11-27 - ATX
#////////////////////////////////////////////////////////////////////////////

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### SOURCE FUNCTIONS ####
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

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### DICTIONARIES ####
# Hand-made dictionary of replacements
# Ensuring any white space in CSV matches encoding in R (was having issues)
street_name_dict = read_csv("../input_data/StreetName_Replacements.csv") %>%
  mutate(
    OG_NAME = str_trim(OG_NAME), # Remove leading/trailing spaces
    OG_NAME = str_replace_all(OG_NAME, "\\s+", " ") # Replace multiple spaces with a single space
  )
hosp_name_dict  = read_csv("../input_data/HospitalName_Replacements.csv") %>%
  mutate(
    OG_NAME = str_trim(OG_NAME), # Remove leading/trailing spaces
    OG_NAME = str_replace_all(OG_NAME, "\\s+", " ") # Replace multiple spaces with a single space
  )

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### PUDF Hosp Data ####
pudf_fac_file_path = "../private_input_data/ALL_PUDF_FACILITIES_2015Q3-2023Q4.csv"
if(!file.exists(pudf_fac_file_path)){
  # Define the OG PUDF folder path
  base_pudf_path <- "../private_input_data/OG_PUDF_DATA"
  
  # Find all files in folders containing "facility", (?i) for case sensitivity
  facility_files = fs::dir_ls(base_pudf_path, recurse = TRUE, regexp = "(?i)facility.*\\.txt$")
  facility_df = facility_files %>% # Read and row-bind all files
    map(~ read_delim(.x, delim = "\t") %>%
          dplyr::select(-starts_with("FAC_"), -starts_with("POA_"), -starts_with("CERT_"), -starts_with("."))) %>%
    bind_rows() %>%
    distinct() # Ensure distinct rows, but may still have duplicates due to name changes
  
  # Change from facility to PUDF so it's obvious below the origin of the hospital names
  pudf_hosp_df = facility_df %>%
    mutate(THCIC_ID = as.character(THCIC_ID),
           THCIC_ID = str_pad(THCIC_ID, width=6, side="left", pad="0"),
           PROVIDER_NAME_clean = norm_name_with_dict(clean_text_column(PROVIDER_NAME), hosp_name_dict),
           PROVIDER_NAME_no_space = gsub(" ", "", PROVIDER_NAME_clean)
    ) %>%
    group_by(THCIC_ID) %>%
    # FACILITY_TYPE is a new column, so older hospitals don't have this if not in new files
    arrange(FACILITY_TYPE, .by_group = T) %>%
    # Keep a copy of all names associated with a THCIC_ID
    mutate(
      PROVIDER_NAME_clean_list = paste(unique(PROVIDER_NAME_clean), collapse = "; ")
    ) %>%
    slice(1) %>%
    ungroup() %>% # 933 rows
    # Keep most recent THCIC_ID associated with name and put the other in list to keep track
    group_by(PROVIDER_NAME_no_space) %>%
    arrange(FACILITY_TYPE, .by_group = T) %>%
    mutate(
      THCIC_ID_list = paste(unique(THCIC_ID), collapse = "; ")
    ) %>%
    slice(1) %>%
    ungroup() # 922
  
  write.csv(pudf_hosp_df, 
            pudf_fac_file_path,
            row.names = F
  )
}else{
  pudf_hosp_df = read_csv(pudf_fac_file_path)
} # end if clean df of all PUDF facilities exists

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### ALL FOUND FACILITIES ####
#/////////////////////////////
all_found_fac_path = "../private_results/matched_hospital_2024-12-01.csv"
if(!file.exists(all_found_fac_path)){
  
  #/////////////////////////
  ##### DSHS Hosp Data #####
  #/////////////////////////
  # Hospital data set from DSHS, but the CCN's are not unique, so those are useless for us
  dshs_hosp_df = readxl::read_xlsx("../input_data/general-special-hospital-list.xlsx")
  dshs_hosp_df_for_join = dshs_hosp_df %>%
    rename(PRVDR_NUM_list = CCN_NUMBER) %>%
    mutate(PROVIDER_NAME_clean = norm_name_with_dict(clean_text_column(NAME), hosp_name_dict),
           PROVIDER_NAME_no_space = gsub(" ", "", PROVIDER_NAME_clean)) %>%
    mutate(ST_ADR_clean     = norm_name_with_dict(clean_text_column(ADDRESS), street_name_dict),
           ST_ADR_rm_suffix = remove_street_address_suffix(ST_ADR_clean)) %>%
    dplyr::select(PROVIDER_NAME_clean, PROVIDER_NAME_no_space, 
                  starts_with("ST_ADR"),
                  ADDRESS:COUNTY, PRVDR_NUM_list)
  
  # each THCIC_ID mapping to one name is an assumption that needs to be true
  length(unique(dshs_hosp_df_for_join$PROVIDER_NAME_no_space)) # 648
  length(unique(dshs_hosp_df_for_join$ST_ADR_rm_suffix)) # 637, some duplicates but in diff city
  
  # Start building city to county crosswalk
  city_to_county_crosswalk = dshs_hosp_df_for_join %>%
    dplyr::select(ZIP, CITY, COUNTY, STATE) %>%
    distinct()
  
  #/////////////////////////
  #### MATCH HOSP NAMES ####
  #/////////////////////////
  # Simplest match of clean names in each data set
  match_facility_names_dshs <- pudf_hosp_df %>%
    dplyr::select(THCIC_ID, THCIC_ID_list, PROVIDER_NAME_no_space, PROVIDER_NAME_clean_list) %>%
    left_join(dshs_hosp_df_for_join, by="PROVIDER_NAME_no_space") %>%
    drop_na(PROVIDER_NAME_clean) %>% # 448 matches found
    mutate(INFO_ORIGIN = "DSHS")
  
  #//////////////////////
  #### HHS Hosp Data ####
  #//////////////////////
  hhs_hosp_df = read_csv("../big_input_data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility_20241007.csv")
  hhs_hosp_df_tx = hhs_hosp_df %>%
    filter(state=="TX") %>%
    dplyr::select(ccn, hospital_name, address:zip, state, fips_code, hospital_subtype) %>%
    distinct() %>% 
    rename_all(toupper) %>%
    rename(COUNTY_FIPS = FIPS_CODE, PRVDR_NUM_list = CCN) %>%
    mutate(PROVIDER_NAME_clean    = norm_name_with_dict(clean_text_column(HOSPITAL_NAME), hosp_name_dict),
           PROVIDER_NAME_no_space = gsub(" ", "", PROVIDER_NAME_clean),
           ST_ADR_clean     = norm_name_with_dict(clean_text_column(ADDRESS), street_name_dict),
           ST_ADR_rm_suffix = remove_street_address_suffix(ST_ADR_clean)) %>%
    dplyr::select(PROVIDER_NAME_clean, PROVIDER_NAME_no_space, 
                  starts_with("ST_ADR"),
                  ADDRESS:COUNTY_FIPS, PRVDR_NUM_list)
  
  # Add any new zip-city-county combinatins
  # hhs only has county FIPS and not the name, we'll get that below
  city_to_county_crosswalk = city_to_county_crosswalk %>%
    bind_rows(hhs_hosp_df_tx) %>%
    dplyr::select(ZIP, CITY, COUNTY, COUNTY_FIPS, STATE) %>%
    distinct()
  
  #/////////////////////////
  #### MATCH HOSP NAMES ####
  #/////////////////////////
  # Simplest match of clean names in each data set
  match_facility_names_hhs <- pudf_hosp_df %>%
    dplyr::select(THCIC_ID, THCIC_ID_list, PROVIDER_NAME_no_space, PROVIDER_NAME_clean_list) %>%
    filter(!(THCIC_ID %in% match_facility_names_dshs$THCIC_ID)) %>% # 479 not found
    left_join(hhs_hosp_df_tx, by="PROVIDER_NAME_no_space") %>%
    drop_na(PROVIDER_NAME_clean) %>%# 40 matches found
    mutate(INFO_ORIGIN = "HHS")
  
  #//////////////////////
  #### CMS Hosp Data ####
  #//////////////////////
  # Provider of service files are large because it's for all of US
  # Define the folder path and find all CSV files in the folder
  base_cms_path = "../big_input_data/POS_FILES/"
  pos_files = fs::dir_ls(base_cms_path, recurse = TRUE, glob = "*.csv")
  #pos_files ="../big_input_data/POS_FILES/POS_File_Hospital_Non_Hospital_Facilities_Q4_2023.csv"
  hosp_pos_df <- pos_files %>% # Read and process all files
    map(~ read_csv(.x) %>%
          dplyr::select(-starts_with(".")) %>% # remove empty ...number columns
          mutate(across(everything(), as.character)) %>% # ensures no type inconsistencies
          filter(STATE_CD == "TX")) %>%
    bind_rows() %>%
    distinct() # Ensure unique rows but may still have duplicates due to name changes
  
  ##### NOTE #####
  # Joining all these files creates a bit of a nightmare and some ambiguity
  #  Mainly: Nexus Children’s Hospital - Dallas, 9525 Greenville Ave, Dallas, TX 75243
  #  vs: Kindred Hospital Dallas Central, 8050 Meadow Rd, Dallas, TX 75231
  #  Kindred Hospital Dallas has a THCIC_ID and so does Kindred Hospital Dallas Central
  # There are also records of Kindred Hospital Dallas being at the address of Nexus Children’s Hospital
  # Very messy and confusing if you'd like any finer grain than City
  
  cms_hosp_df = hosp_pos_df %>% # 14101 rows
    mutate(
      FAC_NAME_clean = norm_name_with_dict(clean_text_column(FAC_NAME), hosp_name_dict)
    ) %>%
    mutate(ST_ADR_clean = norm_name_with_dict(clean_text_column(ST_ADR), street_name_dict),
           ST_ADR_rm_suffix = remove_street_address_suffix(ST_ADR_clean)
    ) %>%
    # Group by address to join any facilities with different names at same location
    group_by(STATE_CD, CITY_NAME, ST_ADR_rm_suffix) %>% # ZIP_CD removed bc I found an inconsistency in above example
    arrange(desc(CRTFCTN_DT), .by_group = T) %>% 
    mutate(
      FAC_NAME_clean_list = paste(unique(FAC_NAME_clean), collapse = "; "),
      PRVDR_NUM_list      = paste(unique(PRVDR_NUM), collapse = "; ")
    ) %>%
    slice(1) %>% # take only most recent certification date if all other location details the same
    ungroup() %>%
    rename(ZIP=ZIP_CD, CITY=CITY_NAME, STATE=STATE_CD) %>%
    mutate(COUNTY_FIPS = paste0(FIPS_STATE_CD, FIPS_CNTY_CD),
           COUNTY_FIPS = ifelse(COUNTY_FIPS=="NANA", NA, COUNTY_FIPS)) %>%
    dplyr::select(FAC_NAME_clean_list, PRVDR_NUM_list, STATE, CITY, ST_ADR, ST_ADR_clean, 
                  ST_ADR_rm_suffix, ZIP, COUNTY_FIPS, FAC_NAME_clean, everything())
  
  # renamed cms_hosp_df cols to match, which is needed below
  city_to_county_crosswalk = city_to_county_crosswalk %>%
    bind_rows(cms_hosp_df) %>%
    dplyr::select(ZIP, CITY, COUNTY, COUNTY_FIPS, STATE) %>%
    distinct()
  
  #/////////////////////////
  #### MATCH HOSP NAMES ####
  #/////////////////////////
  # Search for hospital names in list of POS hospital names
  match_facility_names_pos <- pudf_hosp_df %>%
    dplyr::select(THCIC_ID,  THCIC_ID_list, PROVIDER_NAME_clean, PROVIDER_NAME_clean_list) %>%
    filter(!(THCIC_ID %in% match_facility_names_dshs$THCIC_ID)) %>% # 479 names not found
    filter(!(THCIC_ID %in% match_facility_names_hhs$THCIC_ID)) %>% # 438 names not found
    separate_rows(PROVIDER_NAME_clean_list, sep = "; ") %>% # 1158 rows
    # Perform the join based on a match within the FAC_NAME_clean_list in CMS dataset
    rowwise() %>%
    mutate(
      match = list(
        cms_hosp_df %>%
          filter(
            str_detect(FAC_NAME_clean_list, paste0("\\b", PROVIDER_NAME_clean_list, "\\b"))
          )
      )
    ) %>%
    unnest(match, keep_empty = T) %>% # keep the THCIC_IDs that did not find a match
    ungroup() %>%
    group_by(THCIC_ID) %>%
    slice(1) %>%
    ungroup() %>% # back to 438 rows
    filter(!is.na(FAC_NAME_clean_list)) %>% # 173 found 
    mutate(INFO_ORIGIN = "POS")
  
  #/////////////////////////////
  #### ALL FOUND FACILITIES ####
  #/////////////////////////////
  # All 3 hospital name sources give some slight variation in provided details
  all_found_hosp = match_facility_names_pos %>%
    bind_rows(match_facility_names_dshs) %>%
    bind_rows(match_facility_names_hhs) # 661
  
  # used to match county fips to their names
  county_fips = tidycensus::get_acs(geography="county", state="TX", variables="B01001_001",
                                    year=2022, geometry=F ) %>%
    separate(NAME, into = c("county", "STATE"), sep=", ") %>%
    mutate(COUNTY = toupper(gsub(" County", "", county))) %>%
    rename(COUNTY_FIPS = GEOID, COUNTY_POP_2022=estimate) %>%
    dplyr::select(COUNTY, COUNTY_FIPS, COUNTY_POP_2022)
  
  # ZIP-CITY-COUNTY crosswalk
  hosp_city_to_county_crosswalk = city_to_county_crosswalk %>%
    left_join(county_fips, by = "COUNTY_FIPS", suffix = c("", "_from_fips")) %>%
    mutate(
      COUNTY = coalesce(COUNTY, COUNTY_from_fips), # Fill COUNTY from FIPS-based lookup
      COUNTY = ifelse(COUNTY=="DE WITT", "DEWITT", COUNTY),
      STATE = "TX"
    ) %>%
    select(-ends_with("_from_fips")) %>% # Drop intermediate column
    # Join by COUNTY to fill COUNTY_FIPS
    left_join(county_fips, by = "COUNTY", suffix = c("", "_from_name")) %>%
    mutate(
      COUNTY_FIPS = coalesce(COUNTY_FIPS, COUNTY_FIPS_from_name), # Fill COUNTY_FIPS from name-based lookup
    ) %>%
    select(-ends_with("_from_name")) %>%
    distinct() %>%
    drop_na() %>%
    arrange(ZIP, CITY, desc(COUNTY_POP_2022))
  write.csv(hosp_city_to_county_crosswalk,
            "../private_results/TX_zip-city-county_crosswalk.csv",
            row.names = F)
  
  # Clean-up to only useful columns and merge duplicates
  all_found_hosp_clean = all_found_hosp %>%
    dplyr::select(
      INFO_ORIGIN,
      starts_with("THCIC"), starts_with("PROVIDER_NAME_clean"),
      PRVDR_NUM_list,
      starts_with("STATE_CD"), starts_with("CITY"), 
      ST_ADR_clean,
      starts_with("ZIP"), starts_with("ZCTA"), 
      starts_with("FIPS"), starts_with("COUNTY")
      ) %>%
    rename(STREET_ADDRESS_clean = ST_ADR_clean, STATE=STATE_CD) %>%
    mutate(CITY = coalesce(CITY_NAME, CITY),
           ZIP = coalesce(ZIP_CD, ZIP),
           COUNTY_FIPS = ifelse(is.na(COUNTY_FIPS), 
                                paste0(FIPS_STATE_CD, FIPS_CNTY_CD),
                                COUNTY_FIPS),
           COUNTY_FIPS = ifelse(COUNTY_FIPS=="NANA", NA, COUNTY_FIPS),
           ) %>%
    dplyr::select(-ZIP_CD, -FIPS_STATE_CD, -FIPS_CNTY_CD, -CITY_NAME) %>%
    # Join by COUNTY_FIPS to fill COUNTY
    left_join(county_fips, by = "COUNTY_FIPS", suffix = c("", "_from_fips")) %>%
    mutate(
      COUNTY = coalesce(COUNTY, COUNTY_from_fips), # Fill COUNTY from FIPS-based lookup
      COUNTY = ifelse(COUNTY=="DE WITT", "DEWITT", COUNTY),
      STATE = "TX"
    ) %>%
    select(-ends_with("_from_fips")) %>% # Drop intermediate column
    # Join by COUNTY to fill COUNTY_FIPS
    left_join(county_fips, by = "COUNTY", suffix = c("", "_from_name")) %>%
    mutate(
      COUNTY_FIPS = coalesce(COUNTY_FIPS, COUNTY_FIPS_from_name), # Fill COUNTY_FIPS from name-based lookup
    ) %>%
    select(-ends_with("_from_name"))
  
  write.csv(all_found_hosp_clean, all_found_fac_path, row.names = F)

}else{
  all_found_hosp_clean = read_csv(all_found_fac_path)
  hosp_city_to_county_crosswalk = read_csv("../private_results/TX_zip-city-county_crosswalk.csv")
} # end if all_hosp csv file exists

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### MISSING FACILITIES ####
# Facilities not found in either data set
missing_fac_path = paste0("../private_results/missing_hospital_2024-12-01.csv")
if(!file.exists(missing_fac_path)){
  not_found_fac_all = pudf_hosp_df %>% # 933 rows
    filter(!(THCIC_ID %in% all_found_hosp$THCIC_ID)) # 262 missing
  write.csv(not_found_fac_all, 
            missing_fac_path,
            row.names = F )
}else{
  not_found_fac_all = read_csv(missing_fac_path)
} # end if missing hosps file not found

#////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#### NEEDED FACILITIES ####
# These files created by "run_estimation_fns.R"
disease = "FLU" # hyphen separated alphabetical string
date_range = "2022Q3-2023Q2" # hyphen separated start-end date string e.g. 2018Q3-2019Q2
zcta_hosp_pairs = read_csv(paste0("../private_results/ZCTA-HOSP-PAIR_", 
                                  disease, "_", date_range, ".csv"))
hosp_catchments = read_csv(paste0("../private_results/HOSP_CATCHMENTS/HOSP-POP-CATCH_", 
                                  disease, "_", date_range, ".csv"))
# Get unique mapping of a ZCTA to a county and keep the ZIP city name
zcta_to_county_crosswalk = 
  read_csv("../big_input_data/US_ZCTA-COUNTY_pop-weighted_geocorr2022.csv") %>%
  slice(-1) %>% # extra row of col descriptions
  drop_na(zcta) %>%
  rename(ZCTA = zcta, COUNTY_FIPS = county, 
         # e.g. what proportion of ZIP code is in the county
         ZCTA_COUNTY_ALLOCATION_FACTOR = afact) %>%
  mutate(CountyName = toupper(iconv(CountyName, from = "UTF-8", to = "ASCII//TRANSLIT")),
         ZIPName = toupper(iconv(ZIPName, from = "UTF-8", to = "ASCII//TRANSLIT")),
         COUNTY = gsub(pattern = " [A-Z]{2}$", "", CountyName)) %>%
  # Some NA but those ZCTA didn't have a city name
  separate(ZIPName, into=c("CITY", "STATE"), sep=", ", extra = "merge") %>%
  mutate(STATE = gsub(" \\(PO boxes\\)", "", STATE) ) %>%
  dplyr::select(ZCTA_COUNTY_ALLOCATION_FACTOR, ZCTA, COUNTY, COUNTY_FIPS, STATE) # CITY, 

# Make full cross-walk with city name from the ZIP to ZCTA pop-weighted crosswalk
# Using CITY name from the zcta_to_county_crosswalk alone is less acurate
zcta_city_county_crosswalk_path = "../big_input_data/US_ZCTA-CITY-COUNTY_pop_2018-2022_acs.csv"
if(!file.exists(zcta_city_county_crosswalk_path)){
  us_zcta_city_pop = get_zcta_acs_pop(state="US") %>%
    sf::st_drop_geometry() %>%
    rename(ZCTA_POP_2022=estimate) %>%
    left_join(zcta_to_county_crosswalk, 
              by=c("ZCTA")) %>%
    # Assign ZCTA to county where the majority of ZCTA is
    group_by(ZCTA) %>%
    arrange(desc(ZCTA_COUNTY_ALLOCATION_FACTOR), .by_group = T) %>%
    slice(1) %>%
    ungroup()
  write.csv(us_zcta_city_pop,
            "../big_input_data/US_ZCTA-CITY-COUNTY_pop_2018-2022_acs.csv",
            row.names=F
  )
}else{
  us_zcta_city_pop = read_csv(zcta_city_county_crosswalk_path)
} # end if crosswalk file wasn't made

# Get all THCIC_ID not found but needed
not_found_fac_need = not_found_fac_all %>%
  # separate list to ensure none of the alias THCIC_IDs are missed
  separate_rows(THCIC_ID_list, sep = "; ") %>% # 264 rows
  # filter to all missing THCIC_ID's in catchment file only
  filter(THCIC_ID_list %in% hosp_catchments$HOSPITAL)# 64 rows when FLU, 80 when COV

# Taking the city as the location of top ZCTA visiting
# On quick glance it looks pretty good
#  Ones that don't match seem negligible like 
#  Converse, TX => Warm Springs Rehab Hospital-San Antonio only has 1 FLU patient
top_city_visit = zcta_hosp_pairs %>%
  filter(THCIC_ID %in% not_found_fac_need$THCIC_ID_list) %>%
  mutate(across(PAT_ZCTA:THCIC_ID, as.character)) %>%
  left_join(us_zcta_city_pop, 
            by=c("PAT_ZCTA"="ZCTA")) %>%
  group_by(THCIC_ID, CITY_NAME, COUNTY, COUNTY_FIPS) %>%
  summarise(PAT_COUNT = sum(PAT_COUNT)) %>%
  ungroup() %>%
  group_by(THCIC_ID) %>%
  arrange(desc(PAT_COUNT), .by_group = T) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(pudf_hosp_df, by="THCIC_ID") %>%
  separate(CITY_NAME, into=c("CITY", "STATE"), sep=", ") %>%
  mutate(INFO_ORIGIN="CATCHMENT")

# These look pretty good when PAT_COUNT >100 
top_city_visit_check = top_city_visit %>%
  rowwise() %>%
  mutate(city_name_present = 
           ifelse(grepl(CITY, PROVIDER_NAME_clean, ignore.case =T), 1, 0)) %>%
  ungroup() %>%
  dplyr::select(city_name_present, CITY, PROVIDER_NAME_clean, PAT_COUNT) %>%
  arrange(city_name_present, desc(PAT_COUNT))

# Join all the matched hospitals with our unmatched catchment set
all_thcic_id_needed = all_found_hosp_clean %>%
  mutate(across(everything(), as.character)) %>%
  bind_rows(top_city_visit) %>%
  dplyr::select(-PROVIDER_NAME, -PROVIDER_NAME_no_space, -FACILITY_TYPE) %>%
  mutate(CITY = toupper(CITY))

# Add city names to the catchment file
hosp_catchments_with_location = hosp_catchments %>%
  # Perform the join based on a match within the FAC_NAME_clean_list in CMS dataset
  rowwise() %>%
  mutate(
    match = list(
      all_thcic_id_needed %>%
        filter(
          str_detect(string=THCIC_ID_list, pattern=paste0("\\b", HOSPITAL, "\\b"))
        )
    )
  ) %>%
  unnest(match, keep_empty = T) %>% # keep the THCIC_IDs that did not find a match
  ungroup() %>%
# Do not remove any duplicate hospital names from catchment
  dplyr::select(INFO_ORIGIN, DATE_RANGE, DISEASE_INCLUDED, 
                HOSPITAL, HOSP_CATCHMENT,
                CITY, STATE, everything(), 
                -THCIC_ID) # same as HOSPITAL in this file
write.csv(
  hosp_catchments_with_location,
  paste0("../private_results/HOSP_CATCHMENTS/CITY-HOSP-POP-CATCH_", 
         disease, "_", date_range, ".csv"),
  row.names = F
  )



