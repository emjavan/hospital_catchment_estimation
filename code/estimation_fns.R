
#////////////////////////////////////////////////////////////
#' Get the run time of a function
#' 
#' @param func any function 
#' @param ... the parameter inputs to func
#' @returns result of function timed, will print the total run time
#' @examples
#' combo2 = measure_run_time(generate_weight_combinations, 2)
measure_run_time <- function(func, ...) {
  start_time <- Sys.time()  # Capture the start time
  result <- func(...) # Execute the function with provided arguments
  end_time <- Sys.time() # Capture the end time
  elapsed_time_sec <- difftime(end_time, start_time, units = "secs") # Calculate the elapsed time
  #elapsed_time_auto <- difftime(end_time, start_time)
  
  cat("Elapsed Time:", as.numeric(elapsed_time_sec), " sec\n") # Print the elapsed time
  return(result) # Return the result of the function
} # end measure_run_time

#/////////////////////////////////////////////////////////////////////////////////////////////////////
#' Get TX ZCTA population and geometry
get_zcta_acs_pop = function(
  census_api_key = NULL,
  data_variables = "B01001A_001", # could change to vector of population by age
  data_set_name = "population", # name whatever makes sense for data being pulled
  data_year = 2022, # default is ACS 2-year so 2022 is 2018-2022 average
  download_geometry = TRUE, 
  state="TX" # FIPS code 48
  ){
  # Check if API key
  if(is.null(Sys.getenv("CENSUS_API_KEY")) | !is.null(census_api_key)){
    census_api_key(census_api_key, install=TRUE, overwrite=TRUE)
  }

  output_file_path_prefix = paste("../input_data/acs_zcta", state, data_set_name, data_year, sep="_")
  output_file_path = paste0(output_file_path_prefix, ".rda")
  if(!file.exists(output_file_path)){
    acs_data = get_acs(geography = "zcta", variables = data_variables, 
                       year = data_year, geometry = download_geometry, 
                       cb = FALSE) # cb is a input of tigris::zctas
    
    zcta_to_city = read_csv(paste0("../input_data/", state, "_ZCTA-CDP_pop-weighted_geocorr2022.csv")) %>%
      slice(-1) %>% # extra row of col descriptions
      drop_na(zcta)
    
    # Leaving PO Boxes because they have ZCTA population data in tidycensus
    # Ultimate goal is to assign hospitals to cities so that is sufficient
    acs_data_filtered = acs_data %>%
      filter(GEOID %in% zcta_to_city$zcta) %>%
      dplyr::select(-NAME, -variable) %>%
      left_join(zcta_to_city, by=c("GEOID"="zcta")) %>%
      mutate(ACS_5YEAR=data_year) %>%
      dplyr::select(ZIPName, GEOID, estimate, moe) %>% # 76573 converts to 76530 for example
      distinct() %>%
      rename(ZCTA=GEOID)
    
    save(acs_data_filtered, file=output_file_path)
    
  }else{
    # load data if it file already created
    load(output_file_path)
  }
  
  return(acs_data_filtered)
} # end get_zcta_acs_pop

#/////////////////////////////////////////////////////////////////////////////////////////////////////
#' Extract data date from file path
# get_data_date <- function(file_path) {
#   if (grepl("IP_PUDF_BASE_DATA_1_*_tab\\.txt$", file_path)) {
#     # Extract the date or date+quarter from the out.IP file name
#     data_date = gsub(".*IP_PUDF_BASE_DATA_1_", "", file_path)
#     data_date = gsub("_tab\\.txt$", "", data_date)
#     
#     # Need to create some synthetic PUDF data
#   } else if (grepl("synthetic_data/IP_PUDF_synthetic_data\\.txt$", file_path)) {
#     # For synthetic data files, set data_date to NA
#     data_date <- ""
#   } else {
#     stop("Unknown file type")
#   } # end if else
#   
#   return(data_date)
# } # end get_data_date

#//////////////////////////////////////////////////////////////////////////////////////////////
#' Get dates in a given range, dates of the type `YEAR`Q`QUARTER`
get_dates_in_range = function(
    date_range = c("2018Q3", "2023Q2")
    ){
  start_date = unlist(str_split(date_range[1], pattern="Q"))
  end_date = unlist(str_split(date_range[2], pattern="Q"))
  YEAR = seq(start_date[1], end_date[1], by=1)
  QUARTER = c(1:4)
  
  year_quarter = 
    expand_grid(YEAR, QUARTER) %>%
    filter((YEAR > start_date[1] | (YEAR == start_date[1] & QUARTER >= start_date[2])) &
             (YEAR < end_date[1] | (YEAR == end_date[1] & QUARTER <= end_date[2]))) %>%
    mutate(FILE_DATE = paste(YEAR, QUARTER, sep="Q"))
  
  return(year_quarter)
} # end get_dates_in_range



#' Function to check ICD-10 code match based on matching the first 3 characters or the full string.
#' 
#' This function checks if the diagnosis code from patient data matches an ICD-10 code from the ICD-10 category list.
#' Depending on the value of `code_start_with`, the function will either compare the first 3 characters or the entire string.
#' 
#' @param diag_code A character string. The diagnosis code from the patient data (e.g., PRINC_DIAG_CODE).
#' @param icd10_code A character string. The ICD-10 code from the ICD-10 category list.
#' @param code_start_with A character string ("T" or "F"). Indicates whether to match by the first 3 characters ("T") or the full string ("F").
#' @return A logical value. Returns `TRUE` if the codes match according to the `code_start_with` rule, `FALSE` otherwise.
#' 
#' @examples
#' # Example where codes match the first 3 characters
#' match_icd10("J129", "J12", "T") # TRUE
#' 
#' # Example where full string match is required
#' match_icd10("J129", "J129", "F") # TRUE
#' 
#' # Example where no match occurs
#' match_icd10("J129", "J128", "F") # FALSE
match_icd10 <- function(diag_code, icd10_code, code_start_with) {
  # Check if the code should match on the first 3 characters
  if (code_start_with == "T") {
    # Return TRUE if the first 3 characters of both codes match
    return(substr(diag_code, 1, 3) == substr(icd10_code, 1, 3))
  } else {
    # Return TRUE if the full strings match
    return(diag_code == icd10_code)
  }
  # Explicitly return FALSE if neither condition is met (not really needed due to logic but makes the intent clearer)
  return(FALSE)
}

#' Function to assign disease category based on diagnosis codes and POA status
#'
#' This function checks whether the given diagnosis code (either primary or secondary) matches an entry
#' in the ICD-10 disease category list. It then returns the corresponding disease category if the POA
#' code is "Y", indicating that the diagnosis was present on admission.
#'
#' @param diag_code A character string. The diagnosis code from the patient data (e.g., PRINC_DIAG_CODE).
#' @param poa_code A character string. The POA (Present on Admission) code, typically "Y" or "N".
#' @param icd10_df A data frame. The ICD-10 disease category list with columns ICD10_CODE, CODE_START_WITH, and DISEASE_CAT.
#'
#' @return A character string. Returns the disease category if a match is found and POA code is "Y". Otherwise, returns NA.
#' 
#' @example assign_disease_category(diag_code="J1011", poa_code="Y", icd10_df)
assign_disease_category <- function(diag_code, poa_code, icd10_df) {
  
  if (is.na(diag_code)) {
    return(NA_character_)  # Return NA for missing POA codes or when POA is not "Y"
  }
  
  if ((!is.na(poa_code)) & (poa_code == "Y")) {
    # Add columns for icd-10 substring and matching logic, then filter for only matching string
    match_row <- icd10_df %>%
      mutate(
        diag_substr = substr(diag_code,  1, 3) # Extract first 3 characters from diag_code
      ) %>%
      rowwise() %>%
      mutate(
        # if the codes only start with substring then match there, otherwise match entire ICD-10
        match_condition = ifelse(CODE_STARTS_WITH == T, 
                                 diag_substr == ICD10_3CHAR_SUBSTRING, 
                                 diag_code == ICD10_CODE)  # Match logic
      ) %>%
      ungroup() %>%
      filter(match_condition)
    
    # Return the disease category if a match is found
    if (nrow(match_row) > 0) {
      # print the matched row if debugging
      #ic(c(match_row$ICD10_CODE[1], match_row$DISEASE_CAT[1]))
      return(match_row$DISEASE_CAT[1])  # Return the first matching disease category
    } else {
      return(NA_character_)  # Return NA if no match is found
    }
  }
  return(NA_character_)  # Return NA if POA code is not "Y"
} # end assign_disease_category


#' Assign disease categories and create summary variables
#'
#' This function processes a dataset by assigning disease categories based on ICD-10 codes
#' and creating binary variables indicating the presence of specific diseases (COV, FLU, RSV, ILI) in secondary diagnoses.
#' It also processes length of stay, ZIP codes, ward/ICU status, and age groups.
#'
#' @param df data.frame: The input patient dataset.
#' @param icd10_df data.frame: The ICD-10 code reference dataset.
#'
#' @return data.frame: A modified patient dataset with assigned disease categories, binary flags for specific diseases,
#' processed length of stay, ZIP code, and age group.
#' 
#' @examples
#' processed_data <- process_patient_data(patient_data, icd10_df)
process_patient_data <- function(df, icd10_df) {
  
  all_disease_cats = unique(icd10_df$DISEASE_CAT)
  
  cleaned_pat_df =
    df %>%
    rowwise() %>% # Ensure row-wise operations for the entire pipeline
    mutate(
      PRIMARY_ADMIT_POA_Y = assign_disease_category(PRINC_DIAG_CODE, POA_PRINC_DIAG_CODE, icd10_df)
    ) %>%
    bind_cols(
      map_dfc(1:24, function(i) {
        transmute(
          .,
          !!str_c("SECONDARY_ADMIT_POA_Y_", i) := assign_disease_category(
            .data[[paste0("OTH_DIAG_CODE_", i)]],
            .data[[paste0("POA_OTH_DIAG_CODE_", i)]],
            icd10_df
          )
        )
      })
    ) %>%
    ungroup()

  cleaned_pat_df_int <- cleaned_pat_df %>%
    # Create PRIMARY_* columns dynamically
    bind_cols(
      map_dfc(.x = all_disease_cats, 
              .y = ., 
              .f = ~ .y %>% 
                rowwise() %>% 
                transmute(!!str_c("PRIMARY_", .x) := as.integer(replace_na(PRIMARY_ADMIT_POA_Y == .x, FALSE)))
              )) %>%
    bind_cols(
      map_dfc(all_disease_cats, function(disease) {
        transmute(.,
          !!str_c("SECONDARY_", disease) := as.integer(
            rowSums(
              across(starts_with("SECONDARY_ADMIT_POA_Y"), ~ replace_na(. == disease, FALSE)) ) > 0
          ))}
        ))
  return(cleaned_pat_df_int) 
} # end process_patient_data

#//////////////////////////////////////////////////////////////////////////////////////////////
#' Get vector of all file paths in date range
get_og_pudf_files_in_date_range = function(
    date_range = c("2023Q4", "2023Q4"), # NULL for synthetic data set){
    path = file_path
    ){
  
  # Choose between a passed range of PUDF data or a synthetic dataset
  if(!is.null(date_range)){
    all_dates = get_dates_in_range(date_range)
    
    # List files from matching folders
    files_in_range <- map(all_dates$FILE_DATE, ~ list.files(
      path = file.path("../private_input_data/OG_PUDF_DATA", paste0("PUDF_", .x, "_tab-delimited")),
      pattern = "^IP_PUDF_BASE_DATA_1_",
      full.names = TRUE
    )) %>%
      flatten_chr()
    
    # Abort if files missing 
    if(!(length(all_dates$FILE_DATE) == length(files_in_range))){
      ic(files_in_range)
      ic(length(all_dates$FILE_DATE))
      ic(length(files_in_range))
      
      abort(message = "not all files found from date range \n")
    } # end if needed file(s) not found
  }else{
    # files_in_range = path to synthetic data
    
  } # end if using PUDF or synthetic data
  
  return(files_in_range)
}  # end get_og_pudf_files_in_date_range

#//////////////////////////////////////////////////////////////////////////////////////////////
#' Assign all ICD-10 for each patient a disease category from the icd10_df
categorize_patients_by_disease = function(date_range, icd10_df){
  
  # Get vector of all diseases and create their output file directories 
  all_disease_cats = unique(icd10_df$DISEASE_CAT)
  dir_name_prexif = "../private_input_data/PUDF_"
  dir_names = paste0(dir_name_prexif, all_disease_cats, "/")
  walk(dir_names, ~ {
    if (!dir.exists(.x)) {
      dir.create(.x, recursive = TRUE)
      message("Created directory: ", .x)
    } else {
      message("Directory already exists: ", .x)
    }
  })
  
  # Make output file paths
  all_dates = get_dates_in_range(date_range)
  ic(all_dates$FILE_DATE)
  
  # Get the paths of all original PUDF files needed for categorizing
  files_in_og_pudf_range = get_og_pudf_files_in_date_range(date_range)
  
  # Make all expected output file paths
  output_file_path_df <- 
    expand_grid(
      disease = all_disease_cats,
      input_file_path = files_in_og_pudf_range,
      ) %>%
    mutate(file_date = gsub(".*PUDF_(\\d{4}Q\\d).*", "\\1", input_file_path) ) %>%
    separate(file_date, into=c("year", "quarter"), remove=F, sep="Q") %>%
    mutate(across(year:quarter, as.numeric)) %>%
    left_join(icd10_df %>%
                select(DISEASE_CAT, START_YEAR, START_QUARTER) %>%
                distinct(),
              by=c("disease"="DISEASE_CAT")) %>%
    # Filter any user provided date from icd10_df "START_YEAR" and "START_QUARTER", user must supply both
    filter(
      is.na(START_YEAR) |  # Skip filtering if START_YEAR is NA
      !((year < START_YEAR | (year == START_YEAR & quarter < START_QUARTER) )) 
      ) %>%
    mutate(
        output_file_path    = paste0(dir_name_prexif, disease, "/", disease, "_ALL_PAT_CAT_", file_date, ".csv"),
        output_file_exists  = FALSE, # does the file already exist
        output_file_created = FALSE # was the file successfully created, i.e. some people had ICD-10 of interest
      ) # end mutate
  
  # Loop over files, but internal loop will generate all disease files per date
  for(file_index in 1:nrow(output_file_path_df)){
     if(!file.exists(output_file_path_df$output_file_path[file_index])){
       message("Input file is ", output_file_path_df$input_file_path[file_index], "\n")
       
       # Get date of current file
       single_file_date = output_file_path_df$file_date[file_index]
       ic(single_file_date)
       
       # Read in data
       pudf_data = read_delim(output_file_path_df$input_file_path[file_index]) %>%
         select(-starts_with(".")) # was adding an empty ...number column
       
       # Categorize for disease ICD-10 codes
       # This takes a very long time on local machine => highly recommend TACC
       categorized_pudf = process_patient_data(pudf_data, icd10_df) %>%
         mutate(file_creation_date = Sys.Date())
       
       # Filter to only the rows for a single date, i.e. each disease per date
       output_file_path_df_single_date = output_file_path_df %>%
         filter(file_date==single_file_date)
       
       for(disease_index in 1:nrow(output_file_path_df_single_date)){
         # Get all the columns counting occurrence of disease in record
         # Almost all should be 1 time just primary or secondary infection
         categorized_pudf_cols = 
           paste0(c("PRIMARY_", "SECONDARY_"), # e.g. PRIMARY_COV and SECONDARY_COV cols
                  output_file_path_df_single_date$disease[disease_index]) 

         # Filter to only where primary and secondary disease exist
         categorized_pudf_per_disease = categorized_pudf %>%
           filter(if_any(all_of(categorized_pudf_cols), ~ . > 0))
         
         # Disease rows should exist if it's not some really rare disease or ICD-10 typo
        if(nrow(categorized_pudf_per_disease) > 0){
          output_file_path_single = output_file_path_df_single_date$output_file_path[disease_index]
          
          # Write disease specific categorized file with a date of creation
          message("File will be written to ",output_file_path_single, "\n")
          write.csv(categorized_pudf_per_disease, output_file_path_single, row.names=F)
          
          # Keep track of if the expected file was created
          # Find the index of the matching file path in output_file_path_df
          matching_index <- which(output_file_path_df$output_file_path == output_file_path_single)
          
          # Update the output_file_created column to TRUE for the matching row
          output_file_path_df$output_file_created[matching_index] = TRUE
         }else{
           warn(paste0(output_file_path_df$disease[file_index], 
                       " columns were not found in IP PUDF ", 
                       output_file_path_df$file_date[file_index]))
         } # end if disease cols were not found
       } # end for loop over date range
     }else{
       # Keep track of if the expected file already exists
       output_file_path_df$output_file_exists[file_index] = TRUE
       message("File exists ", output_file_path_df$output_file_path[file_index], "\n")
     } # end if the file already exists
  } # end loop over original PUDF files
  
  # return the file paths for the found files 
  return(output_file_path_df)
} # end categorize_patients_by_disease

#//////////////////////////////////////////////////////////////////////////////////////////////
#' Clean IP PUDF files to ZCTA-Hosp pairs by ICD-10 code set
count_patients_zcta_hosp_pairs = function(
    date_range = c("2018Q4", "2020Q4"), # NULL for synthetic data set
    icd10_df
  ){

  # remove anonymized PAT_ZIP before counting
  filter(!(PAT_ZIP=="88888")) %>% 
  
 
  
  #check if files exists before making with categorize_patients_by_disease()
  output_file_path_df = categorize_patients_by_disease(date_range, icd10_df)
  
  
  # other wise open files 
  # => need those new disease files locations
  

  
  
  
  
      output_folder_path
  
  
} # end count_patients_zcta_hosp_pairs













#//////////////////////////////////////////////////////////////////////////////////////////////
#' Calculate Hospital Catchments
#'
#' @description This function calculates the population catchment of hospitals 
#' based on the proportion of patients from each spatial geometry who visit each hospital. 
#' It allows users to specify custom column names for `GEOMETRY`, `HOSPITAL`, and `PAT_COUNT` 
#' (the number of patients). Optionally, columns for `YEAR`, `QUARTER`, and 
#' `DISEASE` can also be specified for grouping.
#'
#' @param geom_hosp_df A dataframe containing the data for calculation.
#' @param geom_col A string specifying the column name for geometry in `geom_hosp_df`.
#' @param hosp_col A string specifying the column name for hospital in `geom_hosp_df`.
#' @param pat_count_col A string specifying the column name for `PAT_COUNT` in `geom_hosp_df`.
#' @param year_col Optional. A string specifying the column name for `YEAR` in `geom_hosp_df`.
#' @param quarter_col Optional. A string specifying the column name for `QUARTER` in `geom_hosp_df`.
#' @param disease_col Optional. A string specifying the column name for `DISEASE` in `geom_hosp_df`.
#' @param population_df A dataframe with geom populations, containing columns `GEOMETRY` and `POPULATION`.
#' @param example_output_path A string specifying the file path and name to write the example dataset.
#' @param calcultion_outputfile_path A string specifying where to write files used to calculate catchment
#' @param catchment_outputfile_path A string specifying where to write files with catchment per hospital
#'
#' @return A dataframe with hospital catchments (`C_j`) calculated for each hospital, 
#' optionally grouped by `YEAR`, `QUARTER`, and `DISEASE`.
#'
#' @examples
#' # Example dataframe
#' example_data <- data.frame(
#'   ZCTA = c(1, 2, 2),
#'   HOSPITAL = c("H1", "H1", "H2"),
#'   PAT_COUNT = c(100, 50, 50),
#'   YEAR = c(2023, 2023, 2023),
#'   QUARTER = c(1, 1, 1),
#'   DISEASE = c("COVID-19", "COVID-19", "COVID-19")
#' )
#' population_data <- data.frame(
#'   ZCTA5 = c(1, 2),
#'   POPULATION = c(1000, 500)
#' )
#' 
#' catchments <- calculate_hospital_catchments(
#'   geom_hosp_df = example_data,
#'   geom_col = "ZCTA",
#'   hosp_col = "HOSPITAL",
#'   pat_count_col = "PAT_COUNT",
#'   year_col = "YEAR",
#'   quarter_col = "QUARTER",
#'   disease_col = "DISEASE",
#'   population_df = population_data,
#'   calcultion_outputfile_path = "example_data.csv",
#'   
#' )
calculate_hospital_catchments <- function(
    geom_hosp_df, 
    geom_col, 
    hosp_col, 
    pat_count_col, 
    year_col = NULL, 
    quarter_col = NULL, 
    disease_col = NULL, 
    population_df, 
    calcultion_outputfile_path = NULL,
    catchment_outputfile_path = NULL
    ) {
  # Ensure columns exist
  required_cols <- c(geom_col, hosp_col, pat_count_col)
  optional_cols <- c(year_col, quarter_col, disease_col)
  missing_cols <- setdiff(required_cols, names(geom_hosp_df))
  
  if (length(missing_cols) > 0) {
    stop(paste("The following required columns are missing in `geom_hosp_df`: ", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # Join with population data
  geom_hosp_needed_cols_df <- geom_hosp_df %>%
    rename_with(~ c("ZCTA", "HOSPITAL", "PAT_COUNT"), 
                .cols = c(!!geom_col, !!hosp_col, !!pat_count_col)) %>%
    # Join population by GEOMETRY
    left_join(population_df %>% rename(POPULATION = "POPULATION", GEOMETRY = !!geom_col), by = "GEOMETRY") %>%
    # Calculate total patients per each GEOMETRY
    group_by(GEOMETRY) %>%
    mutate(TOTAL_PAT_PER_GEOM = sum(PAT_COUNT)) %>%
    ungroup() %>%
    # Calculate catchment population contributions
    mutate(PAT_COUNT_PROPORTION = (PAT_COUNT / TOTAL_PAT_PER_GEOM_COUNT),
           GEO_CONTRIBUTION = PAT_COUNT_PROPORTION * POPULATION) 
  
  # write data to calculate catchment to file if path specified
  if(!is.null(calcultion_outputfile_path)){
    write.csv(geom_hosp_needed_cols_df, calcultion_outputfile_path, row.names = FALSE)
  }
  
  # Summarize catchment population by hospital
  catchments <- geom_hosp_needed_cols_df %>%
    group_by(HOSPITAL, across(all_of(optional_cols))) %>%
    summarize(HOSP_CATCHMENT = sum(GEO_CONTRIBUTION, na.rm = TRUE), .groups = "drop")
  
  # write hospital population catchment to file if path specified
  if(!is.null(catchment_outputfile_path)){
    write.csv(catchments, catchment_outputfile_path, row.names = FALSE)
  }
  
  return(catchments)
}










