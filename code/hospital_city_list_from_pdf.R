#///////////////////////////////////////////////////////////////////////
# Extract THCIC_ID with hospital names and city from PUDF online file
# File from PUDF page for the specific year
#///////////////////////////////////////////////////////////////////////

# To Do
# Loop/pmap/lapply over all the years of files to get list of all
#  THCIC_IDs reporting together

# Load packages and disable ic if you don't want debugging/verbose
source("get_packages_used.R")
source("estimation_fns.R")
ic_disable()

# Input data
input_file_path = "../input_data/PUDF_InpatientStatusReport4Q2023.pdf"

# File from TX Comptroller
# https://comptroller.texas.gov/taxes/misc-gross-receipts/docs/population-of-incorporated-cities-or-towns-in-texas-for-mgrt.csv
texas_cities <- read_csv("../input_data/population-of-incorporated-cities-or-towns-in-texas-for-mgrt.csv") %>% 
  separate(`Incorporated City or Town`, into=c("CITY", "CITY_TYPE"), sep="\\s(?=[^\\s]+$)" ) %>%
  mutate(CITY = str_trim(toupper(CITY)))

# Extract the quarter number and year separately if needed
quarter_number <- str_match(input_file_path, "(\\d)Q")[, 2]
year <- str_match(input_file_path, "Q(\\d{4})")[, 2]

# Extract hospital text from PDF
hosp_pdf_text <- pdf_text(input_file_path) %>%
  str_c(collapse = "\n") # get rid of page association and make one big string

# Create clean mapping of THCIC_ID to CITY
hosp_data = parse_page(hosp_pdf_text) %>%
  # Combine names that went on to another line
  mutate(ID_Hospital = ifelse(CITY=="ABOVE", paste(lag(ID_Hospital), ID_Hospital), ID_Hospital),
         ID_Hospital = ifelse(lead(CITY)=="ABOVE", lead(ID_Hospital), ID_Hospital) # 
         ) %>%
  filter(!(CITY=="ABOVE")) %>%
  # These aren't correctly assigned, so throw them out, only needed for spacing 
  dplyr::select(-(Q1:Comment_4)) %>%
  mutate(Reports_With = ifelse(str_detect(Reports_With, "^\\d{6}\\b"), Reports_With, NA)) %>%
  separate(ID_Hospital, into=c("THCIC_ID", "HOSP_NAME"), 
           sep = "\\s", extra = "merge") %>%
  rename(HOSP_CITY = CITY,
         HOSP_REPORTS_WITH_THCIC_ID = Reports_With)

# Write cleaned PDF into an easy to use CSV
output_file_path = paste0("../input_data/PUDF_hospital-name-to-city_",  year, "Q", quarter_number, ".csv")
write.csv(hosp_data, output_file_path, row.names=F)
  
  
  
  
  

