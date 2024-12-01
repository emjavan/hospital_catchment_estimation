# Source of Hospital names across Texas
DSHS PUDF only gives the hospital names (no address. lat/long, etc) and their names are some times acronymns. 
We use the three "Hosp Data" files below to find a best candidate name for hospitals and use the name dictonaries
to normalize to find best match, e.g. UNIVERSITY OF TEXAS to UT, etc.

## 1. DSHS Hosp Data
File downloaded from [here](https://www.hhs.texas.gov/sites/default/files/documents/general-special-hospital-list.xlsx) </br>

First row of general-special-hospital-list.xlsx was : </br>
11/13/2024 </br>
FOR A CURRENT LICENSE STATUS INFORMATION PLEASE VISIT OUR PUBLIC LICENSE SEARCH WEBSITE AT HTTPS://VO.RAS.DSHS.STATE.TX.US/

Updated KINDRED HOSPITAL-TARRANT COUNTY > KINDRED HOSPITAL-TARRANT COUNTY FORT WORTH since there is a duplicate/non-unique name mapping


## 2. HHS Hosp Data
This file is too large for github so it's in the folder "big_input_data" and ignored. </br>
You can download [here](https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/anag-cw7u/about_data)

## 3. CMS Hosp Data
This file is too large for github so it's in the folder "big_input_data" and ignored. </br>
You can download [here](https://data.cms.gov/provider-characteristics/hospitals-and-other-facilities/provider-of-services-file-hospital-non-hospital-facilities)

## Other Files here
1. icd10_disease_category_list.csv
The ICD-10-CM codes used to categorize diseases, e.g. U071 is COVID and use the 3-letter naming scheme "COV"

2. StreetName_Replacements.csv
Replacements to normalize street names, e.g. STREET to ST

3. HospitalName_Replacements.csv
Replacements to normalize hospital names, e.g. UNIVERSITY OF TEXAS to UT, etc.
