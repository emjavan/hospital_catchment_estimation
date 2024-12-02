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



# big_input_data Files

`zip_zcta_xref.csv` comes from [here](https://github.com/censusreporter/acs-aggregate/blob/master/crosswalks/zip_to_zcta/ZIP_ZCTA_README.md)
which is 4 years old but still a good reference.

`ZIPCode-to-ZCTA-Crosswalk.xlsx` is from [HRSA.gov](https://data.hrsa.gov/DataDownload/GeoCareNavigator/ZIP%20Code%20to%20ZCTA%20Crosswalk.xlsx)

Another place to get ZIP code names is from the [US postal sevice](https://www.unitedstateszipcodes.org/tx/), 
but this isn't a unique mapping of ZIP code to a city and provides no ZCTA mapping. However, when I need to look-up 
a missing ZIP code this is a good reference to confirm. For example,

```
mutate(PAT_ZCTA = ifelse(PAT_ZCTA=="75390", "75235", PAT_ZCTA), # Dallas ZIP w/o population, really tiny
       PAT_ZCTA = ifelse(PAT_ZCTA=="78802", "78801", PAT_ZCTA) # Uvalde PO box not in crosswalk
      )
```
is hard coded into `count_patients_zcta_hosp_pairs` function because when I checked the output of `HOSP-CATCH-CALC_*`
the number of patients leaving this ZIP code for flu/ILI treatment was larger than the population estimate or wasn't 
in the ACS ZCTA download at all. 






