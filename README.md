# Hospital Catchment Estimation
We estimate a hospital's population catchment as the proportion of the population in each surrounding ZIP Code likely to visit that hospital. The "likely" proportions are taken from the [Texas DSHS Inpatient Public Use Data File](https://www.dshs.texas.gov/center-health-statistics/texas-health-care-information-collection/health-data-researcher-information/texas-hospital-emergency-department-research-data-file-ed-rdf/texas-inpatient-public-use-data-file-pudf) (IP PUDF) according to the equation below (1). The IP PUDF is quarterly so time spans can range from 2018Q3 to 2023Q4 for any disease defined by a set of ICD-10-CM codes. The lowest spatial resolution for patient addresses is their ZIP Code. We convert all ZIP Codes to ZIP Code Tabulation Areas (ZCTAs) to assign PO Boxes to polygons with a US Census Bureau population estimate. If a hospital discharged fewer than 30 patients in a quarter, then patient ZIP Codes were anonymized and are not considered.

$$C_j = \sum_{i=1}^n \left(\frac{x_{ij}}{T_i} \cdot P_i\right)$$ (1)

$C_j$ is the catchment population for hospital $j$, where $m$ is the total number of hospitals in Texas receiving patients for a specific disease $D$ and $n$ is the total number of ZCTAs that sent patients to any of the $m$ hospitals. <br/>
&nbsp;&nbsp;&nbsp;&nbsp;For example, COVID-19 has the ICD-10-CM code U071, so we consider all patients with U071 as their principal diagnosis and any who had U071 present on admittance (secondary diagnosis). <br/>
$x_{ij}$ is the count of patients with a mailing address in ZCTA $i$ discharged from hospital $j$ for disease $D$ over a specified span of time $Q$. <br/>
$T_i = \sum_{j=1}^m x_{ij}$ is the total patients originating in ZCTA $i$ that went to any hospital. <br/>
$P_i$ is the ZCTA population estimate from the 5-year American Community Survey 2018-2022 obtained from [tidycensus](https://walker-data.com/tidycensus/). <br/>


### Example Data Table for Calculating $C_j$

| **ZCTA $i$** | **Hospital $j$** | **$x_{ij}$** | **$T_i = \sum_{j=1}^m x_{ij}$** | **$P_i$** | **Contribution to $C_j$** |
|---------------------|------------------------|------------------|--------------------------------------|--------------|----------------------------------------|
| 1                   | 1                      | 100              | 100                                  | 1000         | $\frac{100}{100} \cdot 1000 = 1000$    |
| 2                   | 1                      | 50               | 100                                  | 500          | $\frac{50}{100} \cdot 500 = 250$       |
| 2                   | 2                      | 50               | 100                                  | 500          | $\frac{50}{100} \cdot 500 = 250$       |

### Calculated $C_j$ Values:
- $C_1 = 1000 + 250 = 1250$
- $C_2 = 250$

### Running Code and Results
The Rproj is inside the `code` folder, so open that to interact with R studio. `run_estimation_fns.R` will create `private_results/ZCTA-HOSP-PAIR_DISEASE_DATE-RANGE.csv`, `private_results/HOSP_CATCHMENTS/HOSP-CATCH-CALC_DISEASE_DATE-RANGE.csv` and `private_results/HOSP_CATCHMENTS/HOSP-POP-CATCH_DISEASE_DATE-RANGE.csv`. In `HOSP-CATCH-CALC_DISEASE_DATE-RANGE.csv` you'll find the calculations used to estimate the catchment, so this includes ZCTA-Hospital pairs plus ZCTA population. `HOSP-POP-CATCH_DISEASE_DATE-RANGE.csv` has only hospital THCIC_ID's and their estimated catchments. 

The THCIC_ID's only come with the hospital/provider names in the facility file associated with each year-quarter. `create_thcic-id_to_ccn_crosswalk.R` uses 3 sources of hospital names to determine which city a hospital is in based on its name alone. All the hospitals that found a match are in `private_results/matched_hospital_2024-12-01.csv` and those still missing are in `private_results/missing_hospital_2024-12-01.csv`. The subset of missing hospitals required for the catchment file is chosen as the city of the top visiting ZIP code to each hospital. This works pretty well, but is off for a few contributions with only 1-2 patients admitted. The final file you'd use is `private_results/HOSP_CATCHMENTS/CITY-HOSP-POP-CATCH_DISEASE_DATE-RANGE.csv`. 


























