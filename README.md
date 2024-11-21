# Hospital Catchment Estimation
We estimate a hospital's population catchment as the proportion of the population in each surrounding ZIP Code likely to visit that hospital. The "likely" proportions are taken from the [Texas DSHS Inpatient Public Use Data File](https://www.dshs.texas.gov/center-health-statistics/texas-health-care-information-collection/health-data-researcher-information/texas-hospital-emergency-department-research-data-file-ed-rdf/texas-inpatient-public-use-data-file-pudf) (IP PUDF) according to the equation below (1). The IP PUDF is quarterly so time spans can range from 2018Q3 to 2023Q4 for any disease defined by a set of ICD-10-CM codes. The lowest spatial resolution for patient addresses is their ZIP Code. We convert all ZIP Codes to ZIP Code Tabulation Areas (ZCTAs) to assign PO Boxes to polygons with a US Census Bureau population estimate. If a hospital discharged fewer than 30 patients in a quarter, then patient ZIP Codes were anonymized and are not considered.

$$C_j = \sum_{i=1}^n \left(\frac{x_{ij}}{T_i} \cdot P_i\right)$$ (1)

$C_j$ is the catchment population for hospital $j$, where $m$ is the total number of hospitals in Texas receiving patients for a specific disease $D$. 
&nbsp;&nbsp;&nbsp;&nbsp;For example, COVID-19 has the ICD-10-CM code U071, so we consider all patients with U071 as their principal diagnosis and any who had U071 present on admittance (secondary diagnosis). <br/>
$x_{ij}$ is the count of patients with a mailing address in ZCTA $i$ discharged from hospital $j$ for disease $D$ over a specified span of time $Q$. <br/>
$T_i = \sum_{j=1}^m x_{ij}$ is the total patients originating in ZCTA $i$ that went to any hospital. <br/>
$P_i$ is the ZCTA population estimate from the 5-year American Community Survey 2018-2022 obtained from [tidycensus](https://walker-data.com/tidycensus/). <br/>
