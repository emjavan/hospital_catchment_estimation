#!/bin/bash

# Run inside DSHS_IP_RDF/code/
# Code filters ICD-10 codes from the input file based on the ICD-10 codes in the CSV file
# Will take any codes that start with the character string in CSV file

# Check for arguments
if [ $# -ne 3 ]; then
   echo "Usage: $0 <input_txt_file> <icd10_csv_file> <output_txt_file>"
   exit 1
fi

input_file="$1"
icd10_file="$2"

# make output file a passed value so I can specify the path to write files to
#output_file="${input_file%.txt}_filtered.txt"
output_file="$3"

# Get the total number of rows before filtering (excluding header)
echo -e "\n\n"
filtered_rows_prev=$(awk 'END{print NR-1}' "$input_file")
echo "Total rows before filtering: $filtered_rows_prev"

# Read the ICD-10 codes from the CSV, skipping the header
icd10_patterns=$(awk -F',' 'NR > 1 {print $1}' "$icd10_file" | tr '\n' '|')
icd10_patterns="^(${icd10_patterns%|})"

# Get the header line from the input file
header=$(head -n 1 "$input_file")

# Detect diagnostic columns (PRINC_DIAG_CODE, OTH_DIAG_CODE_1, OTH_DIAG_CODE_2, OTH_DIAG_CODE_3)
col_indices=$(echo "$header" | awk -F'\t' '{
    for (i=1; i<=NF; i++) {
        if ($i ~ /PRINC_DIAG_CODE|OTH_DIAG_CODE_[1-24]/) {
            printf i ","
        }
    }
}' | sed 's/,$//')

# Ensure we found columns
if [ -z "$col_indices" ]; then
    echo "No diagnostic code columns found in input file >_<"
    exit 1
fi

# Print the header to the output file
echo "$header" > "$output_file"

# Filter the rows based on ICD-10 codes in the diagnostic columns
awk -F'\t' -v cols="$col_indices" -v patterns="$icd10_patterns" '
BEGIN {
    split(cols, col_idx, ",")
}
NR > 1 {
    for (i in col_idx) {
        if ($col_idx[i] ~ patterns) {
            print $0
            next
        }
    }
}
' "$input_file" >> "$output_file"

# Get the total number of rows after filtering (excluding header)
filtered_rows=$(awk 'END{print NR-1}' "$output_file")
echo "Total rows after filtering: $filtered_rows"

echo "Filtered data written to $output_file"
echo -e "\n\n"