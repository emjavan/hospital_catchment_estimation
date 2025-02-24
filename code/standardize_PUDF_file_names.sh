#!/bin/bash

# The PUDF files as they come order the data as QUARTER_NUM Q YEAR
# This doesn't sort temporally when you look at all the files in a dir
# Renaming to make it easier to quickly see which files you have/don't


###############################
# STANDARDIZE PUDF FOLDER NAMES
###############################

# Specify the path to the directory containing the folders
parent_dir="../private_input_data/OG_PUDF_DATA"

# Loop through the matching folders in the specified directory
for folder in "$parent_dir/PUDF "*; do
  # Extract year, quarter, and the rest of the folder name
  new_name=$(echo "$folder" | sed -E 's|.*/PUDF ([0-9])Q([0-9]{4}) (.+)|PUDF_\2Q\1_\3|')
  # Rename the folder
  mv "$folder" "$parent_dir/$new_name"
done

##############################
# STANDARDIZE PUDF FILES NAMES
##############################

# Base directory containing the files
BASE_DIR="../private_input_data/OG_PUDF_DATA"

# Function to replace double underscores with a single underscore in all file names
remove_double_underscores() {
  find "$BASE_DIR" -type f -name "*__*" | while read -r file; do
    dir=$(dirname "$file")
    base=$(basename "$file")
    # Replace any occurrence of double underscore with a single underscore
    new_base=$(echo "$base" | sed 's/__/_/g')
    # Only rename if there's a change
    if [ "$base" != "$new_base" ]; then
      echo "Cleaning: $file -> $dir/$new_base"
      mv "$file" "$dir/$new_base"
    fi
  done
}

# Call the cleaning function before processing the filenames
remove_double_underscores

# Rename files to the 2023 standard format
standardize_filenames() {
  find "$BASE_DIR" -type f | while read -r file; do
    dir=$(dirname "$file")
    filename=$(basename "$file")

    # Extract components from the filename using a robust regex
    if [[ "$filename" =~ ([a-zA-Z_]+)(_base[0-9]*|_grouper|_charges|_facility_type)?_?([0-9]+)q([0-9]+)(_tab\.txt) ]]; then
      prefix="${BASH_REMATCH[1]}"          # e.g., "PUDF"
      type_suffix="${BASH_REMATCH[2]}"     # e.g., "_base1", "_grouper"
      quarter="${BASH_REMATCH[3]}"         # e.g., "4"
      year="${BASH_REMATCH[4]}"            # e.g., "2023"
      suffix="${BASH_REMATCH[5]}"          # e.g., "_tab.txt"

      # Map old prefixes and type suffixes to new standardized names
      case "$type_suffix" in
        _base1) standardized_prefix="IP_PUDF_BASE_DATA_1" ;;
        _base2) standardized_prefix="IP_PUDF_BASE_DATA_2" ;;
        _grouper) standardized_prefix="IP_PUDF_GROUPER" ;;
        _charges) standardized_prefix="IP_PUDF_CHARGES" ;;
        *) standardized_prefix="${prefix}${type_suffix}" ;; # Fallback to original
      esac

      # Create the new filename
      new_filename="${standardized_prefix}_${year}Q${quarter}_${suffix}"

      # Rename the file if necessary
      if [[ "$filename" != "$new_filename" ]]; then
        echo "Renaming: $file -> $dir/$new_filename"
        mv "$file" "$dir/$new_filename"
      fi
    else
      echo "Skipping: $file (no match found)"
    fi
  done
}

# Execute the function
standardize_filenames

##########################################
# FIX PUDF FILES NAMES with IP_PUDF PREFIX
##########################################

# Function to fix 2023+ files that already start with IP PUDF
fix_filenames() {
  find "$BASE_DIR" -type f | while read -r file; do
    dir=$(dirname "$file")
    filename=$(basename "$file")

# IP_PUDF_BASE_DATA_1_2023Q1tab.txt

    # Match files with year and quarter, and optional double underscore
    # little q is the seperator in the original file names
    if [[ "$filename" =~ IP_PUDF_(.*)_([0-9]+)q([0-9]+)__(tab\.txt) ]]; then
      prefix="IP_PUDF_${BASH_REMATCH[1]}" # Prefix (e.g., BASE_DATA_1)
      quarter="${BASH_REMATCH[2]}"       # Quarter (e.g., 4)
      year="${BASH_REMATCH[3]}"          # Year (e.g., 2023)
      suffix="${BASH_REMATCH[4]}"        # Suffix (_tab.txt)

      # Construct the new filename in standardized format
      new_filename="${prefix}_${year}Q${quarter}${suffix}"
    
    # Capital Q seperates the cleaned file names
    # replace double underscore with single
    elif [[ "$filename" =~ IP_PUDF_(.*)_([0-9]+)Q([0-9]+)__(tab\.txt) ]]; then
      prefix="IP_PUDF_${BASH_REMATCH[1]}" # Prefix (e.g., BASE_DATA_1)
      quarter="${BASH_REMATCH[3]}"       # Quarter (e.g., 4)
      year="${BASH_REMATCH[2]}"          # Year (e.g., 2023)
      suffix="${BASH_REMATCH[4]}"        # Suffix (_tab.txt)

      # Construct the new filename in standardized format
      new_filename="${prefix}_${year}Q${quarter}_${suffix}"

    else
      # Skip files without IP_PUDF or mismatched formats
      echo "Skipping: $file (no match for standardization)"
      continue
    fi

    # Check if renaming is necessary
    if [[ "$filename" != "$new_filename" ]]; then
      echo "Renaming: $file -> $dir/$new_filename"
      mv "$file" "$dir/$new_filename"
    fi
  done
}

# Execute the function
fix_filenames

# Final check: Ensure all files start with "IP_PUDF_" and capitalization of filename after
fix_missing_ip_prefix() {
  find "$BASE_DIR" -type f | while read -r file; do
    dir=$(dirname "$file")
    filename=$(basename "$file")
    # Check if filename starts with "PUDF_"
    if [[ "$filename" =~ ^PUDF_ ]]; then
      # Split the filename at underscores
      prefix=$(echo "$filename" | cut -d'_' -f1)         # "PUDF"
      token=$(echo "$filename" | cut -d'_' -f2)          # e.g., "charges"
      rest=$(echo "$filename" | cut -d'_' -f3-)          # the rest of the filename

      # Convert token to uppercase
      token_upper=$(echo "$token" | tr '[:lower:]' '[:upper:]')
      new_filename="IP_${prefix}_${token_upper}_${rest}"
      
      echo "Fixing prefix: Renaming $file -> $dir/$new_filename"
      mv "$file" "$dir/$new_filename"
    fi
  done
}

fix_missing_ip_prefix