#############################################################################################
# Author: Caleb Frankenberger                                                               # 
# Date: 11/07/2024                                                                          #
# Project: NHANES Diabetes Risk Factor Analysis                                             #
# Purpose: Convert NHANES .XPT files to .CSV format for SAS import                          #
# Description: This script loads .XPT files from the NHANES BP_Lifestyle_Analysis           #
#              project, inspects their structure, and exports them to .CSV format.          #
# Note: Once exported to CSV, these files will be imported into SAS for further analysis.   #
#############################################################################################

# Load necessary library
library(haven)

# Set the working directory to the location of the raw NHANES data files
setwd("C:/path/to/project/NHANES_Risk_Factor_Analysis/Data/RAW")

# Define the names of the NHANES .XPT files (without extensions)
file_names <- c("BMX", "BPXO", "DEMO", "DIQ")

# Generate file paths with .XPT and .CSV extensions
file_names_xpt <- paste("P_", file_names, ".XPT", sep="")
file_names_csv <- paste("P_", file_names, ".csv", sep="")

# Loop through each .XPT file to load, inspect, and save as CSV
for(i in 1:length(file_names_xpt)) {
  # Load the .XPT file into R
  data <- read_xpt(file_names_xpt[i])
  
  # Print the file name, first few rows, and structure for verification
  cat("File:", file_names_xpt[i], "\n")
  print(head(data))       # Display the first few rows of the data
  str(data)               # Display the structure of the data
  cat("\n---------------------------------\n")
  
  # Export the loaded data to .CSV format with the same base name
  write.csv(data, file_names_csv[i], row.names=FALSE, na="")
}

# The .CSV files are now saved in the working directory and ready for import into SAS

## End of script ##