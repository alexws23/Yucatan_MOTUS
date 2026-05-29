### Load Required Packages
library(tidyverse)
library(patchwork)

######## Step 1: Extract MOTUS Data ####################
# Starting point and output file
############# Need to extract the zipped file
### Just right click and click on "Extract All"
# Extract the file into a folder on the Desktop named Diagnostics
# Modify the working directory below.

#setwd("C:/Users/🟡YOURUSERNAME🟡/Desktop/Diagnostics/")
setwd("C:/Users/awsmilor/Desktop/Diagnostics/")

### One Last Step: Set the desired gap length ###
gap_length = 40 #Default is 40 seconds, meaning all missed beacon pulses will be highlighted. Set a higher number if you are only interested in long gaps

############################################
### 🟢 Run Script 🟢 ###
rm(list = setdiff(ls(), c("gap_length")))

dir <- list.dirs()

if(length(i <- grep("SG-", dir)))
  dir <- dir[i]
dir <- gsub("./","",dir)

folder_path <- paste("./",dir, sep = "")
output_file <- paste("./",dir,"/all_data.txt", sep = "")
rm(list = c("i","dir"))

# Function to process files recursively
process_files_recursive <- function(folder_path) {
  # List all files and directories in the current folder
  files <- list.files(folder_path, full.names = TRUE)
  
  # Filter for directories
  directories <- files[file.info(files)$isdir]
  
  # Filter for .txt.gz files
  txt_gz_files <- files[grepl("\\.txt\\.gz$", files)]
  
  # Process .txt.gz files
  for (file_path in txt_gz_files) {
    gz_con <- gzfile(file_path, "rt")  # Open connection to gzipped file
    file_content <- readLines(gz_con)  # Read content using readLines
    close(gz_con)  # Close the connection
    
    file_name <- tools::file_path_sans_ext(basename(file_path))  # Extract file name without extension
    
    output_file_path <- file.path(folder_path, paste0(file_name, ".txt"))  # Define output file path
    
    writeLines(file_content, output_file_path)  # Write content to plain text file
  }
  
  # Recursively process subdirectories
  for (dir_path in directories) {
    process_files_recursive(dir_path)
  }
}


# Call the recursive function to process all files
process_files_recursive(folder_path)


##############################################################################

# Function to combine all text files recursively
combine_txt_files <- function(folder_path, output_file) {
  # List all files and directories in the current folder
  files <- list.files(folder_path, full.names = TRUE, recursive = TRUE)
  
  # Filter for .txt files
  txt_files <- files[grepl("\\.txt$", files)]
  
  # Initialize an empty character vector to store combined content
  combined_content <- character()
  
  # Process each .txt file
  for (file_path in txt_files) {
    file_content <- readLines(file_path, warn = FALSE)  # Read content of the file
    combined_content <- c(combined_content, file_content)  # Append content to combined_content
  }
  
  # Write combined content to the output file
  writeLines(combined_content, output_file)
}


# Call function to combine all text files 
combine_txt_files(folder_path, output_file)
message("Raw Data Processed Successfully")

###############################################################
##### Step 2: Prepare Data for Analysis
# read in the file
# Read the file line by line
lines <- readLines(output_file)

# Split each line by comma
split_lines <- strsplit(lines, ",")

# Force each row to have exactly 6 columns
split_fixed <- lapply(split_lines, function(x) {
  length(x) <- 6   # pad with NAs or truncate to length 6
  x
})

# Convert to data frame
data <- as.data.frame(do.call(rbind, split_fixed), stringsAsFactors = FALSE)

# Assign column names
colnames(data) <- c("port","time","freq","power","noise","S2N")

rownames(data) <- NULL

write.csv(data, file = "all_data.csv") #create a new csv of all the combined data

rm(list = c("split_fixed","split_lines","lines"))

message("Data prepared for diagnostic analysis")
