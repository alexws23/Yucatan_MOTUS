
##### 🔴 READ BELOW BEFORE RUNNING!!! 🔴 #####
##### NOTE: There appears to be a lag in data processing on the sensorgnome. You may need to wait 15-30 minutes after installation to download data
##### NOTE: Ports 1-4 are assumed to be for 166 radios (ie. RTLs/FunCubes). Ports 5-6 are assumed to be for 434 radios. Failure to match this may result in incorrect diagnostics
##### NOTE: For ease of legibility with reading final diagnostics, I recommend running this script with the source command (top right of panel or CTRL+SHIFT+S)

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

data$time <- as.numeric(data$time)
c <- data #%>% 
  #mutate(time = ifelse(time < 1600000000, time+209347200+6000, time))
c$time <- as.POSIXct(c$time, origin="1970-01-01")

comp <- data %>% 
  mutate(time = ifelse(time < 1600000000, time+209347200+6000, time))
comp$time <- as.POSIXct(comp$time, origin="1970-01-01")

# Remove rows starting with C, G, or S
data <- data[!substr(data$port, 1, 1) %in% c("C", "G", "S"), ]

rownames(data) <- NULL

# Keep only rows starting with p
data_166 <- data[substr(data$port, 1, 1) == "p", ]

# Keep only rows starting with T
if (length(i <- grep("T", data$port)) > 0) {
  data_434 <- data[substr(data$port, 1, 1) == "T", ]
} else{message("No 434 Ports")}


rm(list = c("split_fixed","split_lines","lines"))

message("Data prepared for diagnostic analysis")

data$time <- as.numeric(data$time)
data$time <- as.POSIXct(data$time, origin="1970-01-01")

write.csv(data, file = "Kanella_data_error.csv")
write.csv(data, file = "Spunky_data_error.csv")

test <- read_lines(file = "C:/Users/awsmilor/Desktop/Diagnostics/SG-CAA1RPI36B13-2026-03-12T13_13_51.928Z/sgv2-CAA1RPI36B13-002-2019-05-11T20-03-17.1690P-all.txt.txt")
test <- read_lines(file = "sgv2-6EEERPI3D9AB-004-2025-08-26T12-45-21.8630Z-all.txt/sgv2-6EEERPI3D9AB-004-2025-08-26T12-45-21.8630Z-all.txt")
split <- strsplit(test, ",")
fixed <- lapply(split, function(x) {
  length(x) <- 6   # pad with NAs or truncate to length 6
  x
})
sample <- as.data.frame(do.call(rbind, fixed), stringsAsFactors = FALSE)
colnames(sample) <- c("port","time","freq","power","noise","S2N")

sample$time <- as.numeric(sample$time)

c <- sample %>% 
  mutate(time = ifelse(time < 1600000000, time+209347200+6000, time))

c$time <- as.POSIXct(c$time, origin="1970-01-01")
sample$time <- as.POSIXct(sample$time, origin="1970-01-01")

G <- c %>% 
  filter(port %in% c("C", "G"))

library(dplyr)
library(lubridate)
library(zoo)

# Identify bad timestamps
df <- c %>%
  mutate(
    bad_time = year(time) == 2019 &
      port %in% c("p1", "p2", "p3", "p4", "C")
  )

# Store valid timestamps only
df <- df %>%
  mutate(
    good_time = ifelse(!bad_time & port == "G", time, as.POSIXct(NA))
  )

# Fill nearest valid timestamps
df <- df %>%
  mutate(
    prev_good = na.locf(good_time, na.rm = FALSE),
    next_good = na.locf(good_time, fromLast = TRUE, na.rm = FALSE)
  )

# Create groups of consecutive bad timestamps
df <- df %>%
  mutate(
    block = cumsum(!bad_time)
  )

# Correct timestamps while preserving offsets
df_corrected <- df %>%
  group_by(block) %>%
  mutate(
    
    # First bad timestamp in block
    first_bad = first(time[bad_time]),
    
    # Offset from first bad timestamp
    offset_sec = as.numeric(time - first_bad, units = "secs"),
    
    # Shift entire block to begin at previous good timestamp
    corrected_time = case_when(
      bad_time ~ as.POSIXct(prev_good + offset_sec, origin = "1970-01-01"),
      TRUE ~ time
    )
    
  ) %>%
  ungroup()

ports <- df_corrected[!substr(data$port, 1, 1) %in% c("C", "G", "S"), ]

ports %>% 
  filter(year(corrected_time) != 2024) %>% 
  ggplot(aes(x = corrected_time, y = as.numeric(S2N), color = port))+
  geom_point() +
  theme_minimal()

data <- ports %>%
  select(c("port", "time", "freq", "power", "noise", "S2N"))

data$time <- ports$corrected_time
