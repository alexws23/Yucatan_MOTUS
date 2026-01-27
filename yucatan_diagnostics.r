
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
 
### One Last Step: Set the desired gap length ###
gap_length = 40 #Default is 40 seconds, meaning all missed beacon pulses will be highlighted. Set a higher number if you are only interested in long gaps
#gap_length = 5 #If using a newly deployed beacon, I suggest setting the length to 5, since new beacons pulse every 5 seconds.

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

##########################################################################
### Step 3: Locate all detections of 166 test transmitter
############# Filter #####################
## baseline_freq <- 166.376
## tag_freq <- 166.380
upper_freq <- 5
lower_freq <-3

# a pulse needed to be at least 15 milliseconds
signal_duration <- 0.015

##### enter the tag you are looking for

code <- "732"

#tag_ID_1 <- 0.159
tag_ID_1 <- 0.159
#tag_ID_2 <- 0.156
tag_ID_2 <- 0.156
#tag_ID_3 <- 0.024
tag_ID_3 <- 0.034
#tag_PI <- 40
tag_PI <- 39.6

message("Locating Detections of 732 LOTEK Test Transmitter...")

##################################################################################################
data <- data_166

data$time <- as.numeric(data$time)

##################################################################################################
########## subset by port / antenna ######
data_p1 <- data[which(data$port == "p1"),]
data_p2 <- data[which(data$port == "p2"),]
data_p3 <- data[which(data$port == "p3"),]
data_p4 <- data[which(data$port == "p4"),]
data_p5 <- data[which(data$port == "p5"),]
data_p6 <- data[which(data$port == "p6"),]

########## difference between the sensor gnome frequency and the frequency of tranmitter(s)
# freq set at 166.376
data_p1_2 <- subset(data_p1,freq > lower_freq & freq < upper_freq)
data_p2_2 <- subset(data_p2,freq > lower_freq & freq < upper_freq)
data_p3_2 <- subset(data_p3,freq > lower_freq & freq < upper_freq)
data_p4_2 <- subset(data_p4,freq > lower_freq & freq < upper_freq)
data_p5_2 <- subset(data_p5,freq > lower_freq & freq < upper_freq)
data_p6_2 <- subset(data_p6,freq > lower_freq & freq < upper_freq)

################################ Code to produce all possible sequences of known pulse code ###################
###############
sequence <- c(tag_ID_1, tag_ID_2, tag_ID_3)

# Function to generate all possible combinations
generate_combinations <- function(sequence) {
  combinations <- list()
  for (i in seq_along(sequence)) {
    for (j in c(-0.001, 0, 0.001)) {
      for (k in c(-0.001, 0, 0.001)) {
        for (l in c(-0.001, 0, 0.001)) {
          combination <- sequence + c(j, k, l)
          combinations <- c(combinations, list(combination))
        }
      }
    }
  }
  return(combinations)
}

# Generate all possible combinations
sequence_to_ID <- generate_combinations(sequence)

###########################################################################################################
######################################## Port 1 ###########################################################
###########################################################################################################

###################### Get time difference #################################################################
df_p1 <- data_p1_2 %>%
  mutate(diff = time - lag(time, default = first(time)))
options(digits=3)

######### If difference in time is less than a certain length delete the row  ######  
df_p1_2 <- subset(df_p1, diff > signal_duration )

################## round to 3 decimal places #############################################
df_p1_2$diff <- round(df_p1_2$diff, 3)

sequence_found <- sapply(seq_along(df_p1_2$diff), function(i) {
  any(sapply(sequence_to_ID, function(sequence_to_ID) {
    identical(df_p1_2$diff[i:(i + length(sequence_to_ID) - 1)], sequence_to_ID)
  }))
})

df_p1_2$targets <- as.integer(sequence_found)

sum(df_p1_2$targets)

# Function to change the next three cells to 1 after encountering a 1
change_next_three_cells <- function(column) {
  for (i in 1:(length(column) - 3)) {
    if (column[i] == 1) {
      column[i + 1] <- 2
      column[i + 2] <- 3
      column[i + 3] <- 4
    }
  }
  return(column)
}

if (nrow(df_p1_2) > 0) {

df_p1_2$targets_2 <- change_next_three_cells(df_p1_2$targets)

### If you want to delete rows with a zero
df_p1_3 <- df_p1_2[!(df_p1_2$targets_2 %in% 0),]

###########################################################
row.names(df_p1_3) <- NULL

df_p1_3$group <- rep_len(1:4, nrow(df_p1_3))

dd <- df_p1_3 %>%
  group_by(group) %>%
  mutate(n = row_number()) %>%
  spread(group, diff) %>%
  select(-n) %>%
  (function(x) { x[is.na(x)] <- 0; x })

dd$targets <- NULL
dd$targets_2 <- NULL

colnames(dd) <- c("port","time","freq_1","power_1","noise_1","s2n_1","P1_1","P2_1","P3_1","PInt_1")

dd$freq_1 <- as.numeric(dd$freq_1)

#port_1 <- dd %>% 
#    group_by(grp = as.integer(gl(n(), 4, n()))) %>% 
#    summarise(time= min(time),freq_1 = mean(freq_1),power_1 = max(power_1), noise_1 = max(noise_1), PI_1_1 = (max(P1_1)), PI_2_1 = (max(P2_1)),PI_3_1 = (max(P3_1)),PInt_1 = (max(PInt_1)))

if (nrow(dd) > 0) {
  port_1 <- dd %>%
    group_by(grp = as.integer(gl(n(), 4, n()))) %>% 
    summarise(
      time = min(time, na.rm = TRUE),
      freq_1 = mean(freq_1, na.rm = TRUE),
      power_1 = max(power_1, na.rm = TRUE),
      noise_1 = max(noise_1, na.rm = TRUE),
      S2N_1 = max(s2n_1, na.rm = TRUE),
      PI_1_1 = max(P1_1, na.rm = TRUE),
      PI_2_1 = max(P2_1, na.rm = TRUE),
      PI_3_1 = max(P3_1, na.rm = TRUE),
      PInt_1 = max(PInt_1, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  port_1 <- dd  # or port_1 <- NULL if you prefer
}

port_1$grp <- NULL

time_p1 <- port_1$time * 100
port_1_rounded <- round(time_p1)/100
port_1$time <- as.POSIXct(port_1_rounded, origin="1970-01-01")

}else{

# Name of the object you want to check
object_name <- "port_1"

# Check if the object exists in the environment
if (!exists(object_name)) {
  # Create a new object with the specified name
  assign(object_name, data.frame(time = "1999-01-01 12:00:00", freq_1 = NA, power_1 = NA, noise_1 = NA, S2N_1 = NA, PI_1_1 = NA, PI_2_1 = NA, PI_3_1 = NA,PInt_1 = NA))
  message(paste("Object", object_name, "was not found and a placeholder has been created.
                🔴 This may indicate an issue. See diagnostic recommendations below"))
} else {
  message(paste("Object", object_name, "already exists."))
}
}

###########################################################################################################
######################################## Port 2 ###########################################################
###########################################################################################################

###################### Get time difference #################################################################
df_p2 <- data_p2_2 %>%
  mutate(diff = time - lag(time, default = first(time)))
options(digits=3)

######### If difference in time is less than 0.001 delete the row  ######  
df_p2_1 <- subset(df_p2, diff > signal_duration )

################## round to 3 decimal places #############################################
df_p2_1$diff <- round(df_p2_1$diff, 3)

########################################################################################
sequence_found <- sapply(seq_along(df_p2_1$diff), function(i) {
  any(sapply(sequence_to_ID, function(sequence_to_ID) {
    identical(df_p2_1$diff[i:(i + length(sequence_to_ID) - 1)], sequence_to_ID)
  }))
})

df_p2_1$targets <- as.integer(sequence_found)

sum(df_p2_1$targets)

# Function to change the next three cells to 1 after encountering a 1
change_next_three_cells <- function(column) {
  for (i in 1:(length(column) - 3)) {
    if (column[i] == 1) {
      column[i + 1] <- 2
      column[i + 2] <- 3
      column[i + 3] <- 4
    }
  }
  return(column)
}

if (nrow(df_p2_1) > 0) {
  
df_p2_1$targets_2 <- change_next_three_cells(df_p2_1$targets)

### If you want to delete rows with a zero
df_p2_4 <- df_p2_1[!(df_p2_1$targets_2 %in% 0),]

###########################################################

row.names(df_p2_4) <- NULL
df_p2_4$group <- rep_len(1:4, nrow(df_p2_4))

dd <- df_p2_4 %>%
  group_by(group) %>%
  mutate(n = row_number()) %>%
  spread(group, diff) %>%
  select(-n) %>%
  (function(x) { x[is.na(x)] <- 0; x })

dd$targets <- NULL
dd$targets_2 <- NULL

colnames(dd) <- c("port","time","freq_2","power_2","noise_2","s2n_2","P1_2","P2_2","P3_2","PInt_2")

dd$freq_2 <- as.numeric(dd$freq_2)

#port_2 <- dd %>% 
#    group_by(grp = as.integer(gl(n(), 4, n()))) %>% 
#    summarise(time= min(time),freq_2 = mean(freq_2),power_2 = max(power_2), noise_2 = max(noise_2), PI_1_2 = (max(P1_2)), PI_2_2 = (max(P2_2)),PI_3_2 = (max(P3_2)),PInt_2 = (max(PInt_2)))

if (nrow(dd) > 0) {
  port_2 <- dd %>%
    group_by(grp = as.integer(gl(n(), 4, n()))) %>%
    summarise(
      time = min(time, na.rm = TRUE),
      freq_2 = mean(freq_2, na.rm = TRUE),
      power_2 = max(power_2, na.rm = TRUE),
      noise_2 = max(noise_2, na.rm = TRUE),
      S2N_2 = max(s2n_2, na.rm = TRUE),
      PI_1_2 = max(P1_2, na.rm = TRUE),
      PI_2_2 = max(P2_2, na.rm = TRUE),
      PI_3_2 = max(P3_2, na.rm = TRUE),
      PInt_2 = max(PInt_2, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  port_1 <- dd  # or port_1 <- NULL if you prefer
}

port_2$grp <- NULL

time_p2 <- port_2$time * 100
port_2_rounded <- round(time_p2)/100
port_2$time <- as.POSIXct(port_2_rounded, origin="1970-01-01")

}else{
# Name of the object you want to check
object_name <- "port_2"

# Check if the object exists in the environment
if (!exists(object_name)) {
  # Create a new object with the specified name
  assign(object_name, data.frame(time = "1999-01-01 12:00:00", freq_2 = NA, power_2 = NA, noise_2 = NA, S2N_2 = NA, PI_1_2 = NA, PI_2_2 = NA, PI_3_2 = NA,PInt_2 = NA))
  message(paste("Object", object_name, "was not found and a placeholder has been created.
                🔴 This may indicate an issue. See diagnostic recommendations below"))
} else {
  message(paste("Object", object_name, "already exists."))
}
}

###########################################################################################################
######################################## Port 3 ###########################################################
###########################################################################################################

###################### Get time difference #################################################################
df_p3 <- data_p3_2 %>%
  mutate(diff = time - lag(time, default = first(time)))
options(digits=3)

######### If difference in time is less than a set amount delete the row  ######  
df_p3_1 <- subset(df_p3, diff > signal_duration )


################## round to 3 decimal places #############################################
df_p3_1$diff <- round(df_p3_1$diff, 3)

##########################################################################################

sequence_found <- sapply(seq_along(df_p3_1$diff), function(i) {
  any(sapply(sequence_to_ID, function(sequence_to_ID) {
    identical(df_p3_1$diff[i:(i + length(sequence_to_ID) - 1)], sequence_to_ID)
  }))
})

df_p3_1$targets <- as.integer(sequence_found)

sum(df_p3_1$targets)

# Function to change the next three cells to 1 after encountering a 1

change_next_three_cells <- function(column) {
  for (i in 1:(length(column) - 3)) {
    if (column[i] == 1) {
      column[i + 1] <- 2
      column[i + 2] <- 3
      column[i + 3] <- 4
    }
  }
  return(column)
}

if (nrow(df_p3_1) > 0) {

df_p3_1$targets_2 <- change_next_three_cells(df_p3_1$targets)

### If you want to delete rows with a zero
df_p3_4 <- df_p3_1[!(df_p3_1$targets_2 %in% 0),]

###########################################################

row.names(df_p3_4) <- NULL
df_p3_4$group <- rep_len(1:4, nrow(df_p3_4))

dd <- df_p3_4 %>%
  group_by(group) %>%
  mutate(n = row_number()) %>%
  spread(group, diff) %>%
  select(-n) %>%
  (function(x) { x[is.na(x)] <- 0; x })

dd$targets <- NULL
dd$targets_2 <- NULL

colnames(dd) <- c("port","time","freq_3","power_3","noise_3","s2n_3","P1_3","P2_3","P3_3","PInt_3")

dd$freq_3 <- as.numeric(dd$freq_3)

port_3 <- dd %>% 
  group_by(grp = as.integer(gl(n(), 4, n()))) %>% 
  summarise(time= min(time),
            freq_3 = mean(freq_3),
            power_3 = max(power_3), 
            noise_3 = max(noise_3), 
            S2N_3 = max(s2n_3, na.rm = TRUE),
            PI_1_3 = max(P1_3), 
            PI_2_3 = max(P2_3),
            PI_3_3 = max(P3_3),
            PInt_3 = max(PInt_3))

port_3$grp <- NULL

time_p3 <- port_3$time * 100
port_3_rounded <- round(time_p3)/100
port_3$time <- as.POSIXct(port_3_rounded, origin="1970-01-01")


} else {
  # Name of the object you want to check
  object_name <- "port_3"
# Check if the object exists in the environment
if (!exists(object_name)) {
  # Create a new object with the specified name
  assign(object_name, data.frame(time = "1999-01-01 12:00:00", freq_3 = NA, power_3 = NA, noise_3 = NA, S2N_3 = NA, PI_1_3 = NA, PI_2_3 = NA, PI_3_3 = NA,PInt_3 = NA))
  message(paste("Object", object_name, "was not found and a placeholder has been created.
                🔴 This may indicate an issue. See diagnostic recommendations below"))
} else {
  message(paste("Object", object_name, "already exists."))
}
}

###########################################################################################################
######################################## Port 4 ###########################################################
###########################################################################################################

###################### Get time difference #################################################################
df_p4 <- data_p4_2 %>%
  mutate(diff = time - lag(time, default = first(time)))
options(digits=3)

######### If difference in time is less than a set amount delete the row  ######  
df_p4_1 <- subset(df_p4, diff > signal_duration )

################## round to 3 decimal places #############################################
df_p4_1$diff <- round(df_p4_1$diff, 3)

#########################################################################################
#########################################################################################

sequence_found <- sapply(seq_along(df_p4_1$diff), function(i) {
  any(sapply(sequence_to_ID, function(sequence_to_ID) {
    identical(df_p4_1$diff[i:(i + length(sequence_to_ID) - 1)], sequence_to_ID)
  }))
})

df_p4_1$targets <- as.integer(sequence_found)

sum(df_p4_1$targets)

# Function to change the next three cells to 1 after encountering a 1
change_next_three_cells <- function(column) {
  for (i in 1:(length(column) - 3)) {
    if (column[i] == 1) {
      column[i + 1] <- 2
      column[i + 2] <- 3
      column[i + 3] <- 4
    }
  }
  return(column)
}

if (nrow(df_p4_1) > 0) {

df_p4_1$targets_2 <- change_next_three_cells(df_p4_1$targets)

### If you want to delete rows with a zero
df_p4_1 <- df_p4_1[!(df_p4_1$targets_2 %in% 0),]

###########################################################

row.names(df_p4_1) <- NULL
df_p4_1$group <- rep_len(1:4, nrow(df_p4_1))

dd <- df_p4_1 %>%
  group_by(group) %>%
  mutate(n = row_number()) %>%
  spread(group, diff) %>%
  select(-n) %>%
  (function(x) { x[is.na(x)] <- 0; x })

dd$targets <- NULL
dd$targets_2 <- NULL

colnames(dd) <- c("port","time","freq_4","power_4","noise_4","s2n_4","P1_4","P2_4","P3_4","PInt_4")

dd$freq_4 <- as.numeric(dd$freq_4)

port_4 <- dd %>% 
  group_by(grp = as.integer(gl(n(), 4, n()))) %>% 
  summarise(time= min(time, na.rm=TRUE),
            freq_4 = mean(freq_4, na.rm=TRUE),
            power_4 = max(power_4, na.rm=TRUE), 
            noise_4 = max(noise_4, na.rm=TRUE),
            S2N_4 = max(s2n_4, na.rm = TRUE),
            PI_1_4 = max(P1_4, na.rm=TRUE),
            PI_2_4 = max(P2_4, na.rm=TRUE),
            PI_3_4 = max(P3_4, na.rm=TRUE),
            PInt_4 = max(PInt_4, na.rm=TRUE))

port_4$grp <- NULL

time_p4 <- port_4$time * 100
port_4_rounded <- round(time_p4)/100
port_4$time <- as.POSIXct(port_4_rounded, origin="1970-01-01")

}else{

# Name of the object you want to check
object_name <- "port_4"

# Check if the object exists in the environment
if (!exists(object_name)) {
  # Create a new object with the specified name
  assign(object_name, data.frame(time = "1999-01-01 12:00:00", freq_4 = NA, power_4 = NA, noise_4 = NA, S2N_4 = NA, PI_1_4 = NA, PI_2_4 = NA, PI_3_4 = NA,PInt_4 = NA))
  message(paste("Object", object_name, "was not found and a placeholder has been created.
                🔴 This may indicate an issue. See diagnostic recommendations below"))
} else {
  message(paste("Object", object_name, "already exists."))
}
}

###########################################################################################################
######################################## Port 5 ###########################################################
###########################################################################################################

###################### Get time difference #################################################################
df_p5 <- data_p5_2 %>%
  mutate(diff = time - lag(time, default = first(time)))
options(digits=3)

######### If difference in time is less than a set amount delete the row  ######  
df_p5_1 <- subset(df_p5, diff > signal_duration )

################## round to 3 decimal places #############################################
df_p5_1$diff <- round(df_p5_1$diff, 3)

#########################################################################################
#########################################################################################

sequence_found <- sapply(seq_along(df_p5_1$diff), function(i) {
  any(sapply(sequence_to_ID, function(sequence_to_ID) {
    identical(df_p5_1$diff[i:(i + length(sequence_to_ID) - 1)], sequence_to_ID)
  }))
})

df_p5_1$targets <- as.integer(sequence_found)

sum(df_p5_1$targets)

# Function to change the next three cells to 1 after encountering a 1
change_next_three_cells <- function(column) {
  for (i in 1:(length(column) - 3)) {
    if (column[i] == 1) {
      column[i + 1] <- 2
      column[i + 2] <- 3
      column[i + 3] <- 4
    }
  }
  return(column)
}

if (nrow(df_p5_1) > 0) {
  
  df_p5_1$targets_2 <- change_next_three_cells(df_p5_1$targets)
  
  ### If you want to delete rows with a zero
  df_p5_1 <- df_p5_1[!(df_p5_1$targets_2 %in% 0),]
  
  if (nrow(df_p5_1) > 0) {
  ###########################################################
  
  row.names(df_p5_1) <- NULL
  df_p5_1$group <- rep_len(1:4, nrow(df_p5_1))
  
  dd <- df_p5_1 %>%
    group_by(group) %>%
    mutate(n = row_number()) %>%
    spread(group, diff) %>%
    select(-n) %>%
    (function(x) { x[is.na(x)] <- 0; x })
  
  dd$targets <- NULL
  dd$targets_2 <- NULL
  
  colnames(dd) <- c("port","time","freq_5","power_5","noise_5","s2n_5","P1_5","P2_5","P3_5","PInt_5")
  
  dd$freq_5 <- as.numeric(dd$freq_5)
  
  port_5 <- dd %>% 
    group_by(grp = as.integer(gl(n(), 4, n()))) %>% 
    summarise(time= min(time, na.rm=TRUE),
              freq_5 = mean(freq_5, na.rm=TRUE),
              power_5 = max(power_5, na.rm=TRUE), 
              noise_5 = max(noise_5, na.rm=TRUE),
              S2N_5 = max(s2n_5, na.rm = TRUE),
              PI_1_5 = max(P1_5, na.rm=TRUE),
              PI_2_5 = max(P2_5, na.rm=TRUE),
              PI_3_5 = max(P3_5, na.rm=TRUE),
              PInt_5 = max(PInt_5, na.rm=TRUE))
  
  port_5$grp <- NULL
  
  time_p5 <- port_5$time * 100
  port_5_rounded <- round(time_p5)/100
  port_5$time <- as.POSIXct(port_5_rounded, origin="1970-01-01")
  }else{
    # Name of the object you want to check
    object_name <- "port_5"
    
    # Check if the object exists in the environment
    if (!exists(object_name)) {
      # Create a new object with the specified name
      assign(object_name, data.frame(time = "1999-01-01 12:00:00", freq_5 = NA, power_5 = NA, noise_5 = NA, S2N_5 = NA, PI_1_5 = NA, PI_2_5 = NA, PI_3_5 = NA,PInt_5 = NA))
      message(paste("Object", object_name, "was not found and a placeholder has been created.
                🔴 This may indicate an issue. See diagnostic recommendations below"))
    } else {
      message(paste("Object", object_name, "already exists."))
    }}
}else{
  
  # Name of the object you want to check
  object_name <- "port_5"
  
  # Check if the object exists in the environment
  if (!exists(object_name)) {
    # Create a new object with the specified name
    assign(object_name, data.frame(time = "1999-01-01 12:00:00", freq_5 = NA, power_5 = NA, noise_5 = NA, S2N_5 = NA, PI_1_5 = NA, PI_2_5 = NA, PI_3_5 = NA,PInt_5 = NA))
    message(paste("Object", object_name, "was not found and a placeholder has been created.
                🔴 This may indicate an issue. See diagnostic recommendations below"))
  } else {
    message(paste("Object", object_name, "already exists."))
  }
}


###########################################################################################################
######################################## Port 6 ###########################################################
###########################################################################################################

###################### Get time difference #################################################################
df_p6 <- data_p6_2 %>%
  mutate(diff = time - lag(time, default = first(time)))
options(digits=3)

######### If difference in time is less than a set amount delete the row  ######  
df_p6_1 <- subset(df_p6, diff > signal_duration )

################## round to 3 decimal places #############################################
df_p6_1$diff <- round(df_p6_1$diff, 3)

#########################################################################################
#########################################################################################

sequence_found <- sapply(seq_along(df_p6_1$diff), function(i) {
  any(sapply(sequence_to_ID, function(sequence_to_ID) {
    identical(df_p6_1$diff[i:(i + length(sequence_to_ID) - 1)], sequence_to_ID)
  }))
})

df_p6_1$targets <- as.integer(sequence_found)

sum(df_p6_1$targets)

# Function to change the next three cells to 1 after encountering a 1
change_next_three_cells <- function(column) {
  for (i in 1:(length(column) - 3)) {
    if (column[i] == 1) {
      column[i + 1] <- 2
      column[i + 2] <- 3
      column[i + 3] <- 4
    }
  }
  return(column)
}

if (nrow(df_p6_1) > 0) {
  
  df_p6_1$targets_2 <- change_next_three_cells(df_p6_1$targets)
  
  ### If you want to delete rows with a zero
  df_p6_1 <- df_p6_1[!(df_p6_1$targets_2 %in% 0),]
  
  if (nrow(df_p5_1) > 0) {
  ###########################################################
  
  row.names(df_p6_1) <- NULL
  df_p6_1$group <- rep_len(1:4, nrow(df_p6_1))
  
  dd <- df_p6_1 %>%
    group_by(group) %>%
    mutate(n = row_number()) %>%
    spread(group, diff) %>%
    select(-n) %>%
    (function(x) { x[is.na(x)] <- 0; x })
  
  dd$targets <- NULL
  dd$targets_2 <- NULL
  
  colnames(dd) <- c("port","time","freq_6","power_6","noise_6","s2n_6","P1_6","P2_6","P3_6","PInt_6")
  
  dd$freq_6 <- as.numeric(dd$freq_6)
  
  port_6 <- dd %>% 
    group_by(grp = as.integer(gl(n(), 4, n()))) %>% 
    summarise(time= min(time, na.rm=TRUE),
              freq_6 = mean(freq_6, na.rm=TRUE),
              power_6 = max(power_6, na.rm=TRUE), 
              noise_6 = max(noise_6, na.rm=TRUE),
              S2N_6 = max(s2n_6, na.rm = TRUE),
              PI_1_6 = max(P1_6, na.rm=TRUE),
              PI_2_6 = max(P2_6, na.rm=TRUE),
              PI_3_6 = max(P3_6, na.rm=TRUE),
              PInt_6 = max(PInt_6, na.rm=TRUE))
  
  port_6$grp <- NULL
  
  time_p6 <- port_6$time * 100
  port_6_rounded <- round(time_p6)/100
  port_6$time <- as.POSIXct(port_6_rounded, origin="1970-01-01")
  }else{
    # Name of the object you want to check
    object_name <- "port_5"
    
    # Check if the object exists in the environment
    if (!exists(object_name)) {
      # Create a new object with the specified name
      assign(object_name, data.frame(time = "1999-01-01 12:00:00", freq_5 = NA, power_5 = NA, noise_5 = NA, S2N_5 = NA, PI_1_5 = NA, PI_2_5 = NA, PI_3_5 = NA,PInt_5 = NA))
      message(paste("Object", object_name, "was not found and a placeholder has been created.
                🔴 This may indicate an issue. See diagnostic recommendations below"))
    } else {
      message(paste("Object", object_name, "already exists."))
    }}
}else{
  
  # Name of the object you want to check
  object_name <- "port_6"
  
  # Check if the object exists in the environment
  if (!exists(object_name)) {
    # Create a new object with the specified name
    assign(object_name, data.frame(time = "1999-01-01 12:00:00", freq_6 = NA, power_6 = NA, noise_6 = NA, S2N_6 = NA, PI_1_6 = NA, PI_2_6 = NA, PI_3_6 = NA,PInt_6 = NA))
    message(paste("Object", object_name, "was not found and a placeholder has been created.
                🔴 This may indicate an issue. See diagnostic recommendations below"))
  } else {
    message(paste("Object", object_name, "already exists."))
  }
}


############################################################################################################
######## Merge the ports on time ############################################################################################################
############################################################################################################

x <- merge(port_1,port_2, by="time", all=TRUE)
x <- merge(x,port_3, by="time", all=TRUE)
x <- merge(x,port_4, by="time", all=TRUE)
x <- merge(x,port_5, by="time", all=TRUE)
x <- merge(x,port_6, by="time", all=TRUE)

all_data_166 <- x[order(x$time), ]
all_data_166$time <- as.numeric(all_data_166$time)
all_data_166$time <- as.POSIXct(all_data_166$time, origin="1970-01-01")
all_data_166 <- all_data_166 %>% 
  mutate_if(is.character,as.numeric) %>% 
  drop_na(time)



#### need write out by code ###
filename <- paste0(code, ".csv")
write.csv(all_data_166, file = filename, row.names = FALSE)

rm(list = rm(list=setdiff(ls(), c("data","data_166","data_434", "all_data_166", 
                                  "gap_length"))))

################################################################################################
# Creating Diagnostic Plots

all_data_166_long <- all_data_166 %>%
  pivot_longer(
    cols = -time,
    names_to = c(".value", "port"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  mutate(port = as.character(port)) %>% 
  drop_na(freq)

write.csv(all_data_166_long, file = "732_long.csv", row.names = FALSE)

power <- all_data_166_long %>% 
  group_by(port) %>% 
  ggplot(aes(x = time))+
  geom_point(aes(y = power, color = port))+
  geom_line(aes(y = power, color = port))+
  theme_minimal() +
  labs(title = "Signal Power")

noise <- all_data_166_long %>% 
  group_by(port) %>% 
  ggplot(aes(x = time))+
  geom_point(aes(y = noise, color = port))+
  geom_line(aes(y = noise, color = port))+
  theme_minimal()+
  labs(title = "Noise")

snr <- all_data_166_long %>% 
  group_by(port) %>% 
  ggplot(aes(x = time))+
  geom_point(aes(y = S2N, color = port))+
  geom_line(aes(y = S2N, color = port))+
  theme_minimal() +
  labs(title = "Signal to Noise Ratio")

plot <- power + noise + snr

ggsave(plot, 
       filename = "166_diagnostics.png")

### Create a plot of all gaps in the data
times_by_port <- all_data_166_long %>%
  distinct(port, time) %>%
  arrange(port, time)

times_by_port <- times_by_port %>%
  group_by(port) %>%
  mutate(
    prev_time = lag(time),
    gap_sec   = as.numeric(difftime(time, prev_time, units = "secs"))
  ) %>%
  ungroup()

gaps_by_port <- times_by_port %>%
  filter(!is.na(gap_sec) & gap_sec > gap_length) %>%
  mutate(
    gap_start = prev_time,
    gap_end   = time
  )

gaps <- ggplot() +
  # detections
  geom_point(data = times_by_port,
             aes(x = time, y = port),
             size = 4, color = "forestgreen", shape = "l") +
  
  # gaps
  geom_segment(data = gaps_by_port,
               aes(x = gap_start, xend = gap_end,
                   y = port, yend = port),
               color = "black", linewidth = 1) +
  
  labs(
    title = "Receiver Detections by Port",
    subtitle = paste("Black segments = gaps >",gap_length,"seconds"),
    x = "Time",
    y = "Port"
  ) +
  scale_y_discrete(expand = c(0, 1.7))+
  theme_minimal()+ 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16))

gaps

    ggsave(gaps, 
       filename = "166_gaps.png")

rm(list = c("noise", "plot", "power", "snr"))

message("732 Test Transmitter Detected. Data and plots saved")

#####################################################################################################
### Step 4: Locate all detections of 434 test transmitter

message("Locating 78554c3358 Test Transmitter...")

object <- "data_434"

if (exists(object)) {

data_434_clean <- data_434 %>% 
  select(-c("noise","S2N")) %>% 
  rename(tagID = freq) %>% 
  filter(tagID == "78554c3358") %>% 
  mutate(time = as.numeric(time)) %>% 
  mutate(time = as.POSIXct(time, origin="1970-01-01")) %>% 
  mutate(power = as.numeric(power)) %>% 
  arrange((time)) %>% 
  mutate(grp = rep(1:8, length.out = n())) %>% 
  filter(grp == 1) %>% 
  select(-c(grp))

power <- data_434_clean %>% 
  group_by(port) %>% 
  ggplot(aes(x = time))+
  geom_point(aes(y = power, color = port))+
  geom_line(aes(y = power, color = port))+
  theme_minimal()

ggsave(power, 
       filename = "434_diagnostics.png")

### Create a plot of all gaps in the data
times_by_port <- data_434_clean %>%
  distinct(port, time) %>%
  arrange(port, time)

times_by_port <- times_by_port %>%
  group_by(port) %>%
  mutate(
    prev_time = lag(time),
    gap_sec   = as.numeric(difftime(time, prev_time, units = "secs"))
  ) %>%
  ungroup()

gaps_by_port <- times_by_port %>%
  filter(!is.na(gap_sec) & gap_sec > gap_length) %>%
  mutate(
    gap_start = prev_time,
    gap_end   = time
  )

gaps <- ggplot() +
  # detections
  geom_point(data = times_by_port,
             aes(x = time, y = port),
             size = 4, color = "forestgreen", shape = "l") +
  
  # gaps
  geom_segment(data = gaps_by_port,
               aes(x = gap_start, xend = gap_end,
                   y = port, yend = port),
               color = "black", linewidth = 1) +
  
  labs(
    title = "Receiver Detections by Port",
    subtitle = paste("Black segments = gaps >",gap_length,"seconds"),
    x = "Time",
    y = "Port"
  ) +
  scale_y_discrete(expand = c(0, 1.7))+
  theme_minimal()+ 
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16))

ggsave(gaps, 
       filename = "434_gaps.png")

write.csv(data_434_clean, file = "78554c3358.csv", row.names = FALSE)

message("78554c3358 Test Transmitter Detected. Data and plots saved")

}else {message("No 434 Radios Detected")}

##### Diagnostic Messages #####
wd <- getwd()

message(paste("🟢 Data Processed and Diagnostic Plots Created Successfully! Data and plots saved to", wd))
message("Reviewing Diagnostics...")

if (exists(object)) {
port <- c("T1", "T3", "T5", "T6")

for (i in port) {
  tmp <- data_434_clean %>% 
    filter(port == i)

if (nrow(tmp) > 0) {
  message(paste("🟢 Port", i,"Receiving Test Transmitter Data"))
}else{message(paste("🔴 Port", i, " not Receiving Test Transmitter Data. Recommendation:
              1. Unplug and plug back in.
              2. Move Test Transmitter Closer
              3. Check Antennas"))}
}
}
port <- c("1","2","3","4","5","6")

for (i in port) {
  tmp <- all_data_166_long %>% 
    filter(port == i)
  
  if (nrow(tmp) > 0) {
    message(paste("🟢 Port", i, "Receiving Test Transmitter Data"))
  }else{message(paste("🔴 Port", i, " not Receiving Test Transmitter Data. Recommendation:
                      1. Unplug and plug back in.
                      2. Move Test Transmitter Closer
                      3. Check Antennas"))
    }
}

message("Use diagnostic plots to ensure data recorded matches expectations and that diagnostics are correct")

message("Creating Data Summaries...")

if (exists(object)) {
### Calculating mean signal strength for all 434 ports
power <- data_434_clean %>% 
    group_by(port) %>% 
    summarise(mean = mean(power))
  
port <- power$port
  
for (i in port) {
    pwr <- power %>% 
      filter(port == i)
    mean <- pwr$mean
    message(paste("
Port", i, "#########
Mean Signal Strength Equals", mean))
  }
}

### Calculating summary stats for all 166 ports
### Calculating mean signal strength for all 434 ports
summary <- all_data_166_long %>% 
  group_by(port) %>% 
  summarise(power = mean(power),
            noise = mean(noise),
            s2n = mean(S2N))

port <- summary$port

for (i in port) {
  pwr <- summary %>% 
    filter(port == i)
  mean <- pwr$power
  noise <- pwr$noise
  s2n <- pwr$s2n
  message(paste("
Port", i, "#########
Mean Signal Strength Equals", mean,
                "
Mean Noise Equals", noise,
                "
Mean S2N Equals", s2n))
}

rm(list = c("power", "pwr", "summary", "tmp", "i", "mean", "noise", "port", "s2n", "wd", "object", "gaps","gap_length", "gaps_by_port", "times_by_port"))