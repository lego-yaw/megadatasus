library(tidyverse)
library(dplyr)
library(RCurl)
library(foreign)
library(read.dbc)

summary_datasus <- function(Fonte){
  
  list_fonte <- c("SIASUS-AB", "SIASUS-ABO", "SIASUS-ACF", "SIASUS-AD", "SIASUS-AM", 
                  "SIASUS-AQ", "SIASUS-AN", "SIASUS-AR", 
                  "SIASUS-ATD", "SIASUS-PA", "SIASUS-PS", "SIASUS-SD", "SIHSUS-ER", 
                  "SIHSUS-RD", "SIHSUS-RJ", "SIHSUS-SP", "SISCOLO-CC", 
                  "SISCOLO-HC", "SISMAMA-CM", "SISMAMA-HC", "CIH", "CIHA", "IBGE-POP", "IBGE-POPT", 
                  "PO", "SIM-DO", "SIM-DOEXT", "SIM-DOFET", "SIM-DOINF", "SIM-DOMAT", "SIM-DOREXT",
                  "SINASC-DN", "SINASC-DNEX")
  
  ## Liniting search to the available Fonte
  if (!(Fonte %in% list_fonte)) stop("Fonte inválida. Verifique a lista de fontes.")
  

if (Fonte == "SIASUS-AB") {
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  
  
  # Get the list of files from the FTP directory
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^AB[^O]", file_names_sus, value = TRUE)
  ab_files
  length(ab_files)
  
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIASUS-AB.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} 
else if (Fonte == "SIASUS-ABO") {
  # Código específico para SIASUS-ABO
  fonte_string <- "ABO"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^ABO", file_names_sus, value = TRUE)
  
  ab_files
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
    
    for (files in ab_files) {
      uf <- substr(files, 4, 5)   # Extract UF
      years <- substr(files, 6, 7)  # Extract years (last two digits of the year)
      month <- substr(files, 8, 9)  # Extract month in "01", "02", etc.
      
      # Append the extracted values to the lists
      # Determine if the year belongs to the 1900s or 2000s
      full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
      
      # Append the extracted values to the lists
      uf_list <- c(uf_list, uf)
      years_list <- c(years_list, as.numeric(full_year))
      month_list <- c(month_list, month)  # Keep month in "01", "02" format
    }
    
    # Create a data frame from the extracted information
    file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
    
    # Group by UF and create a range for Year and Month
    grouped_info <- file_info_df %>%
      group_by(UF) %>%
      summarise(
        Year_Range = paste0(min(Year), "-", max(Year)),
        Month_Range = paste0(min(Month), "-", max(Month))
      )
    
    
    # Create a directory named "log_files" in the working directory if it doesn't exist
    dir.create("log_files", showWarnings = FALSE)
    
    # Specify the log file path within the new directory
    log_file_path <- file.path("log_files", "SIASUS-ABO.txt")
    
    # Write the grouped data frame to the log file in the "log_files" directory
    write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
    
    # Display a message indicating that the log file has been written
    cat("Grouped file info has been written to:", log_file_path, "\n")
  
}#
else if (Fonte == "SIASUS-ACF") {
  # Código específico para SIASUS-ACF
  fonte_string <- "ACF"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^ACF", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 4, 5)   # Extract UF
    years <- substr(files, 6, 7)  # Extract years (last two digits of the year)
    month <- substr(files, 8, 9)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIASUS-ACF.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} #
else if (Fonte == "SIASUS-AD") {
  # Código específico para SIASUS-AD
  fonte_string <- "AD"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^AD", file_names_sus, value = TRUE)
  length(ab_files) # 5344
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIASUS-AD.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} # Done
else if (Fonte == "SIASUS-AM") {
  # Código específico para SIASUS-AM
  fonte_string <- "AM"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^AM", file_names_sus, value = TRUE)
  ab_files
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIASUS-AM.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} # Done
else if (Fonte == "SIASUS-AQ") {
  # Código específico para SIASUS-AQ
  fonte_string <- "AQ"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^AQ", file_names_sus, value = TRUE)
  length(ab_files) 
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIASUS-AQ.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
}# Done 
else if (Fonte == "SIASUS-AN") {
  # Código específico para SIASUS-AN
  fonte_string <- "AN"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^AN", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIASUS-AN.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} # Done
else if (Fonte == "SIASUS-AR") {
  # Código específico para SIASUS-AR
  fonte_string <- "AR"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^AR", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIASUS-AR.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} # Done
else if (Fonte == "SIASUS-ATD") {
  # Código específico para SIASUS-ATD
  fonte_string <- "ATD"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^ATD", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 4, 5)   # Extract UF
    years <- substr(files, 6, 7)  # Extract years (last two digits of the year)
    month <- substr(files, 8, 9)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIASUS-ATD.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} # Done
else if (Fonte == "SIASUS-PA") {
  # Código específico para SIASUS-PA
  fonte_string <- "PA"
  

  # link 1
  ftp_String_1 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  ftp_file_list_1 <- getURL(ftp_String_1, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_1 <- unlist(strsplit(ftp_file_list_1, "\r\n"))
  
  # link 2
  ftp_String_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
  ftp_file_list_2 <- getURL(ftp_String_2, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_2 <- unlist(strsplit(ftp_file_list_2, "\r\n"))
  
  # Select only files that start with "AB" but not "ABO"
  ab_files_1 <- grep("^PA", file_names_sus_1, value = TRUE)
  ab_files_2 <- grep("^PA", file_names_sus_2, value = TRUE)
  ab_files <- c(ab_files_1,ab_files_2)
  
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIASUS-PA.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
}# Done -two links 
else if (Fonte == "SIASUS-PS") {
  # Código específico para SIASUS-PS
  fonte_string <- "PS"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^PS", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIASUS-PS.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} # Done
else if (Fonte == "SIASUS-SD") {
  # Código específico para SIASUS-SD
  fonte_string <- "SAD"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^SAD", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 4, 5)   # Extract UF
    years <- substr(files, 6, 7)  # Extract years (last two digits of the year)
    month <- substr(files, 8, 9)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIASUS-SAD.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
  
} # Done
else if (Fonte == "SIHSUS-ER") {
  # Código específico para SIHSUS-ER
  fonte_string <- "ER"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^ER", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIHSUS-ER.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} #Done
else if (Fonte == "SIHSUS-RD") {
  # Código específico para SIHSUS-RD
  fonte_string <- "RD"
  
  
  # Length 1
  ftp_String_1 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"
  
  ftp_file_list_1 <- getURL(ftp_String_1, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_1 <- unlist(strsplit(ftp_file_list_1, "\r\n"))

  
  # Length 2
  ftp_String_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
  
  ftp_file_list_2 <- getURL(ftp_String_2, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_2 <- unlist(strsplit(ftp_file_list_2, "\r\n"))
  
  # Select only files that start with "AB" but not "ABO"
  ab_files_1 <- grep("^RD", file_names_sus_1, value = TRUE)
  ab_files_2 <- grep("^RD", file_names_sus_2, value = TRUE)
  
  ab_files <- c(ab_files_1,ab_files_2)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIHSUS-RD.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
}# Done - two links 
else if (Fonte == "SIHSUS-RJ") {
  # Código específico para SIHSUS-RJ
  fonte_string <- "RJ"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^RJ", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIHSUS-RJ.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
} #Done
else if (Fonte == "SIHSUS-SP") {
  # Código específico para SIHSUS-SP
  fonte_string <- "SP"
  
  # Link 1
  ftp_String_1 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"
  ftp_file_list_1 <- getURL(ftp_String_1, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_1 <- unlist(strsplit(ftp_file_list_1, "\r\n"))
  
  # Link 1
  ftp_String_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
  ftp_file_list_2 <- getURL(ftp_String_2, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_2 <- unlist(strsplit(ftp_file_list_2, "\r\n"))

  
  # Select only files that start with "AB" but not "ABO"
  ab_files_1 <- grep("^SP", file_names_sus_1, value = TRUE)
  ab_files_2 <- grep("^SP", file_names_sus_2, value = TRUE)
  
  ab_files <- c(ab_files_1,ab_files_2)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIHSUS-SP.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} # Done  - two links
else if (Fonte == "SISCOLO-CC") {
  # Código específico para SISCOLO-CC
  fonte_string <- "CC"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISCOLO4/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^CC", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SISCOLO-CC.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
}# Done
else if (Fonte == "SISCOLO-HC") {
  # Código específico para SISCOLO-HC
  fonte_string <- "HC"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISCOLO4/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^HC", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SISCOLO-HC.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} # Done
else if (Fonte == "SISMAMA-CM") {
  # Código específico para SISMAMA-CM
  fonte_string <- "CM"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISMAMA/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))

  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^CM", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SISMAMA-CM.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
  
} # Done
else if (Fonte == "SISMAMA-HC") {
  # Código específico para SISMAMA-HC
  fonte_string <- "HM"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISMAMA/Dados/"
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^HM", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SISMAMA-HC.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
  
} # Done
else if (Fonte == "CIH") {
  # Código específico para CIH
  fonte_string <- "CR"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CIH/200801_201012/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^CR", file_names_sus, value = TRUE)
  
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 6)  # Extract years (last two digits of the year)
    month <- substr(files, 7, 8)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "CIH.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
  
} # Done
else if (Fonte == "CIHA") {
  # Código específico para CIHA
  fonte_string <- "CIHA"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CIHA/201101_/Dados/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))

  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^CIHA", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 5, 6)   # Extract UF
    years <- substr(files, 7, 8)  # Extract years (last two digits of the year)
    month <- substr(files, 9, 10)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "CIHA.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
  
} #Done
else if (Fonte == "IBGE-POP") {
  # Código específico para IBGE-POP
  fonte_string <- "POPBR"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POP/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^POPBR", file_names_sus, value = TRUE)
  length(ab_files) 
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()

  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- "BR"   # Extract UF
    years <- substr(files, 6, 7)  # Extract years (last two digits of the year)
    
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))

  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list,  stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)) )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "IBGE-POP.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
  
} # Done
else if (Fonte == "IBGE-POPT") {
  # Código específico para IBGE-POPT
  fonte_string <- "POPTBR"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPTCU/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  file_names_sus
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^POPTBR", file_names_sus, value = TRUE)
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()

  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- "BR" #  UF
    years <- substr(files, 7, 8)  # Extract years (last two digits of the year)  
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))

  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list,  stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "IBGE-POPT.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
  
  
}# DONE
else if (Fonte == "PO") {
  # Código específico para PO
  fonte_string <- "POBR"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/PAINEL_ONCOLOGIA/DADOS/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))

  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^POBR", file_names_sus, value = TRUE)
  
  length(ab_files) 
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- "BR"   # E UF
    years <- substr(files, 5, 8)  # Extract years (last two digits of the year)
   
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(years))
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "PO.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} # Done
else if (Fonte == "SIM-DO") {
  # Código específico para SIM-DO
  fonte_string <- "DOR e DO"
  
  
  # LINK 1
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DORES/"
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  
  
  # LINK 2
  ftp_String_1 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"
  ftp_file_list_1 <- getURL(ftp_String_1, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_1 <- unlist(strsplit(ftp_file_list_1, "\r\n"))

  
  # Select only files that start with "AB" but not "ABO"
  ab_files_1 <- grep("^DOR", file_names_sus, value = TRUE)
  ab_files_2 <- grep("^DO", file_names_sus_1, value = TRUE)

 
  # Initialize empty lists to store UF, years, and months
  uf_list_1 <- c()
  years_list_1 <- c()
  uf_list_2 <- c()
  years_list_2 <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files_1) {
    uf <- substr(files, 4, 5)   # Extract UF
    years <- substr(files, 6, 7)  # Extract years (last two digits of the year)
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list_1<- c(uf_list_1, uf)
    years_list_1 <- c(years_list_1, as.numeric(full_year))
    
  }
  
  
  for (files in ab_files_2) {
    uf <- substr(files, 3, 4)   # Extract UF
    years <- substr(files, 5, 8)  # Extract years (last two digits of the year)
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    # Append the extracted values to the lists
    uf_list_2<- c(uf_list_2, uf)
    years_list_2 <- c(years_list_2, as.numeric(years))
  }
  
  
  uf_list <- c(uf_list_1,uf_list_2)
  years_list <- c(years_list_1,years_list_2)
  
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIM-DOR.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} # Done
else if (Fonte == "SIM-DOEXT") {
  # Código específico para SIM-DOEXT
  fonte_string <- "DOEXT"
  
  
  
  #link 1
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DOFET/"
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  
  
  
  #link 2
  ftp_String_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
  ftp_file_list_2 <- getURL(ftp_String_2, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_2 <- unlist(strsplit(ftp_file_list_2, "\r\n"))
 
  
  
  
  
  # Select only files that start with "AB" but not "ABO"
  ab_files_1 <- grep("^DOEXT", file_names_sus, value = TRUE)
  ab_files_2 <- grep("^DOEXT", file_names_sus_2, value = TRUE)
  ab_files <- c(ab_files_1,ab_files_2)
  
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- "BR"  # NO uf only BR,
    years <- substr(files, 6, 7)  # Extract years (last two digits of the year)
    # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIM-DOEXT.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
} # Done
else if (Fonte == "SIM-DOFET") {
  # Código específico para SIM-DOFET
  fonte_string <- "DOFET"
  
  
  
  #link 1
  ftp_String <-  "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DOFET/"
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))

  # link 2
  
  ftp_String_2 <-  "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
  ftp_file_list_2 <- getURL(ftp_String_2, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_2 <- unlist(strsplit(ftp_file_list_2, "\r\n"))
  
  # Select only files that start with "AB" but not "ABO"
  ab_files_1 <- grep("^DOFET", file_names_sus, value = TRUE)
  ab_files_2 <- grep("^DOFET", file_names_sus_2, value = TRUE)
 
  ab_files <- c(ab_files_1,ab_files_2)
  
  length(ab_files)
  # Initialize empty lists to store UF, years, and months
  uf_list <- c()
  years_list <- c()
  month_list <- c()
  
  # Loop through the selected files and extract UF, years, and months
  for (files in ab_files) {
    uf <- substr(files, 6, 7)   # Extract UF
    years <- substr(files, 8, 9)  # Extract years (last two digits of the year)
    month <- substr(files, 10, 11)  # Extract month in "01", "02", etc.
    
    # Append the extracted values to the lists
    # Determine if the year belongs to the 1900s or 2000s
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    month_list <- c(month_list, month)  # Keep month in "01", "02" format
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, Month = month_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year and Month
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year)),
      Month_Range = paste0(min(Month), "-", max(Month))
    )
  
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIM-DOFET.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} #Done
else if (Fonte == "SIM-DOINF") {
  # Código específico para SIM-DOINF
  fonte_string <- "DOINF"
 


  
  # link 1
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DOFET/"
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  
  # link 2
  ftp_String_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
  ftp_file_list_2 <- getURL(ftp_String_2, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_2 <- unlist(strsplit(ftp_file_list_2, "\r\n"))
  
  
  ab_files_1 <- grep("^DOINF", file_names_sus, value = TRUE, perl = TRUE)
  ab_files_2 <- grep("^DOINF", file_names_sus_2, value = TRUE, perl = TRUE)
  
  ab_files <- c(ab_files_1,ab_files_2)
  
  length(ab_files)
  # Initialize empty lists to store UF and years
  uf_list <- c()
  years_list <- c()

  
  # Loop through the selected files and extract UF and years
  for (files in ab_files) {
    uf <- "BR" 
    years <- substr(files, 6, 7)  # Extract years (5th to 8th characters)
    
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year))
    )
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIM-DOINF.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
} # Done
else if (Fonte == "SIM-DOMAT") {
  # Código específico para SIM-DOMAT
  fonte_string <- "DOMAT"
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
  
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  
  ab_files <- grep("^DOMAT(?!O)", file_names_sus, value = TRUE, perl = TRUE)
  ab_files
  # Initialize empty lists to store UF and years
  uf_list <- c()
  years_list <- c()
  
  # Loop through the selected files and extract UF and years
  for (files in ab_files) {
    uf <- "BR" 
    years <- substr(files, 6, 7)  # Extract years (5th to 8th characters)
    
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year))
    )
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIM-DOMAT.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
  
  
} #Done
else if (Fonte == "SIM-DOREXT") {
  # Código específico para SIM-DOREXT
  fonte_string <- "DOREXT"
  
  
  #for string 1
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus<- unlist(strsplit(ftp_file_list, "\r\n"))
  
  

  ab_files <- grep("^DOREXT(?!O)", file_names_sus, value = TRUE, perl = TRUE)
  
  # Initialize empty lists to store UF and years
  uf_list <- c()
  years_list<- c()
 
  
  # Loop through the selected files and extract UF and years
  for (files in ab_files) {
    uf <- "BR" 
    years <- substr(files, 7, 8)  # Extract years (5th to 8th characters)
    
    full_year <- ifelse(as.numeric(years) >= 78, paste0("19", years), paste0("20", years))
    
    # Append the extracted values to the lists
    uf_list<- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(full_year))
    
  }
  
  

  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year))
    )
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SIM-DOREXT.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
} #Done
else if (Fonte == "SINASC-DN") {
  # Código específico para SINASC-DN
  fonte_string <- "DN"
  
  # FOR STRING 1
  ftp_String_1 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"
  ftp_file_list_1 <- getURL(ftp_String_1, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_1 <- unlist(strsplit(ftp_file_list_1, "\r\n"))
  
  #FOR STRING 2
  ftp_file_list_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1994_1995/Dados/DNRES/"
  
  ftp_file_list_2 <- getURL( ftp_file_list_2, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus_2 <- unlist(strsplit(ftp_file_list_2, "\r\n"))
  
  # Select only files that start with "AB" but not "ABO"
  ab_files_1 <- grep("^DN(?!O)", file_names_sus_1, value = TRUE, perl = TRUE)
  ab_files_1
  ab_files_2 <- grep("^DNR(?!O)", file_names_sus_2, value = TRUE, perl = TRUE)
  
  ab_files_2
 
  ab_files_1
 length(ab_files_1)
  length(ab_files_2)
  
  length( c(ab_files_1, ab_files_2)) ## check length to match what is in the database
  # Initialize empty lists to store UF and years
  uf_list_1 <- c()
  years_list_1 <- c()
  
  uf_list_2 <- c()
  years_list_2 <- c()
  
  #for link 1
  for (files in ab_files_1) {
    uf <- substr(files, 3, 4)  # Extract UF (3rd and 4th characters)
    years <- substr(files, 5, 8)  # Extract years (5th to 8th characters)
    
    
    # Append the extracted values to the lists
    uf_list_1 <- c(uf_list_1, uf)
    years_list_1 <- c(years_list_1, as.numeric(years))
    
  }
  
  
  ## for link 2
  for (files in ab_files_2) {
    uf <- substr(files, 4,5)  # Extract UF (3rd and 4th characters)
    years <- substr(files, 6, 9)  # Extract years (5th to 8th characters)
    
  
    
    # Append the extracted values to the lists
    uf_list_2 <- c(uf_list_2, uf)
    years_list_2 <- c(years_list_2, as.numeric(years))
    
  }
  

  
  uf_list <- c(uf_list_1, uf_list_2)
  years_list <- c(years_list_1, years_list_2)
  
  
  
  
  file_info_df <- data.frame(UF = uf_list, Year = years_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year))
    )
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SINASC-DN.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
  
} # DONE
else if (Fonte == "SINASC-DNEX") {
  # Código específico para SINASC-DNEX
  fonte_string <- "DNEX"
  
  ftp_String <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"
  ftp_file_list <- getURL(ftp_String, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  file_names_sus <- unlist(strsplit(ftp_file_list, "\r\n"))
  
  # Select only files that start with "AB" but not "ABO"
  ab_files <- grep("^DNEX(?!O)", file_names_sus, value = TRUE, perl = TRUE)
 ab_files
  # Initialize empty lists to store UF and years
  uf_list <- c()
  years_list <- c()
  
  # Loop through the selected files and extract UF and years
  for (files in ab_files) {
    uf <- substr(files, 3, 4)  # Extract UF (3rd and 4th characters)
    years <- substr(files, 5, 8)  # Extract years (5th to 8th characters)
    
    # Append the extracted values to the lists
    uf_list <- c(uf_list, uf)
    years_list <- c(years_list, as.numeric(years))
  }
  
  # Create a data frame from the extracted information
  file_info_df <- data.frame(UF = uf_list, Year = years_list, stringsAsFactors = FALSE)
  
  # Group by UF and create a range for Year
  grouped_info <- file_info_df %>%
    group_by(UF) %>%
    summarise(
      Year_Range = paste0(min(Year), "-", max(Year))
    )
  
  # Create a directory named "log_files" in the working directory if it doesn't exist
  dir.create("log_files", showWarnings = FALSE)
  
  # Specify the log file path within the new directory
  log_file_path <- file.path("log_files", "SINASC-DNEX.txt")
  
  # Write the grouped data frame to the log file in the "log_files" directory
  write.table(grouped_info, file = log_file_path, row.names = FALSE, sep = "\t", quote = FALSE)
  
  # Display a message indicating that the log file has been written
  cat("Grouped file info has been written to:", log_file_path, "\n")
} # DONE
else {
  stop("Fonte não reconhecida.")
}}



summary_datasus(Fonte = "SIASUS-ABO")
