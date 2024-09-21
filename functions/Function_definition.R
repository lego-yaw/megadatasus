library(tidyverse)
library(dplyr)
library(RCurl)
library(foreign)
library(read.dbc)
library(rio)



Acess_Datasus <- function(Fonte, UF, ano_inicio, ano_final, mes_inicial, mes_final, clean = TRUE, delay = 2) {
  
  list_fonte <- c("SIASUS-AB", "SIASUS-ABO", "SIASUS-ACF", "SIASUS-AD", "SIASUS-AM", 
                  "SIASUS-AQ", "SIASUS-AN", "SIASUS-AR", 
                  "SIASUS-ATD", "SIASUS-PA", "SIASUS-PS", "SIASUS-SD", "SIHSUS-ER", 
                  "SIHSUS-RD", "SIHSUS-RJ", "SIHSUS-SP", "SISCOLO-CC", 
                  "SISCOLO-HC", "SISMAMA-CM", "SISMAMA-HC", "CIH", "CIHA", "IBGE-POP", "IBGE-POPT", 
                  "PO", "SIM-DO", "SIM-DOEXT", "SIM-DOFET", "SIM-DOINF", "SIM-DOMAT", "SIM-DOREXT",
                  "SINASC-DN", "SINASC-DNEX","RESP","SISPRENATAL","PCE","CNES-DC", "CNES-EE", "CNES-EF",
                  "CNES-EP", "CNES-EQ","CNES-GM", "CNES-HB","CNES-IN","CNES-LT", "CNES-PF", "CNES-RC",
                  "CNES-SR", "CNES-ST","SINAN-AIDC" ,"SINAN-ANIM" ,"SINAN-ANTR", "SINAN-BOTU", "SINAN-CANC" ,
                  "SINAN-CHAG", "SINAN-CHIK", "SINAN-COLE", "SINAN-COQU", "SINAN-DENG", "SINAN-DERM", "
                  SINAN-ESPO", "SINAN-ESQU", "SINAN-EXAN", "SINAN-FMAC", "SINAN-FTIF", "SINAN-HANS", 
                  "SINAN-HANT", "SINAN-HEPA", "SINAN-HIVA", "SINAN-HIVC", "SINAN-HIVE", "SINAN-HIVG", 
                  "SINAN-IEXO", "SINAN-INFL", "SINAN-LEPT", "SINAN-LERD", "SINAN-LTAN", "SINAN-MALA", "SINAN-MENI", 
                  "SINAN-MENT", "SINAN-NTRA", "SINAN-PAIR", "SINAN-PEST", "SINAN-PFAN", "SINAN-PNEU", "SINAN-RAIV", 
                  "SINAN-ROTA", "SINAN-SDTA", "SINAN-SIFA", "SINAN-SIFC", "SINAN-SIFG", "SINAN-SRC",
                  "SINAN-TETA", "SINAN-TETN", "SINAN-TOXC", "SINAN-TOXG", "SINAN-TRAC", "SINAN-TUBE",
                  "SINAN-VARC", "SINAN-VIOL", "SINAN-ZIKA","SINAN-AIDA","SINAN-ACGR","SINAN-ACBI")
  
  list_uf <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", 
               "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP","BR")
  
  months_numeric <- sprintf("%02d", 1:12)
  
  months_map <- c("Jan" = "01", "Fev" = "02", "Mar" = "03", "Abr" = "04", "Mai" = "05", 
                  "Jun" = "06", "Jul" = "07", "Ago" = "08", "Set" = "09", "Out" = "10", 
                  "Nov" = "11", "Dez" = "12")
  
  convert_month <- function(mes) {
    if (mes %in% months_numeric) {
      return(mes)
    } else if (mes %in% names(months_map)) {
      return(months_map[mes])
    } else {
      stop("Mês inválido. Verifique o formato.")
    }
  }
  
  # Convert mes_inicial and mes_final to default values if NULL
  if (is.null(mes_inicial)) {
    mes_inicial <- "01"
  } else {
    mes_inicial <- convert_month(mes_inicial)
  }
  
  if (is.null(mes_final)) {
    mes_final <- "12"
  } else {
    mes_final <- convert_month(mes_final)
  }
  
  # Convert mes_inicial and mes_final to numeric for use in seq function
  mes_inicio <- as.numeric(mes_inicial)
  mes_final <- as.numeric(mes_final)
  
  if(is.null(UF)){
    UF <- "BR"
  }else{
    UF <- UF
  }
  
  
  if (!(Fonte %in% list_fonte)) stop("Fonte inválida. Verifique a lista de fontes.")
  if (any(!(UF %in% list_uf))) stop("Uma ou mais UF inválidas. Verifique a lista de estados.")
  if (!is.numeric(ano_inicio) || !is.numeric(ano_final) || ano_inicio > ano_final) stop("Intervalo de ano inválido.")
  if (!is.logical(clean)) stop("Clean deve ser TRUE ou FALSE.")
  
  combined_data <- NULL
  
  if (Fonte == "SIASUS-AB") {
    string <- "AB"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# dONE
  else if (Fonte == "SIASUS-ACF") {
    # Código específico para SIASUS-ACF
    string <- "ACF"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    }#DONE
  else if (Fonte == "SIASUS-ABO") {
    # Código específico para SIASUS-ABO
    string <- "ABO"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
  }#Done
  else if (Fonte == "SIASUS-AD") {
    # Código específico para SIASUS-AD
    string <- "AD"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
  } #done
  else if (Fonte == "SIASUS-AM") {
    # Código específico para SIASUS-AM
    string <- "AM"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
  } #done
  else if (Fonte == "SIASUS-AQ") {
    # Código específico para SIASUS-AQ
    string <- "AQ"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
  } #done
  else if (Fonte == "SIASUS-AN") {
    # Código específico para SIASUS-AN
    string <- "AN"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }#done
  else if (Fonte == "SIASUS-AR") {
    # Código específico para SIASUS-AR
    string <- "AR"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } #done
  else if (Fonte == "SIASUS-ATD") {
    # Código específico para SIASUS-ATD
    string <- "ATD"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
  } #done
  else if (Fonte == "SIASUS-PA") {
    # Código específico para SIASUS-PA
    string <- "PA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
  } #done
  else if (Fonte == "SIASUS-PS") {
    # Código específico para SIASUS-PS
    string <- "PS"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } #done
  else if (Fonte == "SIASUS-SD") {
    # Código específico para SIASUS-SD
    string <- "SAD"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
  } #done
  else if (Fonte == "SIHSUS-ER") {
    # Código específico para SIHSUS-ER
    string <- "ER"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } #done
  else if (Fonte == "SIHSUS-RD") {
    
    string <- "RD"
    
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"
    ftp_string_2 <-  "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        file_names_sus_2 <- unlist(strsplit(getURL(ftp_string_2, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string_2, database_string_3)
          query_string_4 <- paste0(ftp_string_2, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          Sys.sleep(delay)  # Simulate delay before downloading
          
          # Check and download the correct file
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } # Done 
  else if (Fonte == "SIHSUS-RJ") {
    # Código específico para SIHSUS-RJ
    string <- "RJ"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
  }#Done
  else if (Fonte == "SIHSUS-SP") {
    
    string <- "SP"
    
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    ftp_string_2 <-  "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"
   
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        file_names_sus_2 <- unlist(strsplit(getURL(ftp_string_2, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string_2 , database_string_3)
          query_string_4 <- paste0(ftp_string_2 , database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          Sys.sleep(delay)  # Simulate delay before downloading
          
          # Check and download the correct file
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } # Done 
  else if (Fonte == "SISCOLO-CC") {
    # Código específico para SISCOLO-CC
    string <- "CC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISCOLO4/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } # Done
  else if (Fonte == "SISCOLO-HC") {
    # Código específico para SISCOLO-HC
    string <- "HC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISCOLO4/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } # Done
  else if (Fonte == "SISMAMA-CM") {
    # Código específico para SISMAMA-CM
    string <- "CM"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISMAMA/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } ################## apply correction to other functions 
  else if (Fonte == "SISMAMA-HC") {
    # Código específico para SISMAMA-HC
    string <- "HM"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISMAMA/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# done
  else if (Fonte == "CIH") {
    # Código específico para CIH
    string <- "CR"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CIH/200801_201012/Dados/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
  } #done
  else if (Fonte == "CIHA") {
    # Código específico para CIHA
    string <- "CIHA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CIHA/201101_/Dados/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } #done
  else if (Fonte == "IBGE-POP") {
    # Código específico para IBGE-POP
    string <- "POPBR"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POP/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } #done
  else if (Fonte == "IBGE-POPT") {
    # Código específico para IBGE-POPT
    string <- "POPTBR"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPTCU/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
  } #done
  else if (Fonte == "PO") {
    # Código específico para PO
    string <- "POBR"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/PAINEL_ONCOLOGIA/DADOS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } #done
  else if (Fonte == "SIM-DO")  {
    
    string <- "DOR"
    string_2 <- "DO"
    
    
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DORES/"
    ftp_string_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        file_names_sus_2 <- unlist(strsplit(getURL(string_2, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          
          database_string_1 <- paste0(string, uf, year_string_1, ".DBC")
          database_string_2 <- paste0(string, uf, year_string_2, ".DBC")
          database_string_3 <- paste0(string_2 ,uf, year_string_2, ".DBC")
          database_string_4 <- paste0(string_2 ,uf,year_string_1, ".DBC")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string_2, database_string_3)
          query_string_4 <- paste0(ftp_string_2, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          Sys.sleep(delay)  # Simulate delay before downloading
          
          # Check and download the correct file
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou",  database_string_3, "ou",  database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }  #Done 
  else if (Fonte == "SIM-DOEXT") {

    string <- "DOEXT"
    
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    ftp_string_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DOFET/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        file_names_sus_2 <- unlist(strsplit(getURL(ftp_string_2, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string_2, database_string_3)
          query_string_4 <- paste0(ftp_string_2, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          Sys.sleep(delay)  # Simulate delay before downloading
          
          # Check and download the correct file
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } #Done
  else if (Fonte == "SIM-DOFET") {
    
    string <- "DOFET"
    
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    ftp_string_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DOFET/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        file_names_sus_2 <- unlist(strsplit(getURL(ftp_string_2, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string_2, database_string_3)
          query_string_4 <- paste0(ftp_string_2, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          Sys.sleep(delay)  # Simulate delay before downloading
          
          # Check and download the correct file
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } #Done
  else if (Fonte == "SIM-DOINF") {
  
    string <- "DOINF"
    
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    ftp_string_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DOFET/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        file_names_sus_2 <- unlist(strsplit(getURL(ftp_string_2, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string_2, database_string_3)
          query_string_4 <- paste0(ftp_string_2, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          Sys.sleep(delay)  # Simulate delay before downloading
          
          # Check and download the correct file
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  } #Done
  else if (Fonte == "SIM-DOMAT") {
    # Código específico para SIM-DOMAT
    string <- "DOMAT"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
    
  } #Done
  else if (Fonte == "SIM-DOREXT") {
    # Código específico para SIM-DOREXT
    string <- "DOREXT"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
    
  } #done
  else if (Fonte == "SINASC-DN") {
    # Código específico para SINASC-DN
    string <- "DN"
    string_2 <- "DNR"
    
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"
    
    ftp_string_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1994_1995/Dados/DNRES/"
    
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        file_names_sus_2 <- unlist(strsplit(getURL( ftp_string_2, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        file_names_sus_2
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string_2, database_string_3)
          query_string_4 <- paste0(ftp_string_2, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          
          }else if (any(database_string_3 %in% file_names_sus_2)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
            
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data)>0){
              all_data[[length(all_data) + 1]] <- data}}
          
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "SINASC-DNEX") {
   
    string <- "DNEX"
    string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# done
  else if (Fonte == "RESP") {
    # Código específico para SINASC-DNEX
    string <- "RESP"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/RESP/DADOS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done ## use thsi to correct the structure of the strings
  else if (Fonte == "SISPRENATAL") {
    
    string <- "PN"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SISPRENATAL/201201_/Dados/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == " PCE") {
    # Código específico para SINASC-DNEX
    string <- "PCE"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/PCE/DADOS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-DC") {
    # Código específico para SINASC-DNEX
    string <- "DC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/DC/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-EE") {
    # Código específico para SINASC-DNEX
    string <- "EE"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EE/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-EF") {
    # Código específico para SINASC-DNEX
    string <- "EF"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EF/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-EP") {
    # Código específico para SINASC-DNEX
    string <- "EP"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EP/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-EQ") {
    # Código específico para SINASC-DNEX
    string <- "EQ"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EQ/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-GM") {
    # Código específico para SINASC-DNEX
    string <- "GM"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/GM/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-HB") {
    # Código específico para SINASC-DNEX
    string <- "HB"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/HB/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-IN") {
    # Código específico para SINASC-DNEX
    string <- "IN"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/IN/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-LT") {
    # Código específico para SINASC-DNEX
    string <- "LT"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/LT/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-PF") {
    # Código específico para SINASC-DNEX
    string <- "PF"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-RC") {
    # Código específico para SINASC-DNEX
    string <- "RC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/RC/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-SR") {
    # Código específico para SINASC-DNEX
    string <- "SR"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/SR/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "CNES-ST") {
    # Código específico para SINASC-DNEX
    string <- "SR"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "SINAN-ACBI") {
    # Código específico para SINASC-DNEX
    string <- "ACBI"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# dONE
  else if (Fonte == "SINAN-ACGR") {
    # Código específico para SINASC-DNEX
    string <- "ACGR"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# dONE
  else if (Fonte == "SINAN-AIDA") {
    # Código específico para SINASC-DNEX
    string <- "AIDA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# Done
  else if (Fonte == "SINAN-AIDC") {
    # Código específico para SINASC-DNEX
    string <- "AIDC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }# done
  else if (Fonte == "SINAN-ANIM") {
    # Código específico para SINASC-DNEX
    string <- "ANIM"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-ANTR") {
    # Código específico para SINASC-DNEX
    string <- "ANTR"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-BOTU") {
    # Código específico para SINASC-DNEX
    string <- "BOTU"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-CANC") {
    # Código específico para SINASC-DNEX
    string <- "CANC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-CHAG") {
    # Código específico para SINASC-DNEX
    string <- "CHAG"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-CHIK") {
    # Código específico para SINASC-DNEX
    string <- "CHIK"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-COLE") {
    # Código específico para SINASC-DNEX
    string <- "COLE"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-COQU") {
    # Código específico para SINASC-DNEX
    string <- "COQU"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-DENG") {
    # Código específico para SINASC-DNEX
    string <- "DENG"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-DERM") {
    # Código específico para SINASC-DNEX
    string <- "DERM"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-ESPO") {
    # Código específico para SINASC-DNEX
    string <- "ESPO"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-ESQU") {
    # Código específico para SINASC-DNEX
    string <- "ESQU"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-EXAN") {
    # Código específico para SINASC-DNEX
    string <- "EXAN"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-FMAC") {
    # Código específico para SINASC-DNEX
    string <- "FMAC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-FTIF") {
    # Código específico para SINASC-DNEX
    string <- "FTIF"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-HANS") {
    # Código específico para SINASC-DNEX
    string <- "HANS"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-HANT") {
    # Código específico para SINASC-DNEX
    string <- "HANT"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-HEPA") {
    # Código específico para SINASC-DNEX
    string <- "HEPA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-HIVA") {
    # Código específico para SINASC-DNEX
    string <- "HIVA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-HIVC") {
    # Código específico para SINASC-DNEX
    string <- "HIVC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-HIVE") {
    # Código específico para SINASC-DNEX
    string <- "HIVE"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-HIVG") {
    # Código específico para SINASC-DNEX
    string <- "HIVG"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-IEXO") {
    # Código específico para SINASC-DNEX
    string <- "IEXO"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-INFL") {
    # Código específico para SINASC-DNEX
    string <- "INFL"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-LEPT") {
    # Código específico para SINASC-DNEX
    string <- "LEPT"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-LERD") {
    # Código específico para SINASC-DNEX
    string <- "LERD"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-LTAN") {
    # Código específico para SINASC-DNEX
    string <- "LTAN"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-MALA") {
    # Código específico para SINASC-DNEX
    string <- "MALA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-MENI") {
    # Código específico para SINASC-DNEX
    string <- "MENI"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-MENT") {
    # Código específico para SINASC-DNEX
    string <- "MENT"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-NTRA") {
    # Código específico para SINASC-DNEX
    string <- "NTRA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-PAIR") {
    # Código específico para SINASC-DNEX
    string <- "PAIR"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-PEST") {
    # Código específico para SINASC-DNEX
    string <- "PEST"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-PFAN") {
    # Código específico para SINASC-DNEX
    string <- "PFAN"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-PNEU") {
    # Código específico para SINASC-DNEX
    string <- "PNEU"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-RAIV") {
    # Código específico para SINASC-DNEX
    string <- "RAIV"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-ROTA") {
    # Código específico para SINASC-DNEX
    string <- "ROTA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-SDTA") {
    # Código específico para SINASC-DNEX
    string <- "SDTA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-SIFA") {
    # Código específico para SINASC-DNEX
    string <- "SIFA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-SIFC") {
    # Código específico para SINASC-DNEX
    string <- "SIFC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-SIFG") {
    # Código específico para SINASC-DNEX
    string <- "SIFG"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-SRC") {
    # Código específico para SINASC-DNEX
    string <- "SRC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-TETA") {
    # Código específico para SINASC-DNEX
    string <- "TETA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-TETN") {
    # Código específico para SINASC-DNEX
    string <- "TETN"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-TOXC") {
    # Código específico para SINASC-DNEX
    string <- "TOXC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-TOXG") {
    # Código específico para SINASC-DNEX
    string <- "TOXG"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-TRAC") {
    # Código específico para SINASC-DNEX
    string <- "TRAC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-TUBE") {
    # Código específico para SINASC-DNEX
    string <- "TUBE"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-VARC") {
    # Código específico para SINASC-DNEX
    string <- "VARC"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-VIOL") {
    # Código específico para SINASC-DNEX
    string <- "VIOL"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  else if (Fonte == "SINAN-ZIKA") {
    # Código específico para SINASC-DNEX
    string <- "ZIKA"
    ftp_string <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    
    for (uf in UF) {
      for (year in seq(ano_inicio, ano_final)) {
        year_string_1 <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
        year_string_2 <- as.character(year)
        
        file_names_sus <- unlist(strsplit(getURL(ftp_string, ftp.use.epsv = FALSE, dirlistonly = TRUE), "\r\n"))
        
        for (m in seq(as.numeric(mes_inicio), as.numeric(mes_final))) {
          mes_string <- sprintf("%02d", m)
          database_string_1 <- paste0(string, uf, year_string_1,mes_string, ".dbc")
          database_string_2 <- paste0(string, uf, year_string_2, ".dbc")
          database_string_3 <- paste0(string,year_string_2, ".dbc")
          database_string_4 <- paste0(string,uf, year_string_1, ".dbc")
          
          query_string_1 <- paste0(ftp_string, database_string_1)
          query_string_2 <- paste0(ftp_string, database_string_2)
          query_string_3 <- paste0(ftp_string, database_string_3)
          query_string_4 <- paste0(ftp_string, database_string_4)
          
          file_path_1 <- file.path(tempdir(), database_string_1)
          file_path_2 <- file.path(tempdir(), database_string_2)
          file_path_3 <- file.path(tempdir(), database_string_3)
          file_path_4 <- file.path(tempdir(), database_string_4)
          
          # Simulate delay before downloading
          Sys.sleep(delay)
          
          # Check if the file exists in the list of files on the FTP server
          if (any(database_string_1 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_1, file_path_1, mode = "wb")
              print(paste("Downloaded:", database_string_1))
            }, error = function(e) {
              print(paste("Error downloading", database_string_1, ":", e$message))
            })
          } else if (any(database_string_2 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_2, file_path_2, mode = "wb")
              print(paste("Downloaded:", database_string_2))
            }, error = function(e) {
              print(paste("Error downloading", database_string_2, ":", e$message))
            })
          } else if (any(database_string_3 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_3, file_path_3, mode = "wb")
              print(paste("Downloaded:", database_string_3))
            }, error = function(e) {
              print(paste("Error downloading", database_string_3, ":", e$message))
            })
          } else if (any(database_string_4 %in% file_names_sus)) {
            tryCatch({
              download.file(query_string_4, file_path_4, mode = "wb")
              print(paste("Downloaded:", database_string_4))
            }, error = function(e) {
              print(paste("Error downloading", database_string_4, ":", e$message))
            })
          } else {
            print(paste("Arquivo não encontrado para:", database_string_1, "ou", database_string_2, "ou", database_string_3, "ou", database_string_4))
            next
          }
          
          # Combine the downloaded files
          dbc_files <- list.files(path = tempdir(), pattern = "\\.dbc$", full.names = TRUE)
          all_data <- list()  
          for (file in dbc_files) {
            data <- read.dbc(file)
            if (nrow(data) > 0) {
              all_data[[length(all_data) + 1]] <- data
            }
          }
        }
      }
    }
    
    # Standardize columns across all data frames
    if (length(all_data) > 0) {
      all_columns <- unique(unlist(lapply(all_data, colnames)))
      all_data_aligned <- lapply(all_data, function(df) {
        missing_cols <- setdiff(all_columns, colnames(df))
        df[missing_cols] <- NA
        return(df[, all_columns])
      })
      combined_data <- do.call(rbind, all_data_aligned)
    }
  }
  #########################################################################################################
  # Clean the tempfile
  if (clean && !is.null(combined_data)) {
    unlink(file.path(tempdir(), "*.dbc"))
  }
  
  return(combined_data)
} # function for downlaoding data 









# Example usage:
combined_data <- Acess_Datasus(Fonte = "CIH", 
                               UF = c("AC"), 
                               ano_inicio = 2011, 
                               ano_final = 2011, 
                               mes_inicial = "01", 
                               mes_final = "02", 
                               clean = TRUE,
                               delay = 0.01)  # Adding a delay of 3 seconds # Adding a delay of 3 seconds





view(combined_data)
colnames(combined_data)


export(combined_data, "CIH.csv")



