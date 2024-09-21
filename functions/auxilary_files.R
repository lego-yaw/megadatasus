library(dplyr)
library(RCurl)


auxi_files <- function(Fonte) {
  
  if (Fonte == "IBGE") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/IBGE/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    file_name <- "TAB_POP.zip"
    path_string <- paste0(dest_dir, file_name) 
    extract_path <- dest_dir  
    
    download.file("ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/AUXILIAR/TAB_POP.zip", 
                  path_string, mode = "wb")
    
    unzip(zipfile = path_string, exdir = extract_path, overwrite = TRUE)
    
    file.remove(path_string)
  }
  else if (Fonte == "CIH") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/CIH/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    file_name <- "TAB_CIH.zip"
    path_string <- paste0(dest_dir, file_name) 
    extract_path <- dest_dir  
    
    download.file("ftp://ftp.datasus.gov.br/dissemin/publicos/CIH/200801_201012/Auxiliar/TAB_CIH.zip", 
                  path_string, mode = "wb")
    
    unzip(zipfile = path_string, exdir = extract_path, overwrite = TRUE)
    
    file.remove(path_string)
  }
  else if (Fonte == "CIHA") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/CIHA/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    file_name <- "TAB_CIHA.zip"
    path_string <- paste0(dest_dir, file_name) 
    extract_path <- dest_dir  
    
    download.file("ftp://ftp.datasus.gov.br/dissemin/publicos/CIHA/201101_/Auxiliar/TAB_CIHA.zip", 
                  path_string, mode = "wb")
    
    unzip(zipfile = path_string, exdir = extract_path, overwrite = TRUE)
    
    file.remove(path_string)
  }
  else if (Fonte == "PCE") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/PCE/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    file_name <- "tab_pce.zip"
    path_string <- paste0(dest_dir, file_name) 
    extract_path <- dest_dir  
    
    download.file("ftp://ftp.datasus.gov.br/dissemin/publicos/PCE/AUXILIAR/tab_pce.zip", 
                  path_string, mode = "wb")
    
    unzip(zipfile = path_string, exdir = extract_path, overwrite = TRUE)
    
    file.remove(path_string)
  }
  else if (Fonte == "PO") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/PO/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    file_name <- "PO.zip"
    path_string <- paste0(dest_dir, file_name) 
    extract_path <- dest_dir  
    
    download.file("ftp://ftp.datasus.gov.br/dissemin/publicos/PAINEL_ONCOLOGIA/AUXILIAR/PAINEL_ONCOLOGIA.zip", 
                  path_string, mode = "wb")
    
    unzip(zipfile = path_string, exdir = extract_path, overwrite = TRUE)
    
    file.remove(path_string)
  }
  else if (Fonte == "RESP") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/RESP/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    file_name <- "RESP.zip"
    path_string <- paste0(dest_dir, file_name) 
    extract_path <- dest_dir  
    
    download.file("ftp://ftp.datasus.gov.br/dissemin/publicos/RESP/AUXILIAR/tabresp.zip", 
                  path_string, mode = "wb")
    
    unzip(zipfile = path_string, exdir = extract_path, overwrite = TRUE)
    
    file.remove(path_string)
  }
  else if (Fonte == "SIASUS") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/SIA/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    link_1 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Auxiliar/TAB_SIA.zip"
    link_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Auxiliar/TAB_SIA_199407-199910.zip"
    link_3 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Auxiliar/TAB_SIA_199911-200307.zip"
    link_4 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Auxiliar/TAB_SIA_200308-200712.zip"
    
    list_links <- c(link_1,link_2,link_3, link_4)
    
    for (i in seq_along(list_links)){
      
      file_name <- paste0("SIA",i,".zip")
      path_string <- paste0(dest_dir,file_name)
      
      download.file(list_links[i], path_string, mode = "wb")
      }
    
  }
  else if (Fonte == "SIHSUS") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/SIH/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    link_1 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Auxiliar/TAB_SIH.zip"
    link_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Auxiliar/TAB_SIH_199201-199712.zip"
    link_3 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Auxiliar/TAB_SIH_199801-200307.zip"
    link_4 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Auxiliar/TAB_SIH_200308-200712.zip"
    
    list_links <- c(link_1,link_2,link_3, link_4)
    
    for (i in seq_along(list_links)){
      
      file_name <- paste0("SIH",i,".zip")
      path_string <- paste0(dest_dir,file_name)
      
      download.file(list_links[i], path_string, mode = "wb")
    }
    
  }
  else if (Fonte == "SIM") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/SIM/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    link_1 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/TAB/OBITOS_CID9_TAB.zip"
    link_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/TAB/OBITOS_CID9_TAB.zip"
   
    
    list_links <- c(link_1,link_2)
    
    for (i in seq_along(list_links)){
      
      file_name <- paste0("SIM",i,".zip")
      path_string <- paste0(dest_dir,file_name)
      
      download.file(list_links[i], path_string, mode = "wb")
    }
    
  }
  else if (Fonte == "SINAN") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/SINAN/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    link_1 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/AUXILIAR/POPT_Estimativa_TCU_2019_Sinan.zip"
    link_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/AUXILIAR/TAB_SINANNET.zip"
    link_3 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/AUXILIAR/TAB_SINANONLINE.zip"
    
    
    list_links <- c(link_1,link_2,link_3)
    
    for (i in seq_along(list_links)){
      
      file_name <- paste0("SINAN",i,".zip")
      path_string <- paste0(dest_dir,file_name)
      
      download.file(list_links[i], path_string, mode = "wb")
    }
    
  }
  else if (Fonte == "SINASC") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/SINASC/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    link_1 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1994_1995/Auxiliar/NASC_ANT_TAB.zip"
    link_2 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Auxiliar/NASC_NOV_TAB.zip"
    
    
    
    list_links <- c(link_1,link_2)
    
    for (i in seq_along(list_links)){
      
      file_name <- paste0("SINASC",i,".zip")
      path_string <- paste0(dest_dir,file_name)
      
      download.file(list_links[i], path_string, mode = "wb")
    }
    
  }
  else if (Fonte == "SISCOLO") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/SISCOLO/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    file_name <- "SISCOLO.zip"
    path_string <- paste0(dest_dir, file_name) 
    extract_path <- dest_dir  
    
    download.file("ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISCOLO4/Auxiliar/TAB_SISCOLO4.zip", 
                  path_string, mode = "wb")
    
    unzip(zipfile = path_string, exdir = extract_path, overwrite = TRUE)
    
    file.remove(path_string)
  }
  else if (Fonte == "SISMAMA") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/SISMAMA/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    file_name <- "SISMAMA.zip"
    path_string <- paste0(dest_dir, file_name) 
    extract_path <- dest_dir  
    
    download.file("ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISMAMA/Auxiliar/TAB_SISMAMA.zip", 
                  path_string, mode = "wb")
    
    unzip(zipfile = path_string, exdir = extract_path, overwrite = TRUE)
    
    file.remove(path_string)
  }
  else if (Fonte == "SISPRENATAL") {
    
    wkd <- getwd()
    dest_dir <- paste0(wkd, "/SISPRENATAL/")
    
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    file_name <- "SISPRENATAL.zip"
    path_string <- paste0(dest_dir, file_name) 
    extract_path <- dest_dir  
    
    download.file("ftp://ftp.datasus.gov.br/dissemin/publicos/SISPRENATAL/201201_/Auxiliar/TAB_SISPRENATAL.zip", 
                  path_string, mode = "wb")
    
    unzip(zipfile = path_string, exdir = extract_path, overwrite = TRUE)
    
    file.remove(path_string)
  }
  else{
    print("Error: uxiliary file note found, check name and link ")
  }
}
  


## Downloading all by applying function


list_fontes <- c("IBGE","CIH","CIHA","PCE","PO","RESP","SIASUS","SIHSUS",
                 "SIM","SINAN", "SISCOLO","SISMAMA","SISPRENATAL","SINASC" )


for (fonte in list_fontes){
  auxi_files(Fonte = fonte)
}

  
auxi_files(Fonte = "SIA-SUS")
  
  
  
  
  
  
  
  #



