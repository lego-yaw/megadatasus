# 📦 megadatasus

**A Modern, Scalable Interface for Brazilian Public Health Data (DataSUS)**

---

## 📖 Overview

`megadatasus` is an R package designed to provide **fast, robust, and scalable access** to Brazilian public health data from DataSUS systems.

This package builds upon the ideas introduced by the original https://github.com/danicat/datasus, extending its capabilities to support modern data workflows, large-scale processing, and automated data transformation.

While earlier tools focus primarily on data retrieval, `megadatasus` provides a **complete data processing pipeline**, from ingestion to analysis-ready datasets.

---

## 🚀 Motivation

The original `datasus` package provides a valuable interface to DATASUS repositories, enabling users to download and access public healthcare datasets .

However, working with these datasets at scale presents several challenges:

- Large file sizes (millions of rows)
- Complex and encoded variables
- Manual data cleaning requirements
- Performance limitations with traditional formats (`.dbc`, `.csv`)

`megadatasus` was developed to address these limitations by introducing:

- High-performance data formats (Parquet)
- Efficient data processing (Apache Arrow)
- Automated dictionary-based decoding
- Standardized and reproducible workflows

---

## ⚡ Key Innovations

- 🚀 **Parquet-based data storage** for high performance
- ⚡ **Arrow integration** for fast and memory-efficient data access
- 🧠 **Automatic decoding of variables** (CID, municipalities, categories)
- 🔄 **Standardized cleaning across systems**
- 📦 **Unified interface for all DataSUS datasets**
- ☁️ **External data storage + local caching**

---

## 📂 Supported Systems

- SIM — Mortality Information System  
- SINASC — Live Birth Information System  
- SIHSUS — Hospital Information System  
- SIASUS — Ambulatory Information System  
- CNES — Health Establishments Registry  
- IBGE — Population Data  
- Additional systems: RESP, PCE, CIH, among others  

---
## Main Functions

- `acesso_datasus()`  
  Main function for downloading and importing DataSUS datasets. It provides unified access to multiple public health information systems and supports flexible queries by system, state, year, and file type.

- `Datasus_info()`  
  Lists available DataSUS systems, subsystems, files, and year coverage. It helps users explore the structure of the DataSUS repository and identify which datasets are available before downloading.

- `acesso_sinan()`  
  Dedicated function for accessing and importing SINAN datasets. It simplifies retrieval of notification data from the Notifiable Diseases Information System, supporting different diseases, years, and geographic filters.

- `acesso_IBGE()`  
  Imports IBGE reference data used to support DataSUS workflows, such as population data, municipality codes, and territorial information. It is useful for demographic standardization and integration with health datasets.

- `sinan_info()`  
  Returns metadata and file availability for SINAN datasets, including supported diseases, years, and folders. It is designed to help users navigate the SINAN structure before performing downloads.

- `clean_table()`  
  Cleans and standardizes imported datasets using dictionary-based decoding and system-specific transformation rules. It converts coded fields into human-readable values, formats dates, harmonizes columns, and returns analysis-ready tables.

- `describe_df()`  
  Returns a structured summary of a data frame, listing all column names and providing a general overview of the dataset structure. It is useful for quickly inspecting variables and understanding how the data is organized before further analysis.
---

## ⚙️ Installation

```r
install.packages("remotes")
remotes::install_github("lego-yaw/megadatasus")
```

## Exemple of usage

```r
# Download SIM data for Rio de Janeiro ("2005")
  df <- acesso_datasus(
  Fonte = "SIM-DO",
  UF = "RJ",
  ano_inicio = 2005,
  ano_final = 2005
 )


# Clean SIM dataset (Translation of variables)
 clean_df <- clean_table(df, fonte = "SIM-DO")
 

# Getting information on which data (UF, year and archives) available for SIM
sim_info <- Datasus_info("SIM")
```
