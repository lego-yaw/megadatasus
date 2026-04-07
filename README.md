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

### SIM — Mortality Information System
Supported groups:
- `SIM-DO` — General mortality data
- `SIM-DOEXT` — Deaths from external causes
- `SIM-DOINF` — Infant mortality data
- `SIM-DOMAT` — Maternal mortality data
- `SIM-DOREXT` — Death records related to external causes/residence-based structure
- `SIM-DOFET` — Fetal death data

### SINASC — Live Birth Information System
Supported groups:
- `SINASC-DN` — Live birth declarations
- `SINASC-DNEX` — Extended live birth files

### SIHSUS — Hospital Information System
Supported groups:
- `SIHSUS-RD`
- `SIHSUS-ER`
- `SIHSUS-RJ`
- `SIHSUS-SP`

These files cover different hospital data tables available in the SIHSUS repository.

### SIASUS — Ambulatory Information System
Supported groups:
- `SIASUS-AB`
- `SIASUS-ABO`
- `SIASUS-ACF`
- `SIASUS-AD`
- `SIASUS-AM`
- `SIASUS-SD`
- `SIASUS-AQ`
- `SIASUS-AN`
- `SIASUS-AR`
- `SIASUS-ATD`
- `SIASUS-PA`
- `SIASUS-PS`

These correspond to the major ambulatory production and procedure files available in SIASUS.

### CNES — National Registry of Health Establishments
Supported groups:
- `CNES-DC`
- `CNES-EE`
- `CNES-EF`
- `CNES-EP`
- `CNES-EQ`
- `CNES-GM`
- `CNES-HB`
- `CNES-IN`
- `CNES-LT`
- `CNES-PF`
- `CNES-RC`
- `CNES-SR`
- `CNES-ST`

### SINAN — Notifiable Diseases Information System
Supported groups include several disease- and condition-specific files, such as:
- `SINAN-AIDC`
- `SINAN-ANIM`
- `SINAN-ANTR`
- `SINAN-BOTU`
- `SINAN-CANC`
- `SINAN-RAIV`
- `SINAN-CHAG`
- `SINAN-CHIK`
- `SINAN-COLE`
- `SINAN-COQU`
- `SINAN-DENG`
- `SINAN-DERM`
- `SINAN-ESPO`
- `SINAN-ESQU`
- `SINAN-EXAN`
- `SINAN-FMAC`
- `SINAN-FTIF`
- `SINAN-HANS`
- `SINAN-HANT`
- `SINAN-HEPA`
- `SINAN-HIVA`
- `SINAN-HIVC`
- `SINAN-HIVE`
- `SINAN-HIVG`
- `SINAN-IEXO`
- `SINAN-LEPT`
- `SINAN-LERD`
- `SINAN-LTAN`
- `SINAN-MALA`
- `SINAN-MENI`
- `SINAN-MENT`
- `SINAN-NTRA`
- `SINAN-PAIR`
- `SINAN-PEST`
- `SINAN-PFAN`
- `SINAN-PNEU`
- `SINAN-ROTA`
- `SINAN-SDTA`
- `SINAN-SIFA`
- `SINAN-SIFC`
- `SINAN-SIFG`
- `SINAN-SRC`
- `SINAN-TETA`
- `SINAN-TETN`
- `SINAN-TOXC`
- `SINAN-TOXG`
- `SINAN-TRAC`
- `SINAN-TUBE`
- `SINAN-VARC`
- `SINAN-VIOL`
- `SINAN-ZIKA`
- `SINAN-AIDA`
- `SINAN-ACGR`
- `SINAN-ACBI`

### IBGE — Population and Territorial Reference Data
Supported group:
- `IBGE-POP`
- `IBGE-POPT`

These datasets are useful for population denominators, municipality codes, and demographic integration with health data.

### RESP — Public Health Event / Surveillance Data
Supported group:
- `RESP`

### SISPRENATAL — Prenatal Care Information System
Supported group:
- `SISPRENATAL-PN`

### PCE — Schistosomiasis Control Program
Supported group:
- `PCE`

### PO — Oncology Panel Data
Supported group:
- `PO`

### CIH — Hospital Communication Data
Supported group:
- `CIH-CR`

### CIHA — Expanded Hospital Communication Data
Supported group:
- `CIHA`

### SISCOLO — Cervical Cancer Information System
Supported groups:
- `SISCOLO-CC`
- `SISCOLO-HC`

### SISMAMA — Breast Cancer Information System
Supported groups:
- `SISMAMA-CM`
- `SISMAMA-HC`

### ESUS Notifica
Supported group:
- `e-SUS` (ESUSNOTIFICA)
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
