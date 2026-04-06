library(megadatasus)
library(devtools)
library(usethis)

df<- acesso_datasus("SISMAMA-CM", UF="RJ", ano_inicio=2015, ano_final=2015, mes_inicial = "01", mes_final = "01")
View(df)


df_clean <- megadatasus::clean_table(datafile = df, fonte = "SISMAMA-CM" )


# megadatasus::setup_data()

View(df_clean)



unlink("man", recursive = TRUE)
unlink("NAMESPACE")

# 4. Rebuild docs
devtools::document()

# 5. Reinstall cleanly
devtools::install(clean = TRUE, upgrade = "never")


library(megadatasus)


?acesso_datasus()


# Download SIM data for Rio de Janeiro ("2005")
df <- acesso_datasus(
  Fonte = "SIM-DO",
  UF = "RJ",
  ano_inicio = 2005,
  ano_final = 2005
)

View(df)

# Download SINASC data for Rio de Janeiro (monthly)
df <- acesso_datasus(
  Fonte = "SINASC-DN",
  UF = "MG",
  ano_inicio = 2019,
  ano_final = 2019,
  mes_inicial = "01",
  mes_final = "06",
  quiet = FALSE
)

View(df)

# Download e-SUS data
df <- acesso_datasus(
  Fonte = "e-SUS",
  UF = "BR",
  ano_inicio = 2023,
  ano_final = 2023
)

View(df)

describe_df(df, "e-SUS")



###


sim_info <- Datasus_info("SIM")
sim_info


sinasc_info <- Datasus_info("SINASC-DN")
sinasc_info


sim_info <- Datasus_info("SIM", refresh = TRUE)

View(sim_info)


clean_df <- clean_table(df, fonte = "SINASC-DN" ) # error



finais <- sinan_info("FINAIS")  ## eroor


describe_df(clean_df)




###

res <- acesso_IBGE(
  Fonte = "IBGE-POP",
  UF = "BR",
  ano_inicio = 2010,
  ano_final = 2012,
  dest_dir = "D:/RSUS/TESTFOLDER"
)



clean_table()



res <- acesso_IBGE(
  Fonte = "IBGE-POPT",
  UF = "BR",
  ano_inicio = 2018,
  ano_final = 2020,
  dest_dir = "D:/RSUS/TESTFOLDER",
  overwrite = TRUE,
  quiet = FALSE
)




##

df <- acesso_sinan(
  Fonte = "SINAN-ZIKA",
  UF = c("RJ", "SP"),
  ano_inicio = 2022,
  ano_final = 2022,
  prefer = "PRELIM",
  clean = TRUE,
  verbose = TRUE
)

View(df)


df <- acesso_sinan(
  Fonte = "SINAN-ZIKA",
  UF = "BR",
  ano_inicio = 2020,
  ano_final = 2020
)



df <- acesso_sinan(
  Fonte = "SINAN-DENG",
  UF = "BR",
  ano_inicio = 2020,
  ano_final = 2021
)
View(df)


df <- acesso_sinan(
  Fonte = "SINAN-ZIKA",
  UF = "BR",
  ano_inicio = 2020,
  ano_final = 2021
)

View(df)

describe_df(df, "SINAN")










all_sinan <- sinan_info("BOTH")
View(all_sinan)

df_ex <- data.frame(
  SINAN_UF = c("RJ"),
  DT_NOTIFIC = c("20240101"),
  CS_SEXO = c("M"),
  stringsAsFactors = FALSE
)

# Detecção automática
describe_df(df_ex)

# Forçando interpretação por sistema
describe_df(df_ex, sistema = "SINAN")



?describe_df()


library("megadatasus")



df <- acesso_datasus(
     Fonte = "SINASC-DN",
     UF = "MG",
     ano_inicio = 2019,
   ano_final = 2019, mes_inicial = "01",
     mes_final = "06",
     quiet = FALSE
   )




