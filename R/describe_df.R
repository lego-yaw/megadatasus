#' Descreve colunas de bases do DataSUS
#'
#' Identifica e descreve variáveis de um \code{data.frame} com base em
#' dicionários internos de diferentes sistemas do DataSUS, como SIH, SINAN,
#' SIM, SINASC, CIHA, SIA/APAC, SISCOLO, SISMAMA, CNES e outros.
#'
#' A função pode detectar automaticamente o sistema mais provável a partir
#' dos nomes das colunas ou usar um sistema informado manualmente. O resultado
#' é uma tabela com o nome original da coluna, o nome DBF padronizado,
#' sua descrição e o sistema associado.
#'
#' @param df Um \code{data.frame} contendo as colunas a serem descritas.
#' @param sistema Um valor de texto indicando o sistema a ser priorizado
#'   na interpretação das colunas. Pode ser \code{"auto"} para detecção
#'   automática ou um dos seguintes:
#'   \code{"SIH"}, \code{"SINAN"}, \code{"SIM"}, \code{"SINASC"},
#'   \code{"CIHA"}, \code{"SIA_APAC"}, \code{"SISCOLO"}, \code{"SISMAMA"},
#'   \code{"CNES_DC"}, \code{"CNES_EQ"}, \code{"CNES_EP"}, \code{"CNES_EE"},
#'   \code{"CNES_EF"}, \code{"CNES_ST"}, \code{"CNES_GM"}, \code{"CNES_HB"},
#'   \code{"CNES_IN"}, \code{"CNES_LT"}, \code{"CNES_PF"}, \code{"CNES_RC"},
#'   \code{"CNES_SR"}.
#'
#' @details
#' A função converte os nomes das colunas para maiúsculas, compara com os
#' dicionários internos e retorna uma tabela descritiva. Quando \code{sistema = "auto"},
#' a função tenta inferir o sistema mais provável com base na frequência de
#' correspondência das variáveis encontradas.
#'
#' Se uma coluna não for encontrada nos dicionários, sua descrição será
#' marcada como \code{"Não mapeado"} e o sistema como \code{"desconhecido"}.
#'
#' O sistema detectado automaticamente é armazenado como atributo do objeto
#' retornado:
#'
#' \preformatted{
#' attr(resultado, "detected_system")
#' }
#'
#' @return
#' Um \code{data.frame} com as colunas:
#' \describe{
#'   \item{column}{Nome original da coluna no objeto de entrada.}
#'   \item{dbf}{Nome da variável em maiúsculas, usado para correspondência com os dicionários.}
#'   \item{description}{Descrição textual da variável.}
#'   \item{sistema}{Sistema DataSUS associado à variável.}
#' }
#'
#' Além disso, o objeto retornado possui o atributo:
#' \describe{
#'   \item{detected_system}{Sistema detectado automaticamente ou informado pelo usuário.}
#' }
#'
#' @examples
#' df_ex <- data.frame(
#'   SINAN_UF = c("RJ"),
#'   DT_NOTIFIC = c("20240101"),
#'   CS_SEXO = c("M"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Detecção automática
#' describe_df(df_ex)
#'
#' # Forçando interpretação por sistema
#' describe_df(df_ex, sistema = "SINAN")
#'
#' @export

describe_df <- function(
    df,
    sistema = c(
      "auto", "SIH", "SINAN", "SIM", "SINASC", "CIHA", "SIA_APAC",
      "SISCOLO", "SISMAMA",
      "CNES", "CNES_DC", "CNES_EQ", "CNES_EP", "CNES_EE", "CNES_EF",
      "CNES_ST", "CNES_GM", "CNES_HB", "CNES_IN", "CNES_LT",
      "CNES_PF", "CNES_RC", "CNES_SR",
      "PRENATAL", "ONCOLOGIA", "PCE", "RESP", "e-SUS"
    )
) {

  sistema <- match.arg(sistema)

  if (is.null(names(df))) {
    stop("df must have column names.", call. = FALSE)
  }

  cols <- toupper(names(df))

  # -----------------------------
  # HELPERS
  # -----------------------------
  make_dict <- function(dbf, description, sistema = "SINAN") {
    dbf <- toupper(as.character(dbf))
    description <- as.character(description)
    stopifnot(length(dbf) == length(description))
    data.frame(
      dbf = dbf,
      description = description,
      sistema = sistema,
      stringsAsFactors = FALSE
    )
  }

  make_dict_named <- function(desc_map, sistema = "SINAN") {
    stopifnot(length(desc_map) > 0)
    data.frame(
      dbf = toupper(names(desc_map)),
      description = unname(desc_map),
      sistema = sistema,
      stringsAsFactors = FALSE
    )
  }

  normalize_dict <- function(x) {
    x$dbf <- toupper(as.character(x$dbf))
    x$description <- as.character(x$description)
    x$sistema <- as.character(x$sistema)
    x
  }

  # -----------------------------
  # 1. DICTIONARIES
  # -----------------------------

  # ---- SIH ----
  dict_sih <- data.frame(
    dbf = c(
      "UF_ZI","ANO_CMPT","MES_CMPT","ESPEC","CGC_HOSP","N_AIH","CEP","MUNIC_RES",
      "NASC","SEXO","UTI_MES_TO","MARCA_UTI","QT_DIARIAS","PROC_REA",
      "VAL_SH","VAL_SP","VAL_TOT","VAL_UTI","DI_INTER","DT_SAIDA",
      "DIAG_PRINC","DIAG_SECUN","COBRANCA","IDADE","DIAS_PERM","MORTE","CNES",
      "AIH","ANO","MES","DT_INTER","MUN_RES","UF_RES",
      "SEQUENCIA","REMESSA","CO_ERRO","MUN_MOV"
    ),
    description = c(
      "Município gestor.",
      "Ano de processamento da AIH.",
      "Mês de processamento da AIH.",
      "Especialidade do leito.",
      "CNPJ do hospital.",
      "Número da AIH.",
      "CEP do paciente.",
      "Município de residência.",
      "Data de nascimento.",
      "Sexo do paciente.",
      "Dias de UTI no mês.",
      "Tipo de UTI.",
      "Quantidade de diárias.",
      "Procedimento realizado.",
      "Valor hospitalar.",
      "Valor profissional.",
      "Valor total da AIH.",
      "Valor de UTI.",
      "Data de internação.",
      "Data de saída.",
      "Diagnóstico principal (CID10).",
      "Diagnóstico secundário.",
      "Motivo de saída.",
      "Idade do paciente.",
      "Dias de permanência.",
      "Indica óbito.",
      "Código CNES.",
      "Número da AIH.",
      "Ano de processamento da AIH.",
      "Mês de processamento da AIH.",
      "Data de internação.",
      "Município de residência.",
      "UF de residência.",
      "Sequencial da AIH na remessa.",
      "Número da remessa.",
      "Código de erro de processamento.",
      "Município de movimentação/atendimento."
    ),
    sistema = "SIH",
    stringsAsFactors = FALSE
  )

  # ---- SINAN ----
  dict_sinan_generic <- make_dict(
    dbf = c(
      "NU_NOTIFIC","TP_NOT","ID_AGRAVO","CS_SUSPEIT","DT_NOTIFIC","SEM_NOT","NU_ANO",
      "SG_UF_NOT","ID_MUNICIP","ID_REGIONA","ID_UNIDADE","DT_SIN_PRI","SEM_PRI",
      "NM_PACIENT","DT_NASC","NU_IDADE_N","CS_SEXO","CS_GESTANT","CS_RACA","CS_ESCOL_N",
      "ID_CNS_SUS","NM_MAE_PAC","SG_UF","ID_MN_RESI","ID_RG_RESI","ID_DISTRIT",
      "ID_BAIRRO","NM_BAIRRO","ID_LOGRADO","NM_LOGRADO","NU_NUMERO","NM_COMPLEM",
      "ID_GEO1","ID_GEO2","NM_REFEREN","NU_CEP","NU_DDD_TEL","NU_TELEFON","CS_ZONA",
      "ID_PAIS","NDUPLIC_N","IN_VINCULA","DT_INVEST","CLASSI_FIN","CRITERIO",
      "TPAUTOCTO","COUFINF","COPAISINF","COMUNINF","CODISINF","CO_BAINFC","NOBAIINF",
      "DOENCA_TRA","EVOLUCAO","DT_OBITO","DT_ENCERRA","FONETICA_N","SOUNDEX",
      "DT_DIGITA","DT_TRANSUS","DT_TRANSDM","DT_TRANSSM","DT_TRANSRM","DT_TRANSRS",
      "DT_TRANSSE","NU_LOTE_V","NU_LOTE_H","CS_FLXRET","FLXRECEBI","IDENT_MICR",
      "MIGRADO_W"
    ),
    description = c(
      "Número da notificação.",
      "Tipo de notificação.",
      "Código do agravo.",
      "Subtipo de suspeita do agravo.",
      "Data da notificação.",
      "Semana epidemiológica da notificação.",
      "Ano da notificação.",
      "UF da notificação.",
      "Município da notificação.",
      "Regional de saúde da notificação.",
      "Unidade notificadora.",
      "Data dos primeiros sintomas.",
      "Semana epidemiológica dos sintomas.",
      "Nome do paciente.",
      "Data de nascimento.",
      "Idade.",
      "Sexo.",
      "Situação gestacional.",
      "Raça/cor.",
      "Escolaridade.",
      "Número do cartão SUS.",
      "Nome da mãe.",
      "UF de residência.",
      "Município de residência.",
      "Regional de saúde da residência.",
      "Distrito de residência.",
      "Código do bairro.",
      "Nome do bairro.",
      "Código do logradouro.",
      "Nome do logradouro.",
      "Número do endereço.",
      "Complemento.",
      "Georreferência 1.",
      "Georreferência 2.",
      "Ponto de referência.",
      "CEP.",
      "DDD.",
      "Telefone.",
      "Zona de residência.",
      "País de residência.",
      "Indicador de duplicidade.",
      "Indicador de vinculação.",
      "Data da investigação.",
      "Classificação final.",
      "Critério de confirmação.",
      "Caso autóctone.",
      "UF provável de infecção.",
      "País provável de infecção.",
      "Município provável de infecção.",
      "Distrito provável de infecção.",
      "Código do bairro da infecção.",
      "Nome do bairro da infecção.",
      "Doença relacionada ao trabalho.",
      "Evolução do caso.",
      "Data do óbito.",
      "Data de encerramento.",
      "Chave fonética.",
      "Descrição Soundex.",
      "Data de digitação.",
      "Data de transferência da unidade.",
      "Data de transferência do distrito.",
      "Data de transferência da secretaria municipal.",
      "Data de transferência da regional municipal.",
      "Data de transferência da regional de saúde.",
      "Data de transferência da secretaria estadual.",
      "Número do lote vertical.",
      "Número do lote horizontal.",
      "Indicador de fluxo de retorno.",
      "Recebido por fluxo de retorno.",
      "Identificador do computador.",
      "Migrado do SINAN Windows."
    ),
    sistema = "SINAN"
  )
  dict_sinan_dengue <- make_dict(
    dbf = c(
      "DT_SORO","RESUL_SORO","DT_NS1","RESUL_NS1","DT_VIRAL","RESUL_VI_N","SOROTIPO",
      "HISTOPA_N","IMUNOH_N","RESUL_PCR_","MANI_HEMOR","EPISTAXE","GENGIVO","METRO",
      "PETEQUIAS","HEMATURA","SANGRAM","LACO_N","PLASMATICO","EVIDENCIA","CON_FHD",
      "COMPLICA","HOSPITALIZ","DT_INTERNA","UF","MUNICIPIO","HOSPITAL","TP_SISTEMA",
      "FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO","NAUSEA","DOR_COSTAS",
      "CONJUNTVIT","ARTRITE","ARTRALGIA","PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO",
      "DIABETES","HEMATOLOG","HEPATOPAT","RENAL","HIPERTENSA","ACIDO_PEPT","AUTO_IMUNE",
      "DT_CHIK_S1","DT_CHIK_S2","DT_PRNT","RES_CHIKS1","RES_CHIKS2","RESUL_PRNT",
      "DT_PCR","CLINC_CHIK"
    ),
    description = c(
      "Data do exame sorológico (IgM).",
      "Resultado da sorologia (IgM).",
      "Data do exame NS1.",
      "Resultado do exame NS1.",
      "Data do exame de isolamento viral.",
      "Resultado do isolamento viral.",
      "Sorotipo da dengue.",
      "Resultado histopatológico.",
      "Resultado de imuno-histoquímica.",
      "Resultado do RT-PCR.",
      "Manifestações hemorrágicas.",
      "Epistaxe.",
      "Sangramento gengival.",
      "Metrorragia.",
      "Petéquias.",
      "Hematúria.",
      "Sangramento gastrointestinal.",
      "Prova do laço.",
      "Extravasamento plasmático.",
      "Evidência de complicação.",
      "Grau de dengue hemorrágica.",
      "Tipo de complicação da dengue.",
      "Indica hospitalização.",
      "Data da internação.",
      "UF da internação.",
      "Município da internação.",
      "Hospital de internação.",
      "Tipo de sistema de registro.",
      "Febre.",
      "Mialgia.",
      "Cefaleia.",
      "Exantema.",
      "Vômito.",
      "Náusea.",
      "Dor nas costas.",
      "Conjuntivite.",
      "Artrite.",
      "Artralgia.",
      "Indicador de petéquias.",
      "Leucopenia.",
      "Prova do laço (indicador).",
      "Dor retro-orbitária.",
      "Diabetes pré-existente.",
      "Doença hematológica pré-existente.",
      "Hepatopatia.",
      "Doença renal crônica.",
      "Hipertensão.",
      "Doença ácido-péptica.",
      "Doença autoimune.",
      "Data do 1º exame IgM para Chikungunya.",
      "Data do 2º exame IgM para Chikungunya.",
      "Data do exame PRNT.",
      "Resultado do 1º exame Chikungunya.",
      "Resultado do 2º exame Chikungunya.",
      "Resultado do PRNT.",
      "Data do RT-PCR.",
      "Apresentação clínica de Chikungunya."
    ),
    sistema = "SINAN"
  )
  dict_sinan_acbi <- make_dict(
    dbf = c(
      "DT_ACID","SIT_TRAB","CNAE","TERCEIRIZA","UF_EMP","MUN_EMP","PERCUTANEA","MUCOSA",
      "PELE_INTEG","PELE_NAO_I","OUTRO_EXP","MAT_ORG","TIPO_ACID","AGENTE","LUVA",
      "AVENTAL","OCULOS","MASCARA","FACIAL","BOTA","VACINA","ANTI_HIV","HBSAG",
      "ANTI_HBS","ANTI_HCV","SEM_QUIMIO","RECUSA_QUI","AZT3TC","AZT3TC_IND","AZT3TC_NFV",
      "IMU_HEP_B","VAC_HEP_B","OUTRO_ARV","FONTE","FO_HBSAG","FO_ANT_HIV","FO_ANT_HBC",
      "FO_ANT_HCV","CAT"
    ),
    description = c(
      "Data do acidente.",
      "Situação de trabalho.",
      "Código CNAE.",
      "Indica terceirização.",
      "UF do empregador.",
      "Município do empregador.",
      "Exposição percutânea.",
      "Exposição mucosa.",
      "Exposição em pele íntegra.",
      "Exposição em pele não íntegra.",
      "Outro tipo de exposição.",
      "Material orgânico envolvido.",
      "Tipo de acidente.",
      "Agente causador.",
      "Uso de luvas.",
      "Uso de avental.",
      "Uso de óculos.",
      "Uso de máscara.",
      "Uso de proteção facial.",
      "Uso de botas.",
      "Vacinação contra hepatite B.",
      "Resultado anti-HIV.",
      "Resultado HBsAg.",
      "Resultado anti-HBs.",
      "Resultado anti-HCV.",
      "Quimioprofilaxia não indicada.",
      "Quimioprofilaxia recusada.",
      "Esquema AZT+3TC.",
      "AZT+3TC+Indinavir.",
      "AZT+3TC+Nelfinavir.",
      "Uso de imunoglobulina Hep B.",
      "Vacina Hep B aplicada.",
      "Outro antirretroviral.",
      "Paciente fonte conhecido.",
      "HBsAg do paciente fonte.",
      "Anti-HIV do paciente fonte.",
      "Anti-HBc do paciente fonte.",
      "Anti-HCV do paciente fonte.",
      "CAT emitida."
    ),
    sistema = "SINAN"
  )
  dict_sinan_acgr <- make_dict(
    dbf = c(
      "NUTEMPO","TPTEMPO","LOCAL_ACID","CNAE_PRIN","UF_ACID","MUN_ACID","HORA_ACID",
      "HORA_JOR","CID_ACID","MAIS_TRAB","ATENDE_MED","DT_ATENDE","UF_ATENDE","MUN_ATENDE",
      "UNI_ATENDE","PART_CORP1","PART_CORP2","PART_CORP3","CID_LESAO","REGIME"
    ),
    description = c(
      "Tempo na ocupação.",
      "Tipo/unidade de tempo.",
      "Local do acidente.",
      "CNAE principal.",
      "UF do acidente.",
      "Município do acidente.",
      "Hora do acidente.",
      "Hora após jornada.",
      "Causa do acidente (CID).",
      "Outros trabalhadores afetados.",
      "Atendimento médico.",
      "Data do atendimento.",
      "UF do atendimento.",
      "Município do atendimento.",
      "Unidade de atendimento.",
      "Parte do corpo afetada 1.",
      "Parte do corpo afetada 2.",
      "Parte do corpo afetada 3.",
      "Diagnóstico da lesão.",
      "Regime de tratamento."
    ),
    sistema = "SINAN"
  )
  dict_sinan_aids <- make_dict(
    dbf = c(
      "DT_DIAG","ANT_TRASMI","ANTRELSE_N","ANT_DROGA","ANT_ACIDEN","ANT_HEMOLF",
      "ANTTRANS_M","ANTDTTRANS","ANTUFTRANS","ANT_REL_CA","ANTMUNTRAN","ANT_INSTTR",
      "ANT_INVEST","LAB_TRIAGE","DTTRIAGEM","LAB_CONFIR","DT_CONFIRM","TPRAPIDO1",
      "TPRAPIDO2","TPRAPIDO3","DT_RAPIDO","ANT_SARCOM","ANT_TUBERC","ANT_CANDID",
      "ANT_PULMON","ANT_HERPES","ANT_DISFUN","ANT_DIARRE","ANT_FEBRE","ANT_CAQUEX",
      "ANT_ASTERI","ANT_DERMAT","ANT_ANEMIA","ANT_TOSSE","ANT_LINFO","ANT_PULM_N",
      "ANT_CITO","ANT_CANCER","ANT_CRIPTO","ANT_CRIP_1","ANT_HISTO","ANT_ISOPOR",
      "ANT_H_SIMP","ANT_LEUCO","ANT_LINFOM","ANT_LINFO_","ANT_MICRO","ANT_PNEUMO",
      "ANT_CHAGAS","ANT_SALMO","ANT_TOXO","ANT_CONTAG","DEF_DIAGNO","TRA_UF",
      "TRA_MUNICI","TRA_UNIDAD"
    ),
    description = c(
      "Data do diagnóstico.",
      "Transmissão vertical.",
      "Exposição sexual.",
      "Uso de drogas injetáveis.",
      "Acidente com material biológico.",
      "Histórico de hemofilia.",
      "Histórico de transfusão.",
      "Data da transfusão.",
      "UF da transfusão.",
      "Categoria de exposição.",
      "Município da transfusão.",
      "Instituição de transfusão.",
      "Investigação da transfusão.",
      "Resultado triagem HIV.",
      "Data da triagem.",
      "Resultado confirmatório.",
      "Data confirmação.",
      "Teste rápido 1.",
      "Teste rápido 2.",
      "Teste rápido 3.",
      "Data teste rápido.",
      "Sarcoma de Kaposi.",
      "Tuberculose disseminada.",
      "Candidíase oral.",
      "Tuberculose pulmonar.",
      "Herpes zoster.",
      "Disfunção SNC.",
      "Diarreia.",
      "Febre.",
      "Caquexia.",
      "Astenia.",
      "Dermatite persistente.",
      "Anemia/linfopenia.",
      "Tosse persistente.",
      "Linfadenopatia.",
      "Candidíase pulmonar.",
      "Citomegalovírus.",
      "Câncer cervical.",
      "Criptococose.",
      "Criptosporidiose.",
      "Histoplasmose.",
      "Isosporidiose.",
      "Herpes simples.",
      "Leucoencefalopatia.",
      "Linfoma não Hodgkin.",
      "Linfoma primário.",
      "Micobacteriose disseminada.",
      "Pneumonia por Pneumocystis.",
      "Reativação Chagas.",
      "Salmonelose.",
      "Toxoplasmose cerebral.",
      "Linfócitos < 350.",
      "Critério de óbito.",
      "UF do tratamento.",
      "Município do tratamento.",
      "Unidade de tratamento."
    ),
    sistema = "SINAN"
  )
  dict_sinan_animais <- make_dict(
    dbf = c(
      "ANT_MUNIC_","ANT_DT_ACI","ANT_ZONA","ANT_TEMPO_","ANT_LOCA_1","MCLI_LOCAL",
      "CLI_DOR","CLI_EDEMA","CLI_EQUIMO","CLI_NECROS","CLI_LOCAL_","MCLI_SIST",
      "CLI_NEURO","CLI_HEMORR","CLI_VAGAIS","CLI_MIOLIT","CLI_RENAL","CLI_OUTR_2",
      "CLI_TEMPO_","TP_ACIDENT","ANI_SERPEN","ANI_ARANHA","ANI_LAGART","TRA_CLASSI",
      "CON_SOROTE","NU_AMPOLAS","NU_AMPOL_6","NU_AMPO_5","NU_AMPOL_1","NU_AMPOL_4",
      "NU_AMPOL_9","NU_AMPOL_8","NU_AMPOL_3","NU_AMPO_7","COM_LOC","COM_SECUND",
      "COM_NECROS","COM_COMPOR","COM_DEFICT","COM_APUTAC","COM_SISTEM","COM_RENAL",
      "COM_EDEMA","COM_SEPTIC","COM_CHOQUE"
    ),
    description = c(
      "Município do acidente.",
      "Data do acidente.",
      "Zona do acidente.",
      "Tempo até atendimento.",
      "Local da picada.",
      "Manifestações locais.",
      "Dor.",
      "Edema.",
      "Equimose.",
      "Necrose.",
      "Outras alterações locais.",
      "Manifestações sistêmicas.",
      "Sintomas neurológicos.",
      "Sintomas hemorrágicos.",
      "Sintomas vagais.",
      "Sintomas miolíticos.",
      "Comprometimento renal.",
      "Outros sintomas sistêmicos.",
      "Tempo de coagulação.",
      "Tipo de acidente.",
      "Serpente envolvida.",
      "Aranha envolvida.",
      "Lagarta envolvida.",
      "Classificação da gravidade.",
      "Uso de soro.",
      "Ampolas antibotrópicas.",
      "Ampolas botrópico-laquético.",
      "Ampolas botrópico-crotálico.",
      "Ampolas crotálicas.",
      "Ampolas elapídicas.",
      "Ampolas antiescorpiônicas.",
      "Ampolas antiaracnídicas.",
      "Ampolas antilonômicas.",
      "Ampolas antiloxoscélicas.",
      "Complicações locais.",
      "Infecção secundária.",
      "Necrose extensa.",
      "Síndrome compartimental.",
      "Déficit funcional.",
      "Amputação.",
      "Complicações sistêmicas.",
      "Insuficiência renal.",
      "Edema pulmonar.",
      "Sepse.",
      "Choque."
    ),
    sistema = "SINAN"
  )
  dict_sinan_extra <- make_dict_named(c(
    "ANO_NASC"   = "Ano de nascimento.",
    "DTPRICONS"  = "Data da primeira consulta.",
    "ANT_VACINA" = "Histórico de vacinação.",
    "ANT_DOSES"  = "Número de doses recebidas.",
    "ANT_DT_VAC" = "Data da última dose da vacina.",
    "ANT_30_DIA" = "Vacinação nos últimos 30 dias.",
    "ANT_PAIS"   = "País onde ocorreu a vacinação.",
    "CLI_FEBRE"  = "Presença de febre.",
    "CLI_DIARRE" = "Presença de diarreia.",
    "CLI_DORES"  = "Presença de dores no corpo.",
    "CLI_SINTOM" = "Outros sintomas clínicos.",
    "CLI_VOMITO" = "Presença de vômito.",
    "CLI_OBSTIP" = "Presença de obstipação.",
    "CLI_CEFALE" = "Presença de cefaleia.",
    "CLI_OUTROS" = "Outros sintomas relatados.",
    "OUTROS_DES" = "Descrição dos outros sintomas.",
    "CLI_DT"     = "Data de início dos sintomas.",
    "CLI_AGUDA"  = "Início agudo.",
    "CLI_FLACID" = "Paralisia flácida.",
    "CLI_ASSIME" = "Assimetria muscular.",
    "CLI_PROGRE" = "Progressão dos sintomas.",
    "CLI_ASCEND" = "Evolução ascendente.",
    "CLI_DESCEN" = "Evolução descendente.",
    "CLI_F_MIE"  = "Força muscular no membro inferior esquerdo.",
    "CLI_F_MSE"  = "Força muscular no membro superior esquerdo.",
    "CLI_F_MID"  = "Força muscular no membro inferior direito.",
    "CLI_F_MSD"  = "Força muscular no membro superior direito.",
    "LOCA_MIE_N" = "Localização da alteração no membro inferior esquerdo.",
    "LOCA_MSE_N" = "Localização da alteração no membro superior esquerdo.",
    "LOCA_MID_N" = "Localização da alteração no membro inferior direito.",
    "LOCA_MSD_N" = "Localização da alteração no membro superior direito.",
    "CLI_RESPIR" = "Comprometimento respiratório.",
    "CLI_CERVIC" = "Rigidez cervical.",
    "CLI_FACE"   = "Comprometimento facial.",
    "CLI_DT_EXA" = "Data do exame clínico.",
    "CLI_A_FMIE" = "Alteração de força no membro inferior esquerdo.",
    "CLI_A_FMSE" = "Alteração de força no membro superior esquerdo.",
    "CLI_A_FMID" = "Alteração de força no membro inferior direito.",
    "CLI_A_FMSD" = "Alteração de força no membro superior direito.",
    "CLI_A_TMIE" = "Alteração de tônus no membro inferior esquerdo.",
    "CLI_A_TMSE" = "Alteração de tônus no membro superior esquerdo.",
    "CLI_A_TMID" = "Alteração de tônus no membro inferior direito.",
    "CLI_A_TMSD" = "Alteração de tônus no membro superior direito.",
    "CLI_A_T_CE" = "Alteração de tônus cervical.",
    "CLI_A_T_FA" = "Alteração de tônus facial.",
    "CLI_A_SMIE" = "Alteração de sensibilidade no membro inferior esquerdo.",
    "CLI_A_SMSE" = "Alteração de sensibilidade no membro superior esquerdo.",
    "CLI_A_SMID" = "Alteração de sensibilidade no membro inferior direito.",
    "CLI_A_SMSD" = "Alteração de sensibilidade no membro superior direito.",
    "CLI_A_S_FA" = "Alteração de sensibilidade facial.",
    "CLI_AQ_E_N" = "Reflexo aquileu esquerdo.",
    "CLI_AQ_D_N" = "Reflexo aquileu direito.",
    "CLI_PATE_N" = "Reflexo patelar esquerdo.",
    "CLI_PATD_N" = "Reflexo patelar direito.",
    "CLI_BICE_N" = "Reflexo bicipital esquerdo.",
    "CLI_BICD_N" = "Reflexo bicipital direito.",
    "CLI_TRIE_N" = "Reflexo tricipital esquerdo.",
    "CLI_TRID_N" = "Reflexo tricipital direito.",
    "CLI_FLE_E"  = "Resposta de flexão à esquerda.",
    "CLI_FLE_D"  = "Resposta de flexão à direita.",
    "CLI_EXT_E"  = "Resposta de extensão à esquerda.",
    "CLI_EXT_D"  = "Resposta de extensão à direita.",
    "CLI_KERNIG" = "Sinal de Kernig.",
    "CLI_NUCA"   = "Rigidez de nuca.",
    "CLI_BRUDZ"  = "Sinal de Brudzinski.",
    "CLI_CONTAT" = "Histórico de contato com caso semelhante.",
    "CLI_CON_ES" = "Alteração do nível de consciência.",
    "CLI_INJECA" = "Histórico de injeção recente.",
    "CLI_LOCAL"  = "Localização dos sinais e sintomas.",
    "ATE_HIPOTE" = "Hipotonia no atendimento.",
    "ATE_HOSP"   = "Indica hospitalização.",
    "ATE_DT_INT" = "Data da internação.",
    "ATE_UF"     = "UF do atendimento.",
    "ATE_MUNICI" = "Município do atendimento.",
    "LAB_DT_F1"  = "Data da primeira coleta laboratorial.",
    "LAB_DT_NLE" = "Data da avaliação neurológica ou laboratorial.",
    "LAB_DT_CEN" = "Data de envio ao laboratório central.",
    "LAB_DT_R1"  = "Data do primeiro resultado.",
    "LAB_Q_F"    = "Quantidade de amostra.",
    "LAB_CON_F"  = "Condição da amostra.",
    "LAB_DT_RE1" = "Data do resultado do exame.",
    "LAB_RES_F1" = "Resultado laboratorial 1.",
    "LAB_RES_F2" = "Resultado laboratorial 2.",
    "LAB_RES_F3" = "Resultado laboratorial 3.",
    "LAB_DT_L_1" = "Data do primeiro exame de líquor.",
    "LAB_L_CEL1" = "Contagem celular no líquor (1ª coleta).",
    "LAB_L_LIN1" = "Contagem de linfócitos no líquor (1ª coleta).",
    "LAB_L_PRO1" = "Proteína no líquor (1ª coleta).",
    "LAB_L_GLI1" = "Glicose no líquor (1ª coleta).",
    "LAB_L_CL1"  = "Cloretos no líquor (1ª coleta).",
    "LAB_DT_L_2" = "Data do segundo exame de líquor.",
    "LAB_L_CEL2" = "Contagem celular no líquor (2ª coleta).",
    "LAB_L_LIN2" = "Contagem de linfócitos no líquor (2ª coleta).",
    "LAB_L_PRO2" = "Proteína no líquor (2ª coleta).",
    "LAB_L_GLI2" = "Glicose no líquor (2ª coleta).",
    "LAB_L_CL2"  = "Cloretos no líquor (2ª coleta).",
    "LAB_DT_E_1" = "Data do exame etiológico.",
    "LAB_E_D_1"  = "Resultado do exame etiológico.",
    "LAB_CELEBR" = "Resultado de exame cerebral.",
    "LAB_MEDULA" = "Resultado de exame de medula óssea.",
    "LAB_INTEST" = "Resultado de exame intestinal.",
    "LAB_DT_C1"  = "Data de exame complementar.",
    "LAB_RESULT" = "Resultado final laboratorial.",
    "EVOR_DT_RE" = "Data da reavaliação do caso.",
    "EVOR_F_MIE" = "Força muscular no membro inferior esquerdo após evolução.",
    "EVOR_F_MSE" = "Força muscular no membro superior esquerdo após evolução.",
    "EVOR_F_MID" = "Força muscular no membro inferior direito após evolução.",
    "EVOR_F_MSD" = "Força muscular no membro superior direito após evolução.",
    "TONR_MIE_N" = "Tônus muscular do membro inferior esquerdo.",
    "TONR_MSE_N" = "Tônus muscular do membro superior esquerdo.",
    "TONR_MID_N" = "Tônus muscular do membro inferior direito.",
    "TONR_MSD_N" = "Tônus muscular do membro superior direito.",
    "TONR_CER_N" = "Tônus cervical.",
    "TONR_FAC_N" = "Tônus facial.",
    "REFR_AQE_N" = "Reflexo aquileu esquerdo.",
    "REFR_AQD_N" = "Reflexo aquileu direito.",
    "REFR_PAE_N" = "Reflexo patelar esquerdo.",
    "REFR_PAD_N" = "Reflexo patelar direito.",
    "REFR_BIE_N" = "Reflexo bicipital esquerdo.",
    "REFR_BID_N" = "Reflexo bicipital direito.",
    "REFR_TRE_N" = "Reflexo tricipital esquerdo.",
    "REFR_TRD_N" = "Reflexo tricipital direito.",
    "EVOR_RC_FE" = "Reflexo cutâneo facial esquerdo.",
    "EVOR_RC_FD" = "Reflexo cutâneo facial direito.",
    "EVOR_RC_EE" = "Reflexo cutâneo superior esquerdo.",
    "EVOR_RC_ED" = "Reflexo cutâneo superior direito.",
    "EVOR_A_MIE" = "Alteração no membro inferior esquerdo após evolução.",
    "EVOR_A_MSE" = "Alteração no membro superior esquerdo após evolução.",
    "EVOR_A_MID" = "Alteração no membro inferior direito após evolução.",
    "EVOR_A_MSD" = "Alteração no membro superior direito após evolução.",
    "EVOR_S_MIE" = "Sensibilidade no membro inferior esquerdo após evolução.",
    "EVOR_S_MSE" = "Sensibilidade no membro superior esquerdo após evolução.",
    "EVOR_S_MID" = "Sensibilidade no membro inferior direito após evolução.",
    "EVOR_S_MSD" = "Sensibilidade no membro superior direito após evolução.",
    "EVOR_S_FAC" = "Sensibilidade facial após evolução.",
    "EVOR1_DT_R" = "Data da primeira revisão da evolução.",
    "CON_DESCAR" = "Indicador de caso descartado.",
    "NU_LOTE_I"  = "Número do lote interno.",
    "SINAN_FONTE"= "Fonte do registro SINAN.",
    "SINAN_UF"   = "UF do dataset.",
    "SINAN_ANO"  = "Ano do dataset."
  ), sistema = "SINAN")

  dict_sinan <- rbind(
    dict_sinan_generic,
    dict_sinan_dengue,
    dict_sinan_acbi,
    dict_sinan_acgr,
    dict_sinan_aids,
    dict_sinan_animais,
    dict_sinan_extra
  )

  dict_sinan <- dict_sinan[!duplicated(dict_sinan$dbf), ]

  # ---- SINASC ----
  dict_sinasc <- data.frame(
    dbf = c(
      "CODESTAB","CODMUNNASC","LOCNASC","IDADEMAE","ESCMAE",
      "GESTACAO","PARTO","CONSULTAS","DTNASC","SEXO","PESO","APGAR1","APGAR5"
    ),
    description = c(
      "Código do estabelecimento (CNES).",
      "Município de nascimento.",
      "Local de nascimento.",
      "Idade da mãe.",
      "Escolaridade da mãe.",
      "Semanas de gestação.",
      "Tipo de parto.",
      "Consultas pré-natal.",
      "Data de nascimento.",
      "Sexo do nascido.",
      "Peso ao nascer.",
      "Apgar 1 minuto.",
      "Apgar 5 minutos."
    ),
    sistema = "SINASC",
    stringsAsFactors = FALSE
  )

  # ---- CIHA ----
  dict_ciha <- data.frame(
    dbf = c(
      "ANO_CMPT","MES_CMPT","FONTE","CNES","NATUREZA","GESTAO",
      "DT_ATEND","DT_SAIDA","DIAS_PERM","COBRANCA","MORTE",
      "MODALIDADE","MUNIC_MOV","MUNIC_RES","SEXO","COD_IDADE",
      "QT_PROC","UTI_INT_TO"
    ),
    description = c(
      "Ano da competência.",
      "Mês da competência.",
      "Fonte da remuneração.",
      "Código CNES do estabelecimento.",
      "Natureza do estabelecimento.",
      "Gestão do estabelecimento.",
      "Data de atendimento.",
      "Data de saída.",
      "Dias de permanência.",
      "Motivo de saída/permanência.",
      "Óbito.",
      "Modalidade de atendimento.",
      "Município de atendimento.",
      "Município de residência.",
      "Sexo.",
      "Código/faixa etária.",
      "Quantidade de procedimentos.",
      "Diárias de UTI intermediária."
    ),
    sistema = "CIHA",
    stringsAsFactors = FALSE
  )

  # ---- SIA / APAC ----
  dict_sia_apac <- data.frame(
    dbf = c(
      "AP_MVM","AP_CONDIC","AP_GESTAO","AP_CODUNI","AP_AUTORIZ","AP_CMP",
      "AP_PRIPAL","AP_VL_AP","AP_UFMUN","AP_TPUPS","AP_TIPPRE","AP_MN_IND",
      "AP_CNPJCPF","AP_CNPJMNT","AP_CNSPCN","AP_COIDADE","AP_NUIDADE",
      "AP_SEXO","AP_RACACOR","AP_MUNPCN","AP_UFNACIO","AP_CEPPCN",
      "AP_UFDIF","AP_MNDIF","AP_DTINIC","AP_DTFIM","AP_TPATEN","AP_TPAPAC",
      "AP_MOTSAI","AP_OBITO","AP_ENCERR","AP_PERMAN","AP_ALTA","AP_TRANSF",
      "AP_DTOCOR","AP_CODEMI","AP_CATEND","AP_APACANT","AP_UNISOL",
      "AP_DTSOLIC","AP_DTAUT","AP_CIDCAS","AP_CIDPRI","AP_CIDSEC","AP_ETNIA",
      "AQ_CID10","AQ_LINFIN","AQ_ESTADI","AQ_GRAHIS","AQ_DTIDEN","AQ_TRANTE",
      "AQ_CIDINI1","AQ_DTINI1","AQ_CIDINI2","AQ_DTINI2","AQ_CIDINI3",
      "AQ_DTINI3","AQ_CONTTR","AQ_DTINTR","AQ_ESQU_P1","AQ_TOTMPL",
      "AQ_TOTMAU","AQ_ESQU_P2"
    ),
    description = c(
      "Movimento/processamento da APAC.",
      "Condição principal da APAC.",
      "Tipo de gestão do estabelecimento.",
      "Código CNES da unidade executante.",
      "Número da autorização APAC.",
      "Competência da APAC.",
      "Indicador de procedimento principal.",
      "Valor total aprovado da APAC.",
      "UF e município da unidade prestadora.",
      "Tipo de unidade prestadora.",
      "Tipo de prestador.",
      "Município do indivíduo/atendimento.",
      "CPF ou CNPJ relacionado ao registro.",
      "CNPJ da mantenedora.",
      "Cartão Nacional de Saúde do paciente.",
      "Código do tipo/faixa etária.",
      "Idade numérica do paciente.",
      "Sexo do paciente.",
      "Raça/cor do paciente.",
      "Município de residência do paciente.",
      "UF de nacionalidade/nascimento.",
      "CEP do paciente.",
      "UF de referência/diferença de atendimento.",
      "Município de referência/diferença de atendimento.",
      "Data de início da APAC/tratamento.",
      "Data de fim da APAC/tratamento.",
      "Tipo de atendimento.",
      "Tipo de APAC.",
      "Motivo de saída/encerramento.",
      "Indica ocorrência de óbito.",
      "Indica encerramento da APAC.",
      "Indica permanência do paciente em acompanhamento.",
      "Indica alta.",
      "Indica transferência.",
      "Data de ocorrência.",
      "Código de emissão.",
      "Categoria de atendimento.",
      "Número da APAC anterior.",
      "Código da unidade solicitante.",
      "Data da solicitação.",
      "Data da autorização.",
      "CID associado ao caso.",
      "CID principal.",
      "CID secundário.",
      "Etnia do paciente.",
      "CID-10 do caso/APAC.",
      "Linha de financiamento.",
      "Estadiamento clínico/patológico.",
      "Grau histológico.",
      "Data de identificação diagnóstica.",
      "Tratamento anterior.",
      "CID inicial 1.",
      "Data associada ao CID inicial 1.",
      "CID inicial 2.",
      "Data associada ao CID inicial 2.",
      "CID inicial 3.",
      "Data associada ao CID inicial 3.",
      "Controle/continuidade do tratamento.",
      "Data de interrupção do tratamento.",
      "Esquema terapêutico principal 1.",
      "Total de meses/parcelas planejadas ou liberadas.",
      "Total de meses/autorizações aprovadas.",
      "Esquema terapêutico principal 2."
    ),
    sistema = "SIA_APAC",
    stringsAsFactors = FALSE
  )

  # ---- Base comum CNES ----
  dict_cnes_base <- data.frame(
    dbf = c(
      "COMPETEN","TPGESTAO","NIV_DEP","PF_PJ","CLIENTEL","NATUREZA","NAT_JUR",
      "RETENCAO","COD_IR","NIV_HIER","ESFERA_A","ATIVIDAD","TP_UNID",
      "TURNO_AT","TP_PREST","VINC_SUS","CODUFMUN","MAPORTAR","CMPT_INI",
      "CMPT_FIM","SGRUPHAB"
    ),
    description = c(
      "Mês de competência.",
      "Tipo de gestão.",
      "Mantida/individual.",
      "Pessoa física/jurídica.",
      "Fluxo de clientela.",
      "Natureza da organização.",
      "Natureza jurídica.",
      "Retenção tributária do estabelecimento.",
      "Retenção tributária da mantenedora.",
      "Nível de hierarquia.",
      "Esfera administrativa.",
      "Atividade de ensino/pesquisa.",
      "Tipo de estabelecimento.",
      "Turno de atendimento.",
      "Tipo de prestador.",
      "Vínculo com o SUS.",
      "Código de localização do estabelecimento.",
      "Mês de publicação da portaria.",
      "Competência de início.",
      "Competência de fim.",
      "Grupo/código de habilitação ou classificação."
    ),
    stringsAsFactors = FALSE
  )

  dict_cnes_dc <- rbind(
    transform(dict_cnes_base, sistema = "CNES_DC"),
    data.frame(
      dbf = c(
        "AP01CV01","AP02CV01","AP03CV01","AP04CV01","AP05CV01","AP06CV01","AP07CV01",
        "GESPRG4E","GESPRG4M","GESPRG6E","GESPRG6M"
      ),
      description = c(
        "Atendimento prestado: internação/SUS.",
        "Atendimento prestado: ambulatório/SUS.",
        "Atendimento prestado: SADT/SUS.",
        "Atendimento prestado: urgência/SUS.",
        "Atendimento prestado: outros/SUS.",
        "Atendimento prestado: vigilância/SUS.",
        "Atendimento prestado: regulação/SUS.",
        "Nível de atenção ambulatorial alta complexidade estadual.",
        "Nível de atenção ambulatorial alta complexidade municipal.",
        "Nível de atenção hospitalar alta complexidade estadual.",
        "Nível de atenção hospitalar alta complexidade municipal."
      ),
      sistema = "CNES_DC",
      stringsAsFactors = FALSE
    )
  )

  dict_cnes_eq <- rbind(
    transform(dict_cnes_base, sistema = "CNES_EQ"),
    data.frame(
      dbf = c("TIPEQUIP","QT_EXIST","QT_USO","IND_SUS","IND_NSUS"),
      description = c(
        "Tipo de equipamento.",
        "Quantidade de equipamentos existentes.",
        "Quantidade de equipamentos em uso.",
        "Usa equipamento para o SUS.",
        "Não usa equipamento para o SUS."
      ),
      sistema = "CNES_EQ",
      stringsAsFactors = FALSE
    )
  )

  dict_cnes_ep <- data.frame(
    dbf = c(
      "QUILOMBO","ASSENTAD","POPGERAL","ESCOLA","INDIGENA","PRONASCI",
      "DT_ATIVA","DT_DESAT","MOTDESAT","TP_DESAT","TIPO_EQP","TIPOSEGM",
      "IDEQUIPE","ID_AREA","ID_SEGM"
    ),
    description = c(
      "Atende quilombolas.",
      "Atende assentados.",
      "Atende população geral.",
      "Atende escola.",
      "Atende indígenas.",
      "Atende PRONASCI.",
      "Mês/data de ativação.",
      "Mês/data de desativação.",
      "Motivo de desativação.",
      "Tipo de desativação.",
      "Tipo da equipe.",
      "Tipo do segmento.",
      "Código da equipe.",
      "Código da área.",
      "Código do segmento."
    ),
    sistema = "CNES_EP",
    stringsAsFactors = FALSE
  )

  dict_cnes_ee <- rbind(
    transform(dict_cnes_base, sistema = "CNES_EE"),
    data.frame(
      dbf = "SGRUPHAB",
      description = "Estabelecimento de ensino.",
      sistema = "CNES_EE",
      stringsAsFactors = FALSE
    )
  )

  dict_cnes_ef <- rbind(
    transform(dict_cnes_base, sistema = "CNES_EF"),
    data.frame(
      dbf = "SGRUPHAB",
      description = "Estabelecimento filantrópico.",
      sistema = "CNES_EF",
      stringsAsFactors = FALSE
    )
  )

  dict_cnes_st <- rbind(
    transform(dict_cnes_base, sistema = "CNES_ST"),
    data.frame(
      dbf = c("DT_EXPED","ORGEXPED","AV_ACRED","CLASAVAL"),
      description = c(
        "Mês/ano de expedição do alvará.",
        "Órgão de expedição do alvará.",
        "Avaliação de acreditação.",
        "Classe da avaliação."
      ),
      sistema = "CNES_ST",
      stringsAsFactors = FALSE
    )
  )

  dict_cnes_gm <- rbind(
    transform(dict_cnes_base, sistema = "CNES_GM"),
    data.frame(
      dbf = "SGRUPHAB",
      description = "Gestão de metas.",
      sistema = "CNES_GM",
      stringsAsFactors = FALSE
    )
  )

  dict_cnes_hb <- rbind(
    transform(dict_cnes_base, sistema = "CNES_HB"),
    data.frame(
      dbf = c("SGRUPHAB","NULEITOS"),
      description = c(
        "Código da habilitação.",
        "Número de leitos."
      ),
      sistema = "CNES_HB",
      stringsAsFactors = FALSE
    )
  )

  dict_cnes_in <- rbind(
    transform(dict_cnes_base, sistema = "CNES_IN"),
    data.frame(
      dbf = "SGRUPHAB",
      description = "Incentivo do estabelecimento.",
      sistema = "CNES_IN",
      stringsAsFactors = FALSE
    )
  )

  dict_cnes_lt <- rbind(
    transform(dict_cnes_base, sistema = "CNES_LT"),
    data.frame(
      dbf = c("QT_EXIST","QT_SUS","QT_NSUS","TP_LEITO","CODLEITO"),
      description = c(
        "Quantidade de leitos existentes.",
        "Quantidade de leitos SUS.",
        "Quantidade de leitos não SUS.",
        "Tipo de leito.",
        "Especialidade/código do leito."
      ),
      sistema = "CNES_LT",
      stringsAsFactors = FALSE
    )
  )

  dict_siscolo <- data.frame(
    dbf = c(
      "CO_US","CO_US_UF","CO_US_IBGE","REGUS","CO_PAC_IBG","CO_PAC_UF","CO_PAC_ESC",
      "DT_PAC_NAS","CO_PAC_IDA","REGRESID","CO_CNES","CLABIBGE","CLABUF","CLABCID",
      "REGLAB","DT_ID_COMP","ANO_COMP","CO_FX_ETAR","CO_PAC_RAC","CO_CIT_ESC",
      "CO_CIT_GLA","CO_CIT_IND","O_CIT_ESCA","O_CIT_GLAN","CO_COLP","CO_RES_TIP",
      "CO_RES_LOC","CO_BEN_CER","CO_BEN_ALT","CO_NEO_NIC","CO_NEO_ADE","CO_DIF_GRA",
      "CO_EXTEN_P","CO_EXTEN_V","O_EXTEN_PE","O_EXTEN_PA","CO_EXTEN_C","O_EXTEN_VA",
      "CO_MARG","CO_RES_FRA","CO_RES_TAM","O_RES_TAM2","CO_RES_MAR","DT_HIS_EXA",
      "DT_HIS_REC","CO_MAT_INS","CO_CTRL_FR","CO_CTRL_BL","QTDEXA","DINTCOLETA",
      "DINTRESULT","DINTTEMPEX","ESCAMOSA","PNEOPLA","GPNNEOP","GNALTGRAU","INDPNAONEO",
      "CITINDALGR","ESCLIPBX","ESCINALT","ESCIEMCINV","ESCCARINVA","ADENOINSUT",
      "ADENCARCIN","CIT_OUTNEO","CCOLDEFAUL","CCOLNORMAL","CCOLANORMA","CCOLINSATI",
      "CO_COLP_PO","O_COLP_POS","CO_COLP_BI","CO_COLP_CU","CO_COLP_EX","CO_COLP_RE",
      "O_COLP_BIO","CRESBIOPSI","CRESCONIZA","CRESHISSIM","CRESPANHIS","CRESOUTROS",
      "CRESECTO","CRESENDO","CRESJUNC","CO_BEN_MET","CO_BEN_POL","CBENCERVI",
      "CBENALTER","NICDLEVE","NICDMODE","NICDACENT","NICCARCMIN","NICEPIINVA",
      "NICIMPAVAL","NICVERRUCO","NICNAOCERA","NEOINSUTU","NEOMUCINO","NEOVILOGL",
      "DS_NEO_OUT","CDIFGRAU","CDIFMODDIF","CDIFPOUDIF","CDIFINDFER","CDIEXAMINS",
      "C_EXT_VASC","C_EXT_PERI","C_EXT_PARA","C_EXT_CORP","C_EXT_VAGI","CO_EXTEN_L",
      "O_EXTEN_LI","CMARLIVRE","CMARCOMPRO","CMARIMPAVA"
    ),
    description = c(
      "Código da unidade de saúde.",
      "UF da unidade de saúde.",
      "Código IBGE da unidade de saúde.",
      "Regional da unidade de saúde.",
      "Código IBGE do município da paciente.",
      "UF da paciente.",
      "Escolaridade da paciente.",
      "Data de nascimento da paciente.",
      "Idade da paciente.",
      "Regional de residência.",
      "Código CNES do estabelecimento.",
      "Código IBGE do laboratório.",
      "UF do laboratório.",
      "Município/cidade do laboratório.",
      "Regional do laboratório.",
      "Data de identificação da competência.",
      "Ano da competência.",
      "Faixa etária.",
      "Raça/cor da paciente.",
      "Epitélio escamoso na citologia.",
      "Epitélio glandular na citologia.",
      "Alteração citológica/indicação.",
      "Outros achados escamosos.",
      "Outros achados glandulares.",
      "Resultado da colposcopia.",
      "Tipo de resultado/resposta.",
      "Localização do resultado.",
      "Lesão benigna do colo.",
      "Outras alterações benignas.",
      "Neoplasia intraepitelial cervical.",
      "Alteração/neoplasia glandular.",
      "Grau de diferenciação.",
      "Extensão para parede pélvica.",
      "Extensão para vagina.",
      "Outras extensões para pelve.",
      "Outras extensões para paramétrio.",
      "Extensão para corpo uterino/colo.",
      "Outras extensões vaginais.",
      "Margens do material.",
      "Fragmentação do material.",
      "Tamanho da lesão/material.",
      "Outro tamanho do material.",
      "Margem comprometida/livre.",
      "Data do exame histopatológico.",
      "Data do recebimento do material.",
      "Material insuficiente.",
      "Controle de formulário.",
      "Controle de bloco/lâmina.",
      "Quantidade de exames.",
      "Intervalo entre coleta e exame.",
      "Intervalo entre exame e resultado.",
      "Intervalo/tempo de expedição.",
      "Alteração escamosa.",
      "Presença de neoplasia.",
      "Grau de neoplasia não epitelial.",
      "Grau elevado de alteração.",
      "Indicação de processo não neoplásico.",
      "Indícios de alteração glandular.",
      "Lesão intraepitelial de baixo grau.",
      "Lesão intraepitelial de alto grau.",
      "Escamosa microinvasora.",
      "Carcinoma escamoso invasor.",
      "Adenocarcinoma in situ.",
      "Adenocarcinoma invasor.",
      "Outras neoplasias citológicas.",
      "Colposcopia default/sem informação.",
      "Colposcopia normal.",
      "Colposcopia anormal.",
      "Colposcopia insatisfatória.",
      "Conduta pós-colposcopia.",
      "Outra conduta pós-colposcopia.",
      "Indicação de biópsia.",
      "Curetagem uterina.",
      "Exérese/conduta excisional.",
      "Repetir exame/seguimento.",
      "Outra conduta de biópsia.",
      "Resultado de biópsia.",
      "Resultado de conização.",
      "Resultado histológico simplificado.",
      "Resultado anatomopatológico/histológico.",
      "Outros resultados histológicos.",
      "Comprometimento do ectocérvice.",
      "Comprometimento do endocérvice.",
      "Comprometimento da junção escamocolunar.",
      "Metaplasia benigna.",
      "Pólipo benigno.",
      "Benigno de colo do útero.",
      "Outras alterações benignas cervicais.",
      "NIC grau leve.",
      "NIC grau moderado.",
      "NIC acentuado/severo.",
      "Carcinoma microinvasor.",
      "Neoplasia epitelial invasora.",
      "Achado impossível de avaliar.",
      "Achado verrucoso.",
      "NIC não categorizada.",
      "Neoplasia insuficientemente especificada.",
      "Neoplasia mucinosa.",
      "Neoplasia viloglandular.",
      "Descrição de outra neoplasia.",
      "Diferenciação tumoral.",
      "Moderadamente diferenciado.",
      "Pouco diferenciado.",
      "Indiferenciado.",
      "Exame insuficiente.",
      "Extensão vascular.",
      "Extensão perineural/peri.",
      "Extensão parametrial.",
      "Extensão para corpo uterino.",
      "Extensão vaginal.",
      "Extensão local.",
      "Outra extensão local.",
      "Margem livre.",
      "Margem comprometida.",
      "Margem impossível de avaliar."
    ),
    sistema = "SISCOLO",
    stringsAsFactors = FALSE
  )

  dict_sismama <- data.frame(
    dbf = c(
      "CO_PAC_UF","CO_US_UF","CO_PAC_IBG","CO_US_IBGE","CO_US","CO_CNES",
      "PRESTUF","REGUS","REGRESID","REGLAB","ANO_COMP","PRESTMUN","DT_ID_COMP",
      "CO_FX_ETAR","CO_PAC_ESC","CO_PAC_SEX","CO_PAC_RAC","CANMPACANC",
      "CO_CLI_DES","CO_CLI_NOD","CCLIMATMAM","CCLIMATDPC","DINTCOLETA",
      "DINTRESULT","DINTTEMPEX","CO_RES_ADE","PRESULPAAF","CRESBENIG",
      "CRESMALIIN","CRESSUSMAL","CRESPOSMAL","CRESDERPAP","QUANTEXAME",
      "PBENMASTIT","PBENABSUBA","PBENFIBROA","PBENNECGOR","PBENCONDFI",
      "PBENLESEPI","PBENOUTRAS","PMALINTUMP","PMALINTUMF","PMALINOUTR",
      "PSUSLEJPCA","PSUSOUTROS","PPOSMACDUC","PPOSMACLOB","PPOSMACOUT",
      "DEMATACELU","DENEGMALIG","DEMALIINDT","DEPOSMALIG","DELESMALIG",
      "DEPROCINFL"
    ),
    description = c(
      "UF da paciente.",
      "UF da unidade de saúde.",
      "Código IBGE do município da paciente.",
      "Código IBGE da unidade de saúde.",
      "Código da unidade de saúde.",
      "Código CNES do estabelecimento.",
      "UF do prestador.",
      "Regional da unidade de saúde.",
      "Regional de residência.",
      "Regional do laboratório.",
      "Ano da competência.",
      "Município do prestador.",
      "Data de identificação da competência.",
      "Faixa etária.",
      "Escolaridade da paciente.",
      "Sexo da paciente.",
      "Raça/cor da paciente.",
      "Indicador de câncer de mama na paciente.",
      "Classificação clínica descritiva.",
      "Classificação clínica de nódulo.",
      "Achado clínico da mama.",
      "Achado clínico de descarga papilar.",
      "Intervalo entre coleta e exame.",
      "Intervalo entre exame e resultado.",
      "Intervalo/tempo de expedição.",
      "Adequabilidade do resultado.",
      "Presença de resultado sugestivo em PAAF.",
      "Resultado benigno.",
      "Resultado maligno inespecífico.",
      "Resultado suspeito de malignidade.",
      "Resultado possivelmente maligno.",
      "Resultado dermatológico/papilar.",
      "Quantidade de exames.",
      "Achado benigno: mastite.",
      "Achado benigno: abscesso/subagudo.",
      "Achado benigno: fibroadenoma.",
      "Achado benigno: necrose gordurosa.",
      "Achado benigno: condição fibrocística.",
      "Achado benigno: lesão epitelial.",
      "Outras alterações benignas.",
      "Achado maligno: tumor primário.",
      "Achado maligno: tumor filoide/foliar.",
      "Outras alterações malignas.",
      "Achado suspeito: lesão papilar.",
      "Outros achados suspeitos.",
      "Achado pós-maligno: carcinoma ductal.",
      "Achado pós-maligno: carcinoma lobular.",
      "Outros achados pós-malignos.",
      "Descrição macroscópica/celular.",
      "Descrição negativa para malignidade.",
      "Descrição maligna indeterminada.",
      "Descrição positiva para malignidade.",
      "Descrição de lesão maligna.",
      "Descrição de processo inflamatório."
    ),
    sistema = "SISMAMA",
    stringsAsFactors = FALSE
  )

  dict_cnes_pf <- rbind(
    transform(dict_cnes_base, sistema = "CNES_PF"),
    data.frame(
      dbf = c(
        "CPFUNICO","PROF_SUS","PROFNSUS","VINCUL_C","VINCUL_A","VINCUL_N",
        "HORAHOSP","HORA_AMB","HORAOUTR","VINCULAC","CONSELHO","CBOUNICO","CBO"
      ),
      description = c(
        "CPF único do profissional.",
        "Profissional atende SUS.",
        "Profissional não atende SUS.",
        "Profissional com contrato SUS.",
        "Profissional autônomo SUS.",
        "Vínculo não identificado.",
        "Horas hospitalares.",
        "Horas ambulatoriais.",
        "Outras horas trabalhadas.",
        "Vínculo empregatício.",
        "Conselho profissional.",
        "CBO único.",
        "CBO do profissional."
      ),
      sistema = "CNES_PF",
      stringsAsFactors = FALSE
    )
  )

  dict_cnes_rc <- rbind(
    transform(dict_cnes_base, sistema = "CNES_RC"),
    data.frame(
      dbf = c("DS_REGRA","SGRUPHAB"),
      description = c(
        "Descrição da regra contratual.",
        "Regras contratuais."
      ),
      sistema = "CNES_RC",
      stringsAsFactors = FALSE
    )
  )

  dict_cnes_sr <- rbind(
    transform(dict_cnes_base, sistema = "CNES_SR"),
    data.frame(
      dbf = c("CONTSRVU","SRVUNICO","SERV_ESP","CARACTER","AMB_NSUS"),
      description = c(
        "Quantidade de serviço especializado.",
        "Serviço especializado único.",
        "Serviço/classificação.",
        "Características do serviço.",
        "Ambulatorial/hospitalar."
      ),
      sistema = "CNES_SR",
      stringsAsFactors = FALSE
    )
  )

  # ---- PRÉ-NATAL / GESTANTE ----
  dict_prenatal <- data.frame(
    dbf = c(
      "NU_ANO_GES","DT_ATEND","CO_UF_IBGE","NU_GESTA","CO_GESTANT","CO_PAIS",
      "NU_CNS","NU_IDADE","NU_NIS","NU_NIS_REP","DS_ZONA",
      "QT_AB_ECT","QT_AB_GER","QT_AB_MOL","QT_MOR_APS","QT_MOR_PS",
      "QT_NSC_MOR","QT_NSC_VIV","QT_PRT_CIR","QT_PRT_FOR","QT_PRT_VAG",
      "ST_GRA_ANT","ST_GRA_PLA","DT_DUM","DT_DPP",
      "CO_TPO_GRA","CO_ESC_GP","CO_RAC_GP","CO_ETN_GP","CO_STF_GP","QT_CONS","QT_CONSULT",
      "ST_GESTAC", "CO_MUN_UBS", "DT_INC"
    ),
    description = c(
      "Ano da gestação.",
      "Data da consulta pré-natal ou atendimento médico.",
      "Código da UF conforme IBGE.",
      "Número de gestações.",
      "Classificação da gestante (ex: alto risco).",
      "Código do país.",
      "Número do Cartão Nacional de Saúde.",
      "Idade da gestante.",
      "Número de Identificação Social (NIS/PIS/PASEP).",
      "NIS repetido ou validado.",
      "Zona de residência (urbana/rural).",
      "Número de gestações ectópicas.",
      "Total de abortos.",
      "Número de gestações molares.",
      "Óbitos fetais antes de 22 semanas.",
      "Óbitos fetais após 22 semanas.",
      "Óbitos de recém-nascidos.",
      "Número de nascidos vivos.",
      "Partos cesáreos.",
      "Partos com fórceps.",
      "Partos vaginais.",
      "Situação de gestações anteriores.",
      "Gravidez planejada ou não.",
      "Data da última menstruação.",
      "Data provável do parto.",
      "Tipo de gravidez (única/múltipla).",
      "Escolaridade da gestante.",
      "Raça da gestante.",
      "Código de etnia.",
      "Classificação socioeconômica ou condição de saúde.",
      "Quantidade de  Consultas",
      "Quantidade de  Consultas ( 001-005 =  5 consultas ), (006-099 =  6 ou mais consultas)",
      "Status da Gestação",
      "Município da UBS",
      "Mês de Inclusão"
    ),
    sistema = "PRENATAL",
    stringsAsFactors = FALSE
  )

  dict_sim_extra <- data.frame(
    dbf = c(
      "CONTADOR","CARTORIO","REGISTRO","DATAREG","TIPOBITO","DATAOBITO",
      "ESTCIVIL","DATANASC","CODIGO","MUNIOCOR","MUNIRES","BAIRES","AREARES",
      "OCUPACAO","NATURAL","INSTRUCAO","OCUPPAI","INSTRPAI","OCUPMAE",
      "INSTRMAE","FILHVIVOS","FILHMORT","SEMANGEST","TIPOGRAV","TIPOPARTO",
      "PESONASC","ASSISTMED","ATESTANTE","EXAME","CIRURGIA","NECROPSIA",
      "OBITOFE1","OBITOFE2","CAUSABAS","TIPOVIOL","TIPOACID","FONTINFO",
      "ACIDTRAB","LOCACID","CRITICA","NUMEXPORT","CRSOCOR","CRSRES","UFINFORM"
    ),
    description = c(
      "Número sequencial do registro no sistema.",
      "Código ou identificação do cartório de registro civil.",
      "Número do registro do óbito no cartório.",
      "Data de registro do óbito no cartório.",
      "Tipo de óbito (fetal, não fetal).",
      "Data de ocorrência do óbito.",
      "Estado civil do falecido.",
      "Data de nascimento do indivíduo.",
      "Código identificador do registro de óbito.",
      "Município onde ocorreu o óbito.",
      "Município de residência do falecido.",
      "Bairro de residência do falecido.",
      "Área de residência (urbana/rural).",
      "Ocupação do falecido.",
      "Naturalidade (município/UF de nascimento).",
      "Escolaridade do falecido.",
      "Ocupação do pai.",
      "Escolaridade do pai.",
      "Ocupação da mãe.",
      "Escolaridade da mãe.",
      "Número de filhos vivos da mãe.",
      "Número de filhos mortos da mãe.",
      "Semanas de gestação.",
      "Tipo de gravidez (única, múltipla).",
      "Tipo de parto (normal, cesáreo, etc.).",
      "Peso ao nascer (gramas).",
      "Indica se houve assistência médica no óbito.",
      "Tipo de profissional que atestou o óbito.",
      "Indica se exames complementares foram realizados.",
      "Indica se houve cirurgia relacionada ao óbito.",
      "Indica se foi realizada necropsia (autópsia).",
      "Óbito fetal (indicador 1).",
      "Óbito fetal (indicador 2).",
      "Causa básica do óbito (CID-10).",
      "Tipo de violência (se aplicável).",
      "Tipo de acidente (se aplicável).",
      "Fonte da informação do óbito.",
      "Indica se o acidente foi relacionado ao trabalho.",
      "Local onde ocorreu o acidente.",
      "Indicador de crítica/validação do registro.",
      "Número de exportação do registro.",
      "Código da região de saúde do local de ocorrência.",
      "Código da região de saúde de residência.",
      "UF responsável pela informação."
    ),
    sistema = "SIM",
    stringsAsFactors = FALSE
  )

  dict_sinasc_extra <- data.frame(
    dbf = c(
      "ORIGEM","ESTCIVMAE","CODOCUPMAE","QTDFILVIVO","QTDFILMORT",
      "GRAVIDEZ","HORANASC","IDANOMAL","DTCADASTRO","CODANOMAL",
      "NUMEROLOTE","VERSAOSIST","DTRECEBIM","DIFDATA","DTRECORIGA",
      "NATURALMAE","CODMUNNATU","CODUFNATU","ESCMAE2010","SERIESCMAE",
      "DTNASCMAE","RACACORMAE","QTDGESTANT","QTDPARTNOR","QTDPARTCES",
      "IDADEPAI","DTULTMENST","SEMAGESTAC","TPMETESTIM","CONSPRENAT",
      "MESPRENAT","TPAPRESENT","STTRABPART","STCESPARTO","TPNASCASSI",
      "TPFUNCRESP","TPDOCRESP","DTDECLARAC","ESCMAEAGR1","STDNEPIDEM",
      "STDNNOVA","CODPAISRES","TPROBSON","PARIDADE","KOTELCHUCK"
    ),
    description = c(
      "Origem do registro no sistema.",
      "Estado civil da mãe.",
      "Código da ocupação da mãe.",
      "Quantidade de filhos vivos anteriores.",
      "Quantidade de filhos mortos anteriores.",
      "Tipo de gravidez (única, gemelar, múltipla).",
      "Hora do nascimento.",
      "Indica presença de anomalia congênita.",
      "Data de cadastro do registro.",
      "Código da anomalia congênita.",
      "Número do lote de processamento.",
      "Versão do sistema utilizada no registro.",
      "Data de recebimento da declaração.",
      "Diferença entre datas do processo/registro.",
      "Data de recebimento na origem.",
      "Naturalidade da mãe.",
      "Código do município de naturalidade da mãe.",
      "Código da UF de naturalidade da mãe.",
      "Escolaridade da mãe no padrão de 2010.",
      "Série escolar da mãe.",
      "Data de nascimento da mãe.",
      "Raça/cor da mãe.",
      "Quantidade de gestações anteriores.",
      "Quantidade de partos vaginais anteriores.",
      "Quantidade de partos cesáreos anteriores.",
      "Idade do pai.",
      "Data da última menstruação.",
      "Semanas de gestação calculadas.",
      "Tipo de método usado para estimar a idade gestacional.",
      "Número de consultas de pré-natal.",
      "Mês de início do pré-natal.",
      "Tipo de apresentação do recém-nascido no parto.",
      "Indica se houve trabalho de parto.",
      "Indica se o parto cesáreo ocorreu antes do trabalho de parto.",
      "Tipo de assistência ao nascimento.",
      "Tipo de função do responsável pelo preenchimento.",
      "Tipo de documento do responsável pelo preenchimento.",
      "Data de preenchimento/declaracão da DNV.",
      "Agrupamento da escolaridade da mãe.",
      "Status da DNV para uso epidemiológico.",
      "Status da DNV na base nova/atualizada.",
      "Código do país de residência.",
      "Classificação obstétrica de Robson.",
      "Paridade materna.",
      "Índice de adequação do pré-natal de Kotelchuck."
    ),
    sistema = "SINASC",
    stringsAsFactors = FALSE
  )

  dict_cnes_extra <- data.frame(
    dbf = c(
      "REGSAUDE","MICR_REG","DISTRSAN","DISTRADM",
      "CPF_CNPJ","CNPJ_MAN","TERCEIRO","CPF_PROF",
      "NOMEPROF","CNS_PROF","UFMUNRES"
    ),
    description = c(
      "Código da região de saúde onde o estabelecimento está localizado.",
      "Código da microrregião de saúde.",
      "Distrito sanitário do estabelecimento.",
      "Distrito administrativo do estabelecimento.",
      "CPF ou CNPJ do estabelecimento ou responsável.",
      "CNPJ da mantenedora do estabelecimento.",
      "Indica se o serviço é terceirizado.",
      "CPF do profissional de saúde.",
      "Nome completo do profissional de saúde.",
      "Cartão Nacional de Saúde (CNS) do profissional.",
      "Código da UF e município de residência."
    ),
    sistema = "CNES",
    stringsAsFactors = FALSE
  )

  dict_onco_extra <- data.frame(
    dbf = c(
      "ANO_DIAGN","ANOMES_DIA","ANO_TRATAM","ANOMES_TRA",
      "UF_RESID","MUN_RESID","UF_TRATAM","MUN_TRATAM",
      "UF_DIAGN","MUN_DIAG","TRATAMENTO","DIAGNOSTIC",
      "ESTADIAM","CNES_DIAG","CNES_TRAT","TEMPO_TRAT",
      "CNS_PAC","DIAG_DETH","DT_DIAG","DT_TRAT"
    ),
    description = c(
      "Ano do diagnóstico da doença.",
      "Ano e mês do diagnóstico (formato AAAAMM).",
      "Ano de início do tratamento.",
      "Ano e mês de início do tratamento (AAAAMM).",
      "UF de residência do paciente.",
      "Município de residência do paciente.",
      "UF onde o tratamento foi realizado.",
      "Município onde o tratamento foi realizado.",
      "UF onde foi realizado o diagnóstico.",
      "Município onde foi realizado o diagnóstico.",
      "Tipo de tratamento realizado (cirurgia, quimioterapia, radioterapia, etc.).",
      "Tipo ou classificação do diagnóstico (ex: neoplasia maligna).",
      "Estadiamento do tumor (ex: I, II, III, IV).",
      "Código CNES do estabelecimento onde foi realizado o diagnóstico.",
      "Código CNES do estabelecimento onde foi realizado o tratamento.",
      "Tempo entre diagnóstico e início do tratamento (em dias).",
      "Cartão Nacional de Saúde do paciente.",
      "Detalhamento do diagnóstico (CID ou descrição clínica).",
      "Data do diagnóstico.",
      "Data de início do tratamento."
    ),
    sistema = "ONCOLOGIA",
    stringsAsFactors = FALSE
  )

  dict_pce_extra <- data.frame(
    dbf = c(
      "ID_UF","ID_DISTR","ID_LOC","DT_COMP","QT_POP","QT_PRED",
      "QT_EXAM","QT_NRECOL","QT_1A4","QT_5A16","QT_17",
      "QT_POS","QT_ATRAT","QT_TRAT","QT_CI","QT_REC","QT_AUS",
      "QT_ASC","QT_ANC","QT_TAE","QT_TT","QT_EV","QT_SE","QT_HN",
      "QT_OUT","QT_CAP","QT_PESQ","QT_BGLA","QT_BSTR","QT_BTEN",
      "QT_OUT1","QT_POSBGLA","QT_POSBTEN","QT_POSBSTR","QT_POSOUT"
    ),
    description = c(
      "Código da Unidade Federativa (UF).",
      "Código do distrito sanitário.",
      "Código da localidade.",
      "Data de competência (ano/mês do registro).",
      "População total da área.",
      "População prevista para cobertura/ação.",
      "Quantidade de exames realizados.",
      "Número de indivíduos não recolhidos (ausentes).",
      "População examinada de 1 a 4 anos.",
      "População examinada de 5 a 16 anos.",
      "População examinada com 17 anos ou mais.",
      "Número de exames positivos.",
      "Número de indivíduos a tratar.",
      "Número de indivíduos tratados.",
      "Casos com carga parasitária intensa.",
      "Casos reavaliados/recidiva.",
      "Indivíduos ausentes no tratamento.",
      "Casos com Ascaris lumbricoides.",
      "Casos com ancilostomídeos.",
      "Casos com Taenia sp.",
      "Casos com Trichuris trichiura.",
      "Casos com Enterobius vermicularis.",
      "Casos com Schistosoma mansoni.",
      "Casos com Hymenolepis nana.",
      "Outros parasitos identificados.",
      "Casos captados (busca ativa).",
      "Indivíduos pesquisados/investigados.",
      "Casos de Giardia lamblia.",
      "Casos de Strongyloides stercoralis.",
      "Casos de Taenia (teníase).",
      "Outros parasitos (categoria adicional).",
      "Casos positivos para Giardia lamblia.",
      "Casos positivos para Taenia.",
      "Casos positivos para Strongyloides.",
      "Casos positivos para outros parasitos."
    ),
    sistema = "PCE",
    stringsAsFactors = FALSE
  )

  dict_resp_extra <- data.frame(
    dbf = c(
      "CO_SEQ","ANO_NOT","TP_NOTIFIC","DT_NASCMAE","IDADEGES",
      "REGIAORES","UFRES","ANO_NASC","COMPRIMENT","PERIMCEFAL",
      "DIAMCEFAL","MICROCEFAL","DEF_NEURO","DEF_AUDIT","DEF_VISUAL",
      "TPDETECCAO","GEST_DIAG","CLASS_FETO","DT_SINTOMA",
      "FEBRE_GES","EXANT_GES","PRURIDO","CONJUNTIV","DOR_ARTIC",
      "DOR_MUSC","EDEMA","CEFALEIA","HIPERT_GAN","NEUROLOG",
      "EXA_TORSCH","RESUL_S","RESUL_TO","RESUL_C","RESUL_H","RESUL_Z",
      "SO_IGG_Z","SO_IGM_Z","TR_IGG_Z","TR_IGM_Z","PCR_Z",
      "HIST_ARBOV","HIST_MALFO",
      "RNEXA_TORS","RNRESUL_S","RNRESUL_TO","RNRESUL_C","RNRESUL_H","RNRESUL_Z",
      "RNSO_IGG_Z","RNSO_IGM_Z","RNTR_IGG_Z","RNTR_IGM_Z","RNPCR_Z",
      "EXAME_USS","DT_USS","EXA_TRANSF","DT_TRANSF","EXAME_TC","DT_TC",
      "EXAME_RS","DT_RS","REGIAONOT","UFNOT","CODMUNNOT",
      "ST_OBITO","DT_OBITO","CLASSIFIN","ETIOLOGIA","CRITERIO",
      "STATUS_NOT","DT_ULT_ALT"
    ),
    description = c(
      "Identificador único do registro.",
      "Ano da notificação.",
      "Tipo de notificação (suspeita, confirmada, etc.).",
      "Data de nascimento da mãe.",
      "Idade gestacional no momento da avaliação.",
      "Região de residência.",
      "UF de residência.",
      "Ano de nascimento.",
      "Comprimento do recém-nascido (cm).",
      "Perímetro cefálico (cm).",
      "Diâmetro cefálico.",
      "Indica presença de microcefalia.",
      "Deficiência neurológica.",
      "Deficiência auditiva.",
      "Deficiência visual.",
      "Tipo de detecção do caso.",
      "Momento do diagnóstico durante a gestação.",
      "Classificação do feto/recém-nascido.",
      "Data de início dos sintomas.",
      "Febre durante a gestação.",
      "Exantema durante a gestação.",
      "Prurido (coceira).",
      "Conjuntivite.",
      "Dor articular.",
      "Dor muscular.",
      "Edema.",
      "Cefaleia.",
      "Hipertrofia ganglionar.",
      "Alterações neurológicas.",
      "Exames do tipo TORCH realizados.",
      "Resultado sorológico geral.",
      "Resultado toxoplasmose.",
      "Resultado citomegalovírus.",
      "Resultado herpes.",
      "Resultado Zika.",
      "Sorologia IgG para Zika (mãe).",
      "Sorologia IgM para Zika (mãe).",
      "Teste rápido IgG Zika.",
      "Teste rápido IgM Zika.",
      "PCR para Zika.",
      "Histórico de arboviroses.",
      "Histórico de malformações.",
      "Exames TORCH no recém-nascido.",
      "Resultado sorológico RN.",
      "Resultado toxoplasmose RN.",
      "Resultado citomegalovírus RN.",
      "Resultado herpes RN.",
      "Resultado Zika RN.",
      "Sorologia IgG Zika RN.",
      "Sorologia IgM Zika RN.",
      "Teste rápido IgG RN.",
      "Teste rápido IgM RN.",
      "PCR Zika RN.",
      "Exame de ultrassonografia.",
      "Data da ultrassonografia.",
      "Exame transfontanelar.",
      "Data do exame transfontanelar.",
      "Tomografia computadorizada.",
      "Data da tomografia.",
      "Ressonância magnética.",
      "Data da ressonância.",
      "Região de notificação.",
      "UF de notificação.",
      "Município de notificação.",
      "Indica ocorrência de óbito.",
      "Data do óbito.",
      "Classificação final do caso.",
      "Etiologia confirmada.",
      "Critério de confirmação.",
      "Status da notificação.",
      "Data da última atualização do registro."
    ),
    sistema = "RESP",
    stringsAsFactors = FALSE
  )

  dict_siscolo_extra <- data.frame(
    dbf = c(
      "CO_RES_NOR","CO_ATI_ESC","CO_ATI_GLA","CO_ATI_IND","CO_CEL_ESC","CO_CEL_GLA",
      "CO_NEO_MAL","CO_AMOSTRA","CO_ADEQ_MA","CO_BEN_INF","CO_BEN_REP","CO_BEN_ATR",
      "CO_BEN_RAD","CO_BEN_OUT","CO_MIC_LAC","CO_MIC_COC","CO_MIC_CHL","CO_MIC_ACT",
      "CO_MIC_BAC","CO_MIC_TRI","CO_MIC_HER","CO_MIC_CAN","CO_MIC_GAR","CO_MIC_OUT",
      "CO_ANM_PRE","DT_ANM_PRE","ST_MON_EXT","TEMPCITANT","REPREZT","RESNORM",
      "POSNAONEO","LESALTGRA","PSNAONPLA","NLSALTGRA","PSNNPLA","NALTGRA","LBGRAU",
      "LAGRAU","LAGRAUMI","CARCINO","C_EPI_ESCA","C_EPI_GLAN","CO_EPI_MET",
      "DENCARCINV","OUTNEO","ALTERADO","CVCIONAL","MEIOLIQU","AMOSREJAUS",
      "AMOSREJDAN","AMOSREJALH","AMOSREJOUT","REJEITADA","SATISFACT","INSATSFAC",
      "CO_INSA_AC","CO_INSA_SA","CO_INSA_PI","CO_INSA_AR","CO_INSA_CO","CO_INSA_SU",
      "CO_INSA_OU","CBEMINFLA","CBEMMETAS","CBEMREPAR","CBEMATROF","CBEMRADI",
      "CBEMOUT","CMICLACT","CMICCOCO","CMICCHLA","CMICACTI","CMICBACI",
      "CMICTRICO","CMICHERP","CMICCAND","CMICGARD","CMICOUT"
    ),
    description = c(
      "Resultado citológico dentro dos limites da normalidade.",
      "Alteração em células escamosas.",
      "Alteração em células glandulares.",
      "Alteração celular de origem indefinida.",
      "Presença de células escamosas na amostra.",
      "Presença de células glandulares na amostra.",
      "Indicação de neoplasia maligna.",
      "Tipo de amostra coletada.",
      "Adequabilidade do material/amostra.",
      "Presença de processo inflamatório benigno.",
      "Achado benigno reparativo.",
      "Achado benigno atrófico.",
      "Achado benigno relacionado à radiação.",
      "Outros achados benignos.",
      "Presença de Lactobacillus sp.",
      "Presença de cocos.",
      "Presença de Chlamydia sp.",
      "Presença de Actinomyces sp.",
      "Presença de bacilos.",
      "Presença de Trichomonas vaginalis.",
      "Presença de efeito citopático por herpes.",
      "Presença de Candida sp.",
      "Presença de Gardnerella vaginalis.",
      "Outros microrganismos identificados.",
      "Anomalia/alteração prévia registrada.",
      "Data da anomalia/alteração prévia.",
      "Status de monitoramento externo.",
      "Tempo de citologia anterior.",
      "Material representativo da zona de transformação.",
      "Resultado normal.",
      "Achado positivo para processo não neoplásico.",
      "Lesão de alto grau.",
      "Processo escamoso não neoplásico.",
      "Neoplasia/lesão escamosa de alto grau.",
      "Processo escamoso não neoplásico (variação de codificação).",
      "Neoplasia/alteração de alto grau.",
      "Lesão intraepitelial de baixo grau.",
      "Lesão intraepitelial de alto grau.",
      "Lesão intraepitelial de alto grau, não podendo excluir microinvasão.",
      "Carcinoma.",
      "Componente epitelial escamoso presente.",
      "Componente epitelial glandular presente.",
      "Presença de metaplasia epitelial.",
      "Adenocarcinoma invasor (variação/codificação complementar).",
      "Outras neoplasias.",
      "Exame alterado.",
      "Citologia convencional.",
      "Coleta em meio líquido.",
      "Amostra rejeitada por ausência de identificação/material.",
      "Amostra rejeitada por dano no material.",
      "Amostra rejeitada por lâmina/material inadequado.",
      "Amostra rejeitada por outros motivos.",
      "Indica amostra rejeitada.",
      "Amostra satisfatória para análise.",
      "Amostra insatisfatória para análise.",
      "Insatisfatória por ausência de células.",
      "Insatisfatória por secagem/artefato.",
      "Insatisfatória por processo inflamatório intenso.",
      "Insatisfatória por artefatos de leitura.",
      "Insatisfatória por material contaminado.",
      "Insatisfatória por sangue.",
      "Insatisfatória por outros motivos.",
      "Achado benigno inflamatório.",
      "Achado benigno metaplásico.",
      "Achado benigno reparativo.",
      "Achado benigno atrófico.",
      "Achado benigno por radiação.",
      "Outros achados benignos (codificação complementar).",
      "Microrganismo: Lactobacillus.",
      "Microrganismo: cocos.",
      "Microrganismo: Chlamydia.",
      "Microrganismo: Actinomyces.",
      "Microrganismo: bacilos.",
      "Microrganismo: Trichomonas.",
      "Microrganismo: herpes.",
      "Microrganismo: Candida.",
      "Microrganismo: Gardnerella.",
      "Outros microrganismos."
    ),
    sistema = "SISCOLO",
    stringsAsFactors = FALSE
  )

  ## e-sus

  dict_esus_extra <- make_dict_named(c(
    "ID_ESTRANG" = "Identificador de estrangeiro.",
    "COMU_TRAD"  = "Comunidade tradicional.",
    "SG_PAIS"    = "Sigla do país.",
    "ANO_DIAG"   = "Ano do diagnóstico.",
    "MO_SUSPEIT" = "Modo de suspeita/identificação do caso.",
    "UF_NASC"    = "UF de nascimento.",
    "MUN_NASC"   = "Município de nascimento.",

    "EIE_IGG"    = "Resultado de ELISA/ensaio imunoenzimático IgG.",
    "IFI_IGG"    = "Resultado de imunofluorescência indireta IgG.",
    "HAI_IGG"    = "Resultado de hemaglutinação indireta IgG.",
    "QUIMIO_IGG" = "Resultado de quimioluminescência IgG.",
    "PCR"        = "Resultado de PCR.",
    "OUTRO_POSI" = "Outro exame positivo.",

    "AC_NOT"     = "Acompanhamento/notificação atual.",
    "UF_UBS_AC"  = "UF da UBS de acompanhamento.",
    "MUN_UBS_AC" = "Município da UBS de acompanhamento.",
    "UBS_RES_AC" = "UBS responsável pelo acompanhamento.",

    "HOSP_ESP"   = "Indica acompanhamento/encaminhamento em hospital especializado.",
    "UF_HOSPESP" = "UF do hospital especializado.",
    "MUN_ESP"    = "Município do serviço especializado.",
    "NOME_ESP"   = "Nome do serviço/hospital especializado.",

    "ELETROCARD" = "Resultado/realização de eletrocardiograma.",
    "RX_TORAX"   = "Resultado/realização de raio-X de tórax.",
    "RX_COLON"   = "Resultado/realização de raio-X de cólon.",
    "RX_ESOFAGO" = "Resultado/realização de raio-X de esôfago.",
    "ECOCARDIO"  = "Resultado/realização de ecocardiograma.",
    "OUTRO_EXAM" = "Outro exame complementar.",

    "HIV"        = "Comorbidade: HIV.",
    "HIPERTEN"   = "Comorbidade: hipertensão.",
    "HEPATITE"   = "Comorbidade: hepatite.",
    "CARDIOPAT"  = "Comorbidade: cardiopatia.",
    "NEOPLASIA"  = "Comorbidade: neoplasia.",
    "LEISHMANIA" = "Comorbidade: leishmaniose.",
    "OUT_COMORB" = "Outras comorbidades.",

    "FORMA"      = "Forma clínica da doença.",
    "REATIVACAO" = "Indica reativação da doença.",

    "HIST_BNZ"   = "Histórico de tratamento com benznidazol.",
    "TRAT_BNZ"   = "Tratamento atual com benznidazol.",
    "BNZ_TOT_CP" = "Total de comprimidos de benznidazol.",
    "BNZ_DIAS"   = "Duração do tratamento com benznidazol (dias).",

    "TRAT_NFX"   = "Tratamento atual com nifurtimox.",
    "NFX_TOT_CP" = "Total de comprimidos de nifurtimox.",
    "NFX_DIAS"   = "Duração do tratamento com nifurtimox (dias).",

    "ADVERS_BNZ" = "Ocorrência de evento adverso com benznidazol.",
    "BNZ_LEVE"   = "Evento adverso leve com benznidazol.",
    "BNZ_GRAVE"  = "Evento adverso grave com benznidazol.",
    "BNZ_AUGESI" = "Evento adverso: ageusia/disgeusia com benznidazol.",
    "BNZ_PAREST" = "Evento adverso: parestesia com benznidazol.",
    "BNZ_DEPRE"  = "Evento adverso: depressão com benznidazol.",
    "BNZ_GASTRO" = "Evento adverso: alteração gastrointestinal com benznidazol.",
    "BNZ_ARTRAL" = "Evento adverso: artralgia com benznidazol.",
    "REAC_BNZ"   = "Reação adversa a benznidazol.",

    "ADVERS_NFX" = "Ocorrência de evento adverso com nifurtimox.",
    "NFX_LEVE"   = "Evento adverso leve com nifurtimox.",
    "NFX_GRAVE"  = "Evento adverso grave com nifurtimox.",
    "NFX_AGEUSI" = "Evento adverso: ageusia/disgeusia com nifurtimox.",
    "NFX_PAREST" = "Evento adverso: parestesia com nifurtimox.",
    "NFX_MEDULA" = "Evento adverso: alteração medular com nifurtimox.",
    "NFX_GASTRO" = "Evento adverso: alteração gastrointestinal com nifurtimox.",
    "NFX_ARTRAL" = "Evento adverso: artralgia com nifurtimox.",
    "REAC_NFX"   = "Reação adversa a nifurtimox.",

    "HIST_EPID"  = "Histórico epidemiológico.",
    "BUSCAATIVA" = "Caso identificado por busca ativa.",
    "DIAG_FAMIL" = "Diagnóstico em familiar.",
    "EXAM_FAMIL" = "Exame realizado em familiar.",
    "CONF_FAMIL" = "Confirmação em familiar.",

    "TF_RESIDEN" = "Telefone de residência.",
    "UF_RESI_TF" = "UF do local de residência informado para contato.",
    "MN_RESI_TF" = "Município de residência informado para contato.",

    "MUD_UBS_AC" = "Mudança de UBS de acompanhamento.",
    "UF_NOV_AC"  = "UF da nova unidade de acompanhamento.",
    "MUN_NOV_AC" = "Município da nova unidade de acompanhamento.",
    "NM_UBS_AC"  = "Nome da nova UBS de acompanhamento.",

    "NOVO_ESPEC" = "Novo serviço especializado.",
    "ANT_UF_ESP" = "UF do serviço especializado anterior.",
    "ANT_MUN"    = "Município do serviço especializado anterior.",
    "NM_ANT_AC"  = "Nome da unidade anterior de acompanhamento.",

    "ST_ENCERRA" = "Status de encerramento.",
    "SITUACAO"   = "Situação do registro/caso.",
    "DT_CRIACAO" = "Data de criação do registro.",

    "CD_MUNICIP" = "Código do município de notificação.",
    "CD_MN_RESI" = "Código do município de residência.",
    "CD_MUN_NAS" = "Código do município de nascimento.",
    "CD_COMUNIN" = "Código do município de infecção/comunicação.",
    "CD_MUN_UBS" = "Código do município da UBS.",
    "CD_MUN_ESP" = "Código do município do serviço especializado.",
    "CD_ANT_MUN" = "Código do município anterior."
  ), sistema = "e-SUS")
  # -----------------------------
  # 2. COMBINE ALL DICTIONARIES
  # -----------------------------
  dict_list <- list(
    dict_sih,
    dict_esus_extra,
    dict_sinan,
    dict_sim_extra,
    dict_sinasc,
    dict_ciha,
    dict_sia_apac,
    dict_siscolo,
    dict_prenatal,
    dict_sismama,
    dict_sinasc_extra,
    dict_pce_extra,
    dict_cnes_extra,
    dict_onco_extra,
    dict_resp_extra,
    dict_siscolo_extra,
    dict_cnes_dc,
    dict_cnes_eq,
    dict_cnes_ep,
    dict_cnes_ee,
    dict_cnes_ef,
    dict_cnes_st,
    dict_cnes_gm,
    dict_cnes_hb,
    dict_cnes_in,
    dict_cnes_lt,
    dict_cnes_pf,
    dict_cnes_rc,
    dict_cnes_sr
  )

  dict_list <- lapply(dict_list, normalize_dict)
  dict_all <- do.call(rbind, dict_list)

  # -----------------------------
  # 3. AUTO-DETECTION
  # -----------------------------
  if (sistema == "auto") {
    hits <- dict_all[dict_all$dbf %in% cols, , drop = FALSE]

    if (nrow(hits) == 0) {
      detected_system <- "desconhecido"
      dict_use <- dict_all
    } else {
      scores <- sort(table(hits$sistema), decreasing = TRUE)
      detected_system <- names(scores)[1]

      if (detected_system == "SISCOLO" &&
          any(cols %in% c("PBENFIBROA", "PMALINTUMP", "DEMATACELU"))) {
        detected_system <- "SISMAMA"
      }

      dict_use <- dict_all[dict_all$sistema == detected_system, , drop = FALSE]
      if (nrow(dict_use) == 0) {
        dict_use <- dict_all
      }
    }
  } else {
    detected_system <- sistema
    dict_use <- dict_all[dict_all$sistema == sistema, , drop = FALSE]
  }

  # -----------------------------
  # 4. REMOVE DUPLICATES ONLY IN ACTIVE DICTIONARY
  # -----------------------------
  dict_use <- dict_use[!duplicated(dict_use$dbf), , drop = FALSE]

  # -----------------------------
  # 5. MATCH AGAINST ACTIVE DICTIONARY
  # -----------------------------
  idx <- match(cols, dict_use$dbf)

  result <- data.frame(
    column = names(df),
    dbf = cols,
    description = dict_use$description[idx],
    sistema = dict_use$sistema[idx],
    stringsAsFactors = FALSE
  )

  # -----------------------------
  # 6. HANDLE UNMAPPED
  # -----------------------------
  result$description[is.na(result$description)] <- "Não mapeado"
  result$sistema[is.na(result$sistema)] <- if (sistema == "auto") "desconhecido" else sistema

  # -----------------------------
  # 7. ADD DETECTED SYSTEM (GLOBAL)
  # -----------------------------
  attr(result, "detected_system") <- detected_system

  return(result)
}







