#' Clean and Decode DATASUS Tables
#'
#' Applies standardized cleaning, decoding, and formatting to DATASUS datasets
#' using parquet-based dictionaries. The function automatically detects the
#' dataset group (e.g., SIM, SINASC, SIHSUS, SIASUS, CNES, etc.) and applies
#' appropriate transformations.
#'
#' This includes:
#' \itemize{
#'   \item Decoding categorical variables using lookup dictionaries
#'   \item Formatting date fields
#'   \item Normalizing identifiers (e.g., UF codes)
#'   \item Converting numeric fields
#'   \item Handling dataset-specific structures
#' }
#'
#' @param datafile A data.frame or data.table containing raw DATASUS data.
#' @param fonte Character string indicating the data source (e.g., "SIM-DO",
#'   "SINASC-DN", "SIHSUS-RD", "SINAN-DENGUE").
#' @param base_path Optional character string indicating the path to the
#'   parquet dictionary files. If NULL, the default cache directory created
#'   by \code{setup_data()} is used.
#' @param verbose Logical; if TRUE, prints processing messages.
#'
#' @return A cleaned data.frame or data.table with decoded variables and
#'   standardized formats.
#'
#' @details
#' The function relies on external parquet dictionary files previously downloaded
#' using \code{setup_data()}. If these files are not found, the function will stop
#' with an error.
#'
#' Internally, the function:
#' \itemize{
#'   \item Maps dataset-specific codes to human-readable labels
#'   \item Applies transformations based on dataset group
#'   \item Uses efficient lookup joins for decoding variables
#' }
#'
#' @examples
#' \dontrun{
#' library(megadatasus)
#'
#' # Load raw data (example)
#' df <- read.csv("data.csv")
#'
#' # Clean SIM dataset
#' clean_df <- clean_table(df, fonte = "SIM-DO")
#'
#' # Clean SINASC dataset
#' clean_df <- clean_table(df, fonte = "SINASC-DN")
#' }
#'
#' @seealso \code{\link{setup_data}}
#'
#' @export
clean_table <- function(datafile, fonte,
                        base_path = NULL,
                        verbose = TRUE) {

  # -------------------- Parquet base path --------------------
  get_parquet_path <- function() {
    path <- normalizePath(
      file.path(tools::R_user_dir("megadatasus", "cache"), "data"),
      winslash = "/",
      mustWork = FALSE
    )

    if (!file.exists(file.path(path, ".data_ready"))) {
      stop(
        "Parquet data not found.Run megadatasus::setup_data() first.",
        call. = FALSE
      )
    }

    path
  }

  if (is.null(base_path)) base_path <- get_parquet_path()
  if (isTRUE(verbose)) message("Using Parquet base path: ", base_path)

  # -------------------- packages --------------------
  pkgs <- c("dplyr", "rlang", "purrr", "arrow", "stringr", "tibble")
  miss <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(miss)) stop("Install required packages: ", paste(miss, collapse = ", "), call. = FALSE)

  `%>%`  <- dplyr::`%>%`
  `%||%` <- rlang::`%||%`

  msg <- function(...) if (isTRUE(verbose)) message(...)


  df <- tibble::as_tibble(datafile)   #
  fonte_in <- as.character(fonte)
  # -------------------- normalize "fonte" (group) --------------------
  fonte_grp <- dplyr::case_when(
    fonte_in %in% c("SIASUS-AB","SIASUS-ABO","SIASUS-ACF","SIASUS-AD","SIASUS-AM","SIASUS-SD",
                    "SIASUS-AQ","SIASUS-AN","SIASUS-AR","SIASUS-ATD","SIASUS-PA","SIASUS-PS") ~ "SIASUS",
    fonte_in %in% c("SIHSUS-ER","SIHSUS-RD","SIHSUS-RJ","SIHSUS-SP") ~ "SIHSUS",
    fonte_in %in% c("SISCOLO-CC","SISCOLO-HC") ~ "SISCOLO",
    fonte_in %in% c("SISMAMA-CM","SISMAMA-HC") ~ "SISMAMA",
    fonte_in == "CIH-CR" ~ "CIH",
    fonte_in == "CIHA" ~ "CIHA",
    fonte_in %in% c("IBGE-POP","IBGE-POPT") ~ "IBGE",
    fonte_in == "PO" ~ "PO",
    fonte_in %in% c("SIM-DO","SIM-DOEXT","SIM-DOFET","SIM-DOINF","SIM-DOMAT","SIM-DOREXT") ~ "SIM",
    fonte_in %in% c("SINASC-DN","SINASC-DNEX") ~ "SINASC",
    fonte_in == "RESP" ~ "RESP",
    fonte_in == "SISPRENATAL-PN" ~ "SISPRENATAL",
    fonte_in == "PCE" ~ "PCE",
    fonte_in %in% c("CNES-DC","CNES-EE","CNES-EF","CNES-EP","CNES-EQ","CNES-GM","CNES-HB",
                    "CNES-IN","CNES-LT","CNES-PF","CNES-RC","CNES-SR","CNES-ST") ~ "CNES",
    stringr::str_starts(fonte_in, "SINAN-") ~ "SINAN",
    TRUE ~ NA_character_
  )

  if (is.na(fonte_grp)) stop("Unknown fonte: ", fonte_in, call. = FALSE)
  msg("table_clean(): fonte = ", fonte_in, "  => grupo = ", fonte_grp)

  # ===================================== HELPERS ==========================================

  read_dicts <- function(files, base_path) {
    out <- lapply(files, function(f) {
      path <- file.path(base_path, paste0(f, ".parquet"))
      if (!file.exists(path)) return(NULL)

      tryCatch(
        arrow::read_parquet(path),
        error = function(e) {
          warning(sprintf("Error reading dictionary '%s': %s", f, e$message), call. = FALSE)
          NULL
        }
      )
    })
    names(out) <- files
    out
  }

  as_lookup <- function(x, code_col = "code", desc_col = "description") {
    if (is.null(x)) return(NULL)
    if (!all(c(code_col, desc_col) %in% names(x))) return(NULL)

    dplyr::tibble(
      code = as.character(x[[code_col]]),
      description = as.character(x[[desc_col]])
    )
  }

  make_uf_lookup <- function(uf_df) {
    if (is.null(uf_df)) return(NULL)
    nms <- names(uf_df)

    if (!("code" %in% nms)) return(NULL)

    label_col <- dplyr::case_when(
      "UF" %in% nms ~ "UF",
      "description" %in% nms ~ "description",
      "sigla" %in% nms ~ "sigla",
      TRUE ~ NA_character_
    )

    if (is.na(label_col)) return(NULL)

    dplyr::tibble(
      code = sprintf("%02d", suppressWarnings(as.integer(uf_df$code))),
      description = as.character(uf_df[[label_col]])
    )
  }

  apply_lookup <- function(df, col, lookup) {
    if (!col %in% names(df)) return(df)
    if (is.null(lookup)) return(df)
    if (!all(c("code", "description") %in% names(lookup))) return(df)

    codes <- as.character(lookup$code)
    desc  <- as.character(lookup$description)

    df %>%
      dplyr::mutate(
        !!rlang::sym(col) := {
          x <- as.character(.data[[col]])
          hit <- match(x, codes)
          out <- desc[hit]
          dplyr::if_else(!is.na(hit), out, x)
        }
      )
  }

  apply_lookups <- function(df, map) {
    purrr::reduce(names(map), function(acc, nm) apply_lookup(acc, nm, map[[nm]]), .init = df)
  }

  to_date_ymd8 <- function(df, cols, fmt = "%Y%m%d") {
    cols <- intersect(cols, names(df))
    if (!length(cols)) return(df)

    df[cols] <- lapply(df[cols], function(x) {
      x <- as.character(x)
      as.Date(substr(x, 1, 8), format = fmt)
    })

    df
  }

  format_nu_idade_n <- function(df) {
    if (!"NU_IDADE_N" %in% names(df)) return(df)

    x <- suppressWarnings(as.integer(df$NU_IDADE_N))
    df$NU_IDADE_N <- paste(
      x %% 1000,
      dplyr::case_when(
        floor(x / 1000) == 1 ~ "hora(s)",
        floor(x / 1000) == 2 ~ "dia(s)",
        floor(x / 1000) == 3 ~ "mes(es)",
        floor(x / 1000) == 4 ~ "ano(s)",
        TRUE ~ NA_character_
      )
    )

    df
  }

  normalize_uf_code <- function(x) {
    x <- as.character(x)
    x <- stringr::str_trim(x)
    x <- stringr::str_replace_all(x, "\\.0$", "")
    x <- stringr::str_replace_all(x, "[^0-9]", "")

    n <- nchar(x)

    dplyr::case_when(
      n >= 6 ~ substr(x, 1, 2),
      n == 1 ~ sprintf("%02d", suppressWarnings(as.integer(x))),
      n == 2 ~ x,
      TRUE ~ x
    )
  }

  # ================================== HANDLERS ============================================

  # -------------------- SIM --------------------
  if (fonte_grp == "SIM") {

    dict_files <- c("TIPOBITO","COR","ESTCIV","ESCOLASINGLE","OCUP","LOCOCOR","GRAVIDEZ",
                    "ATESTANT","FONTEINV","MUNUCIPIO_CLEAN","PARTO","OBITOPARTO","OBITOGRAV",
                    "OBITOPUERP","ASSISTMED","CIRUGIA","NECROPSIA","FONTE","NPROC","TPMORTO",
                    "CNES24","CIRCOBITO","UF_BR","CID_COMPLETO","TIPOVIOL","NATURAL",
                    "SIM_NAO","SIMNAO")

    d <- read_dicts(dict_files, base_path)

    tipobito  <- as_lookup(d$TIPOBITO)
    cor       <- as_lookup(d$COR)
    estciv    <- as_lookup(d$ESTCIV)
    escolas   <- as_lookup(d$ESCOLASINGLE)
    ocup      <- as_lookup(d$OCUP)
    lococor   <- as_lookup(d$LOCOCOR)
    gravidez  <- as_lookup(d$GRAVIDEZ)
    atestant  <- as_lookup(d$ATESTANT)
    fonteinv  <- as_lookup(d$FONTEINV)
    mun_clean <- as_lookup(d$MUNUCIPIO_CLEAN)
    parto     <- as_lookup(d$PARTO)
    ob_parto  <- as_lookup(d$OBITOPARTO)
    ob_grav   <- as_lookup(d$OBITOGRAV)
    ob_puerp  <- as_lookup(d$OBITOPUERP)
    assistmed <- as_lookup(d$ASSISTMED)
    cirugia   <- as_lookup(d$CIRUGIA)
    necropsia <- as_lookup(d$NECROPSIA)
    fonte_lkp <- as_lookup(d$FONTE)
    nproc     <- as_lookup(d$NPROC)
    tpmorto   <- as_lookup(d$TPMORTO)
    cnes24    <- as_lookup(d$CNES24)
    circobito <- as_lookup(d$CIRCOBITO)
    tipoviol  <- as_lookup(d$TIPOVIOL)
    natural   <- as_lookup(d$NATURAL)
    cid_comp  <- as_lookup(d$CID_COMPLETO)
    uf_br     <- make_uf_lookup(d$UF_BR) %||% as_lookup(d$UF_BR)
    sim_yesno <- as_lookup(d$SIM_NAO) %||% as_lookup(d$SIMNAO)


    columns_to_lookup <- list(
      TIPOBITO    = tipobito,
      RACACOR     = cor,
      ESTCIVIL    = estciv,
      ESTCIV      = estciv,
      ESC         = escolas,
      ESCMAE      = escolas,
      TIPOVIOL    = tipoviol,
      OCUP        = ocup,
      OCUPMAE     = ocup,
      OCUPPAI     = ocup,
      OCUPACAO    = ocup,
      LOCOCOR     = lococor,
      CODMUNRES   = mun_clean,
      MUNIRES     = mun_clean,
      CODMUNOCOR  = mun_clean,
      MUNIOCOR    = mun_clean,
      CODESTAB    = cnes24,
      GRAVIDEZ    = gravidez,
      NATURAL     = natural,
      CAUSABAS    = cid_comp,
      CAUSABAS_O  = cid_comp,
      PARTO       = parto,
      OBITOPARTO  = ob_parto,
      OBITOGRAV   = ob_grav,
      OBITOPUERP  = ob_puerp,
      ASSISTMED   = assistmed,
      CIRUGIA     = cirugia,
      NECROPSIA   = necropsia,
      CIRCOBITO   = circobito,
      FONTE       = fonte_lkp,
      FONTEINV    = fonteinv,
      ATESTANTE   = atestant,
      NPROC       = nproc,
      TPMORTO     = tpmorto,
      STDOEPIDEM  = sim_yesno,
      TPPOS       = sim_yesno,
      ACIDTRAB    = sim_yesno,
      UFINFORM    = uf_br
    )

    df <- apply_lookups(df, columns_to_lookup)

    if ("HORAOBITO" %in% names(df)) {
      x <- stringr::str_pad(as.character(df$HORAOBITO), width = 4, side = "left", pad = "0")
      df$HORAOBITO <- paste0(substr(x, 1, 2), ":", substr(x, 3, 4))
    }

    df <- to_date_ymd8(df, c("DTOBITO","DATAOBITO","DTATESTADO","DTINVESTIG","DTCADASTRO","DATANASC"), fmt = "%Y%m%d")

    if ("DTNASC" %in% names(df)) {
      x <- as.character(df$DTNASC)
      df$DTNASC <- paste0(substr(x, 1, 2), "-", substr(x, 3, 4), "-", substr(x, 5, 8))
    }

    if ("DTRECEBIM" %in% names(df)) {
      x <- as.character(df$DTRECEBIM)
      df$DTRECEBIM <- paste0(substr(x, 1, 2), "-", substr(x, 3, 4), "-", substr(x, 5, 8))
    }

    num_cols <- intersect(c("QTDFILVIVO","QTDFILMORT"), names(df))
    if (length(num_cols)) {
      df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))
    }

    return(df)
  }

  # -------------------- CIH / CIHA --------------------
  if (fonte_grp %in% c("CIH","CIHA")) {

    dict_files <- c("PROC_AREA","SEXO","CGC_HOSPITAL","MUNICBR","ESPEC","DIAG_PRINC",
                    "COBRANCA","NATUREZA","GESTAO","MORTES","NACIONAL","HOMONIMO","FONTE","MODALIDADE")

    d <- read_dicts(dict_files, base_path)

    sexo       <- as_lookup(d$SEXO)
    municbr    <- as_lookup(d$MUNICBR)
    diag_prim  <- as_lookup(d$DIAG_PRINC)
    cobranca   <- as_lookup(d$COBRANCA)
    natureza   <- as_lookup(d$NATUREZA)
    mortes     <- as_lookup(d$MORTES)
    nacional   <- as_lookup(d$NACIONAL)
    homonimo   <- as_lookup(d$HOMONIMO)
    fonte_lkp  <- as_lookup(d$FONTE)
    modalidade <- as_lookup(d$MODALIDADE)

    cgc_lookup <- NULL
    if (!is.null(d$CGC_HOSPITAL) && all(c("CPF_CNPJ","FANTASIA") %in% names(d$CGC_HOSPITAL))) {
      cgc_lookup <- dplyr::tibble(
        code = as.character(d$CGC_HOSPITAL$CPF_CNPJ),
        description = as.character(d$CGC_HOSPITAL$FANTASIA)
      )
    }

    proc_lookup <- NULL
    if (!is.null(d$PROC_AREA) && all(c("IP_COD","IP_DSCR") %in% names(d$PROC_AREA))) {
      proc_lookup <- dplyr::tibble(
        code = as.character(d$PROC_AREA$IP_COD),
        description = as.character(d$PROC_AREA$IP_DSCR)
      )
    }

    espec_lookup <- NULL
    if (!is.null(d$ESPEC) && all(c("code","terms") %in% names(d$ESPEC))) {
      espec_lookup <- dplyr::tibble(
        code = as.character(d$ESPEC$code),
        description = as.character(d$ESPEC$terms)
      )
    }

    gestao_lookup <- NULL
    if (!is.null(d$GESTAO) && all(c("Code","description") %in% names(d$GESTAO))) {
      gestao_lookup <- dplyr::tibble(
        code = as.character(d$GESTAO$Code),
        description = as.character(d$GESTAO$description)
      )
    }

    columns_to_lookup <- list(
      CGC_HOSP   = cgc_lookup,
      MUNIC_RES  = municbr,
      MUNIC_MOV  = municbr,
      ESPEC      = espec_lookup,
      SEXO       = sexo,
      PROC_REA   = proc_lookup,
      DIAG_PRINC = diag_prim,
      DIAG_SECUN = diag_prim,
      COBRANCA   = cobranca,
      NATUREZA   = natureza,
      GESTAO     = gestao_lookup,
      MORTE      = mortes,
      NACIONAL   = nacional,
      HOMONIMO   = homonimo,
      FONTE      = fonte_lkp,
      MODALIDADE = modalidade
    )

    df <- apply_lookups(df, columns_to_lookup)
    df <- to_date_ymd8(df, c("NASC","UTI_MES_TO","UTI_INT_TO","DT_INTER","DT_SAIDA"), fmt = "%Y%m%d")

    return(df)
  }

  # -------------------- IBGE --------------------
  if (fonte_grp == "IBGE") {
    dict_files <- c("FXETARIA","SEXO_IBGE","SITUACAO_IBGE","MUNICBR")
    d <- read_dicts(dict_files, base_path)

    fxetaria <- as_lookup(d$FXETARIA)
    sexo     <- as_lookup(d$SEXO_IBGE)
    situ     <- as_lookup(d$SITUACAO_IBGE)
    municbr  <- as_lookup(d$MUNICBR)

    df <- apply_lookups(df, list(
      FXETARIA = fxetaria,
      SEXO     = sexo,
      SITUACAO = situ,
      MUNIC_RES = municbr
    ))

    return(df)
  }

  # -------------------- SINASC --------------------
  if (fonte_grp == "SINASC") {

    files <- c(
      "ORIGEM", "CNES24", "LOCNASC", "ESTCIMAE", "ESCMAE", "CODOCUPMAE",
      "MUNICBR", "GESTACAO", "GRAVIDEZ", "PARTO", "CONSULTAS", "SEXO", "RACACOR", "IDANOMAL",
      "TPMETESTIM", "MESPRENAT", "TPAPRESENT", "STTRABPART", "STCESPARTO", "TPNASCASSI",
      "TPFUNCRESP", "TPDOCRESP", "ESCMAEAGR1", "STDNEPIDEM", "STDNNOVA", "CODPAISRES", "UF_BR"
    )

    data_list <- read_dicts(files, base_path)
    names(data_list) <- c(
      "origem", "cnes24", "locnasc", "estcimae", "escmae", "codocupmae",
      "municbr", "gestacao", "gravidez", "parto", "consultas", "sexo", "racacor", "idanomal",
      "tpmetestim", "mesprenat", "tpapresent", "sttrabpart", "stcesparto", "tpnascassi",
      "tpfuncresp", "tpdocresp", "escmaeagr1", "stdnepidem", "stdnnova", "codpaisres", "uf_br"
    )

    map_code <- function(x, dict, code_col = "code", desc_col = "description") {
      x <- as.character(x)
      if (is.null(dict) || nrow(dict) == 0) return(x)
      if (!all(c(code_col, desc_col) %in% names(dict))) return(x)
      out <- dict[[desc_col]][match(x, as.character(dict[[code_col]]))]
      ifelse(!is.na(out), as.character(out), x)
    }

    df <- df %>%
      { if ("ORIGEM" %in% names(.))      dplyr::mutate(., ORIGEM      = map_code(ORIGEM,      data_list$origem)) else . } %>%
      { if ("CODESTAB" %in% names(.))    dplyr::mutate(., CODESTAB    = map_code(CODESTAB,    data_list$cnes24)) else . } %>%
      { if ("LOCNASC" %in% names(.))     dplyr::mutate(., LOCNASC     = map_code(LOCNASC,     data_list$locnasc)) else . } %>%
      { if ("CODMUNNASC" %in% names(.))  dplyr::mutate(., CODMUNNASC  = map_code(CODMUNNASC,  data_list$municbr)) else . } %>%
      { if ("CODMUNRES" %in% names(.))   dplyr::mutate(., CODMUNRES   = map_code(CODMUNRES,   data_list$municbr)) else . } %>%
      { if ("CODMUNNATU" %in% names(.))  dplyr::mutate(., CODMUNNATU  = map_code(CODMUNNATU,  data_list$municbr)) else . } %>%
      { if ("ESTCIMAE" %in% names(.))    dplyr::mutate(., ESTCIMAE    = map_code(ESTCIMAE,    data_list$estcimae)) else . } %>%
      { if ("ESCMAE" %in% names(.))      dplyr::mutate(., ESCMAE      = map_code(ESCMAE,      data_list$escmae)) else . } %>%
      { if ("ESCMAE2010" %in% names(.))  dplyr::mutate(., ESCMAE2010  = map_code(ESCMAE2010,  data_list$escmae)) else . } %>%
      { if ("ESCMAEGR1" %in% names(.))   dplyr::mutate(., ESCMAEGR1   = map_code(ESCMAEGR1,   data_list$escmaeagr1)) else . } %>%
      { if ("CODOCUPMAE" %in% names(.))  dplyr::mutate(., CODOCUPMAE  = map_code(CODOCUPMAE,  data_list$codocupmae)) else . } %>%
      { if ("GESTACAO" %in% names(.))    dplyr::mutate(., GESTACAO    = map_code(GESTACAO,    data_list$gestacao)) else . } %>%
      { if ("GRAVIDEZ" %in% names(.))    dplyr::mutate(., GRAVIDEZ    = map_code(GRAVIDEZ,    data_list$gravidez)) else . } %>%
      { if ("PARTO" %in% names(.))       dplyr::mutate(., PARTO       = map_code(PARTO,       data_list$parto)) else . } %>%
      { if ("CONSULTAS" %in% names(.))   dplyr::mutate(., CONSULTAS   = map_code(CONSULTAS,   data_list$consultas)) else . } %>%
      { if ("SEXO" %in% names(.))        dplyr::mutate(., SEXO        = map_code(SEXO,        data_list$sexo)) else . } %>%
      { if ("RACACOR" %in% names(.))     dplyr::mutate(., RACACOR     = map_code(RACACOR,     data_list$racacor)) else . } %>%
      { if ("RACAMAE" %in% names(.))     dplyr::mutate(., RACAMAE     = map_code(RACAMAE,     data_list$racacor)) else . } %>%
      { if ("RACACOREMAE" %in% names(.)) dplyr::mutate(., RACACOREMAE = map_code(RACACOREMAE, data_list$racacor)) else . } %>%
      { if ("IDANOMAL" %in% names(.))    dplyr::mutate(., IDANOMAL    = map_code(IDANOMAL,    data_list$idanomal)) else . } %>%
      { if ("TPMETESTIM" %in% names(.))  dplyr::mutate(., TPMETESTIM  = map_code(TPMETESTIM,  data_list$tpmetestim)) else . } %>%
      { if ("MESPRENAT" %in% names(.))   dplyr::mutate(., MESPRENAT   = map_code(MESPRENAT,   data_list$mesprenat)) else . } %>%
      { if ("TPAPRESENT" %in% names(.))  dplyr::mutate(., TPAPRESENT  = map_code(TPAPRESENT,  data_list$tpapresent)) else . } %>%
      { if ("STTRABPART" %in% names(.))  dplyr::mutate(., STTRABPART  = map_code(STTRABPART,  data_list$sttrabpart)) else . } %>%
      { if ("STCESPARTO" %in% names(.))  dplyr::mutate(., STCESPARTO  = map_code(STCESPARTO,  data_list$stcesparto)) else . } %>%
      { if ("TPNASCASSI" %in% names(.))  dplyr::mutate(., TPNASCASSI  = map_code(TPNASCASSI,  data_list$tpnascassi)) else . } %>%
      { if ("TPFUNCRESP" %in% names(.))  dplyr::mutate(., TPFUNCRESP  = map_code(TPFUNCRESP,  data_list$tpfuncresp)) else . } %>%
      { if ("TPFUNCR" %in% names(.))     dplyr::mutate(., TPFUNCR     = map_code(TPFUNCR,     data_list$tpfuncresp)) else . } %>%
      { if ("TPDOCRESP" %in% names(.))   dplyr::mutate(., TPDOCRESP   = map_code(TPDOCRESP,   data_list$tpdocresp)) else . } %>%
      { if ("STDNEPIDEM" %in% names(.))  dplyr::mutate(., STDNEPIDEM  = map_code(STDNEPIDEM,  data_list$stdnepidem)) else . } %>%
      { if ("STDNNOVA" %in% names(.))    dplyr::mutate(., STDNNOVA    = map_code(STDNNOVA,    data_list$stdnnova)) else . } %>%
      { if ("CODPAISRES" %in% names(.))  dplyr::mutate(., CODPAISRES  = map_code(CODPAISRES,  data_list$codpaisres)) else . }

    date_columns <- c("DTNASC","DTCADASTRO","DTRECEBIM","DTULTMENST","DTRECORIGA","DTNASCMAE","DTDECLARAC")
    existing_dates <- intersect(date_columns, names(df))
    if (length(existing_dates)) {
      df[existing_dates] <- lapply(df[existing_dates], function(x) {
        x <- as.character(x)
        as.Date(substr(x, 1, 8), format = "%d%m%Y")
      })
    }

    if ("HORANASC" %in% names(df)) {
      x <- stringr::str_pad(as.character(df$HORANASC), width = 4, side = "left", pad = "0")
      df$HORANASC <- paste0(substr(x, 1, 2), ":", substr(x, 3, 4))
    }

    numeric_columns <- c("QTDFILVIVO","QTDFILMORT","QTDGESTANT","QTDPARTNOR","QTDPARTCES")
    existing_num <- intersect(numeric_columns, names(df))
    if (length(existing_num)) {
      df[existing_num] <- lapply(df[existing_num], function(x) suppressWarnings(as.numeric(x)))
    }

    return(df)
  }

  # -------------------- PAINEL ONCOLOGIA (PO) --------------------
  if (fonte_grp == "PO") {

    msg("Processando bloco PO...")

    dict_files <- c("MUNICBR", "UF_BR", "TP_TRATAMENTO", "CID_COMPLETO", "ESTADIAMENTO", "CGC_HOSPITAL")
    d <- read_dicts(dict_files, base_path)

    munic_res    <- as_lookup(d$MUNICBR)
    uf_br        <- make_uf_lookup(d$UF_BR) %||% as_lookup(d$UF_BR)
    tp_trat      <- as_lookup(d$TP_TRATAMENTO)
    cid_completo <- as_lookup(d$CID_COMPLETO)
    estadiam     <- as_lookup(d$ESTADIAMENTO)

    cgc_lookup <- NULL
    if (!is.null(d$CGC_HOSPITAL) && all(c("CNES","FANTASIA") %in% names(d$CGC_HOSPITAL))) {
      cgc_lookup <- tibble::tibble(
        code = as.character(d$CGC_HOSPITAL$CNES),
        description = as.character(d$CGC_HOSPITAL$FANTASIA)
      )
    }

    columns_to_lookup <- list(
      UF_RESID   = uf_br,
      UF_TRATAM  = uf_br,
      UF_DIAGN   = uf_br,
      MUN_TRATAM = munic_res,
      MUN_RESID  = munic_res,
      MUN_DIAG   = munic_res,
      TRATAMENTO = tp_trat,
      DIAGNOSTIC = cid_completo,
      ESTADIAM   = estadiam,
      CNES_DIAG  = cgc_lookup,
      CNES_TRAT  = cgc_lookup,
      DIAG_DETH  = cid_completo
    )

    df <- apply_lookups(df, columns_to_lookup)

    date_cols <- intersect(c("ANOMES_DIA", "ANOMES_TRA"), names(df))
    if (length(date_cols)) {
      df[date_cols] <- lapply(df[date_cols], function(x) {
        x <- as.character(x)
        suppressWarnings(format(as.Date(paste0(substr(x, 1, 6), "01"), "%Y%m%d"), "%Y-%m"))
      })
    }

    if ("IDADE" %in% names(df)) {
      df$IDADE <- suppressWarnings(as.numeric(df$IDADE))
    }

    msg("Bloco PO concluido.")
    return(df)
  }

  # --------------------------- RESP ---------------------------
  if (fonte_grp == "RESP") {

    msg("Processando bloco RESP...")

    dict_files <- c(
      "CLASS_FETO", "CLASSIFIN", "CRITERIO", "EXANT_GES", "FX_GES", "GRAVIDEZ",
      "HIST_ARBOV", "MICROCEFAL", "OUTSINT", "PESO", "RACACOR", "REGIAO",
      "RESUL", "REXAME", "SIM_NAO", "STATUS_NOT", "TP_DETECCA", "TP_NOTIFIC",
      "MUNICBR", "SEXO", "UF_BR", "UFREG", "ETIOLOGIA"
    )

    d <- read_dicts(dict_files, base_path)

    look <- list(
      class_feto  = as_lookup(d$CLASS_FETO),
      classifin   = as_lookup(d$CLASSIFIN),
      criterio    = as_lookup(d$CRITERIO),
      exant_ges   = as_lookup(d$EXANT_GES),
      fx_ges      = as_lookup(d$FX_GES),
      gravidez    = as_lookup(d$GRAVIDEZ),
      hist_arbov  = as_lookup(d$HIST_ARBOV),
      microcefal  = as_lookup(d$MICROCEFAL),
      outsint     = as_lookup(d$OUTSINT),
      peso        = as_lookup(d$PESO),
      racacor     = as_lookup(d$RACACOR),
      regiao      = as_lookup(d$REGIAO),
      resul       = as_lookup(d$RESUL),
      rexame      = as_lookup(d$REXAME),
      sim_nao     = as_lookup(d$SIM_NAO),
      status_not  = as_lookup(d$STATUS_NOT),
      tp_detecca  = as_lookup(d$TP_DETECCA),
      tp_notific  = as_lookup(d$TP_NOTIFIC),
      munic_res   = as_lookup(d$MUNICBR),
      sexo        = as_lookup(d$SEXO),
      uf_br       = make_uf_lookup(d$UF_BR) %||% as_lookup(d$UF_BR),
      ufreg       = as_lookup(d$UFREG),
      etiologia   = as_lookup(d$ETIOLOGIA)
    )

    columns_to_lookup <- list(
      RACACOR     = look$racacor,
      REGIAORES   = look$regiao,
      TP_NOTIFIC  = look$tp_notific,
      UFRES       = look$uf_br,
      CODMUNRES   = look$munic_res,
      SEXO        = look$sexo,
      MICROCEFAL  = look$microcefal,
      DEF_NEURO   = look$sim_nao,
      DEF_AUDIT   = look$sim_nao,
      DEF_VISUAL  = look$sim_nao,
      TPDETECCAO  = look$tp_detecca,
      GRAVIDEZ    = look$gravidez,
      CLASS_FETO  = look$class_feto,
      FEBRE_GES   = look$sim_nao,
      EXANT_GES   = look$exant_ges,
      PRURIDO     = look$outsint,
      CONJUNTIV   = look$outsint,
      DOR_ARTIC   = look$outsint,
      DOR_MUSC    = look$outsint,
      EDEMA       = look$outsint,
      CEFALEIA    = look$outsint,
      HIPERT_GAN  = look$outsint,
      EXA_TORSCH  = look$sim_nao,
      RESUL_S     = look$resul,
      RESUL_TO    = look$resul,
      RESUL_C     = look$resul,
      RESUL_H     = look$resul,
      RESUL_Z     = look$resul,
      SO_IGG_Z    = look$resul,
      SO_IGM_Z    = look$resul,
      TR_IGG_Z    = look$resul,
      TR_IGM_Z    = look$resul,
      PCR_Z       = look$resul,
      RNRESUL_S   = look$resul,
      RNRESUL_TO  = look$resul,
      RNRESUL_C   = look$resul,
      RNRESUL_H   = look$resul,
      RNRESUL_Z   = look$resul,
      RNSO_IGG_Z  = look$resul,
      RNSO_IGM_Z  = look$resul,
      RNTR_IGG_Z  = look$resul,
      RNTR_IGM_Z  = look$resul,
      RNPCR_Z     = look$resul,
      EXAME_USS   = look$rexame,
      EXAME_TC    = look$rexame,
      EXA_TRANSF  = look$rexame,
      EXAME_RS    = look$rexame,
      REGIAONOT   = look$regiao,
      UFNOT       = look$ufreg,
      CODMUNNOT   = look$munic_res,
      ST_OBITO    = look$sim_nao,
      CLASSIFIN   = look$classifin,
      CRITERIO    = look$criterio,
      STATUS_NOT  = look$status_not,
      ETIOLOGIA   = look$etiologia
    )

    df <- apply_lookups(df, columns_to_lookup)

    date_cols <- c("DT_NOTIFIC", "DT_NASCMAE", "DT_NASC", "DT_SINTOMA", "DT_USS",
                   "DT_TRANSF", "DT_TC", "DT_RS", "DT_OBITO", "DT_ULT_ALT")
    date_cols <- intersect(date_cols, names(df))
    if (length(date_cols)) {
      df[date_cols] <- lapply(df[date_cols], function(x) {
        as.Date(substr(as.character(x), 1, 8), format = "%d%m%Y")
      })
    }

    msg("Bloco RESP concluido.")
    return(df)
  }

  # --------------------------- PCE ---------------------------
  if (fonte_grp == "PCE") {

    msg("Processando bloco PCE...")

    dict_files <- c("REGIAO")
    d <- read_dicts(dict_files, base_path)

    regiao <- as_lookup(d$REGIAO)

    if ("ID_DISTR" %in% names(df)) {
      df$ID_DISTR <- suppressWarnings(as.numeric(df$ID_DISTR))
    }

    df <- apply_lookups(df, list(ID_DISTR = regiao))

    rename_list <- c(
      POP_TRABALHADO               = "QT_POP",
      POP_NAO_RECOLHIDO            = "QT_NRECOL",
      POP_EXAMINADO                = "QT_EXAM",
      POSITIVOS                    = "QT_POS",
      `1_A_4_OVOS`                 = "QT_1A4",
      `5_A_16_OVOS`                = "QT_5A16",
      `17_OU_MIAS_OVOS`            = "QT_17",
      QUANT_A_TRATAR               = "QT_ATRAT",
      QUANT_TRATADA                = "QT_TRAT",
      NAO_TRATRADO_CONTRAIDICACAO  = "QT_CI",
      NAO_TRATRADO_RECUSAO         = "QT_REC",
      NAO_TRATRADO_AUSENCIA        = "QT_AUS",
      ASCARIS                      = "QT_ASC",
      ANCILOSTOMO                  = "QT_ANC",
      TAENIA                       = "QT_TAE",
      TRICHOCEPHALUS               = "QT_TT",
      ENTEROBIUS_VERMIC            = "QT_EV",
      STRONGYLOIDES_STERC          = "QT_SE",
      HYMENOLEPIS_NANA             = "QT_HN",
      OUTRAS_VERMINOSES            = "QT_OUT",
      ESTACAO_PESQUISA             = "QT_PESQ",
      CARAMUJOS_CAPTURADOS         = "QT_CAP",
      STRAMINEA_CAPTURADA          = "QT_BGLA",
      TENAGOPHILA_CAPTURADA        = "QT_BTEN",
      OUTRA_ESPECIE_CAPTURADA      = "QT_OUT1",
      GLABRATA_POSITIVO            = "QT_POSBGLA",
      STRAMINEA_POSITIVO           = "QT_POSBSTR",
      TENAGOPHILA_POSITIVO         = "QT_POSBTEN",
      OUTRA_ESPECIE_POSITIVO       = "QT_POSOUT"
    )

    rename_list <- rename_list[names(rename_list) %in% names(df)]
    df <- dplyr::rename(df, !!!rename_list)

    if ("DT_COMP" %in% names(df)) {
      df$DT_COMP <- as.character(df$DT_COMP)
      df$DT_COMP <- paste0(substr(df$DT_COMP, 1, 4), "-", substr(df$DT_COMP, 5, 6))
    }

    msg("Bloco PCE concluido.")
    return(df)
  }

  # --------------------------- CNES ---------------------------
  if (fonte_grp == "CNES") {

    msg("Processando bloco CNES...")

    dict_files <- c(
      "CNES24","VINC_SUS","TP_GESTAO","ESFERA_A","RETENCAO",
      "ATIVIDAD","NATUREZA","CLIENTEL","CO_BANCO","ORGEXPED",
      "AV_ACRED","CLASAVAL","SIMNAO","COD_IR","NIV_DEP",
      "TP_UNIDADE","NIV_HER","TP_PREST","TURNO_AT","PF_PJ",
      "MUNICBR"
    )

    d <- read_dicts(dict_files, base_path)

    cnes24     <- as_lookup(d$CNES24)
    vinc_sus   <- as_lookup(d$VINC_SUS)
    tp_gestao  <- as_lookup(d$TP_GESTAO)
    esfera_a   <- as_lookup(d$ESFERA_A)
    retencao   <- as_lookup(d$RETENCAO)
    atividad   <- as_lookup(d$ATIVIDAD)
    natureza   <- as_lookup(d$NATUREZA)
    clientel   <- as_lookup(d$CLIENTEL)
    co_banco   <- as_lookup(d$CO_BANCO)
    orgexped   <- as_lookup(d$ORGEXPED)
    av_acred   <- as_lookup(d$AV_ACRED)
    clasaval   <- as_lookup(d$CLASAVAL)
    simnao     <- as_lookup(d$SIMNAO)
    cod_ir     <- as_lookup(d$COD_IR)
    niv_dep    <- as_lookup(d$NIV_DEP)
    tp_unidade <- as_lookup(d$TP_UNIDADE)
    niv_her    <- as_lookup(d$NIV_HER)
    tp_prest   <- as_lookup(d$TP_PREST)
    turno_at   <- as_lookup(d$TURNO_AT)
    pf_pj      <- as_lookup(d$PF_PJ)
    municbr    <- as_lookup(d$MUNICBR)

    columns_to_lookup <- list(
      CNES      = cnes24,
      CODUFMUN  = municbr,
      VINC_SUS  = vinc_sus,
      TPGESTAO  = tp_gestao,
      ESFERA_A  = esfera_a,
      TP_UNID   = tp_unidade,
      RETENCAO  = retencao,
      ATIVIDAD  = atividad,
      NATUREZA  = natureza,
      CLIENTEL  = clientel,
      CO_BANCO  = co_banco,
      ORGEXPED  = orgexped,
      AV_ACRED  = av_acred,
      CLASAVAL  = clasaval,
      SIMNAO    = simnao,
      COD_IR    = cod_ir,
      NIV_DEP   = niv_dep,
      NIV_HIER  = niv_her,
      TP_PREST  = tp_prest,
      TURNO_AT  = turno_at,
      PF_PJ     = pf_pj
    )

    df <- apply_lookups(df, columns_to_lookup)

    sim_cols <- c(
      "CENTRNEO","ATENDHOS","SERAP01P","SERAP01T","SERAP02P","SERAP02T","SERAP03P","SERAP03T",
      "SERAP04P","SERAP04T","SERAP05P","SERAP05T","SERAP06P","SERAP06T","SERAP07P","SERAP07T",
      "SERAP08P","SERAP08T","SERAP09P","SERAP09T","SERAP10P","SERAP10T","SERAP11P","SERAP11T",
      "SERAPOIO","RES_BIOL","RES_QUIM","RES_RADI","RES_COMU","COLETRES","COMISS01","COMISS02",
      "COMISS03","COMISS04","COMISS05","COMISS06","COMISS07","COMISS08","COMISS09","COMISS10",
      "COMISS11","COMISS12","COMISSAO",
      "AP01CV01","AP01CV02","AP01CV05","AP01CV06","AP01CV03","AP01CV04",
      "AP02CV01","AP02CV02","AP02CV05","AP02CV06","AP02CV03","AP02CV04",
      "AP03CV01","AP03CV02","AP03CV05","AP03CV06","AP03CV03","AP03CV04",
      "AP04CV01","AP04CV02","AP04CV05","AP04CV06","AP04CV03","AP04CV04",
      "AP05CV01","AP05CV02","AP05CV05","AP05CV06","AP05CV03","AP05CV04",
      "AP06CV01","AP06CV02","AP06CV05","AP06CV06","AP06CV03","AP06CV04",
      "AP07CV01","AP07CV02","AP07CV05","AP07CV06","AP07CV03","AP07CV04",
      "ATEND_PR","GESPRG1E","GESPRG1M","GESPRG2E","GESPRG2M","GESPRG4E","GESPRG4M",
      "NIVATE_A","GESPRG3E","GESPRG3M","GESPRG5E","GESPRG5M","GESPRG6E","GESPRG6M",
      "NIVATE_H","LEITHOSP","ATENDAMB","URGEMERG","CENTRCIR","CENTROBS"
    )

    if (!is.null(simnao) && all(c("code", "description") %in% names(simnao))) {
      for (col in intersect(sim_cols, names(df))) {
        mapped <- simnao$description[match(as.character(df[[col]]), simnao$code)]
        df[[col]] <- ifelse(is.na(mapped), df[[col]], mapped)
      }
    }

    date_cols <- intersect(
      c("DT_PUBLM","DT_PUBLE","DT_EXPED","DT_ACRED","DT_ATUAL",
        "COMPETEN","CMPT_INI","CMPT_FIM","MAPORTAR"),
      names(df)
    )

    if (length(date_cols)) {
      df[date_cols] <- lapply(df[date_cols], function(x) {
        x <- as.character(x)
        paste0(substr(x, 1, 4), "-", substr(x, 5, 6))
      })
    }

    numeric_cols <- intersect(
      c(
        "QTLEITP1","QTLEITP2","QTLEITP3","LEITHOSP","URGEMERG",
        paste0("QTINST", sprintf("%02d", 1:37)),
        "QTLEIT05","QTLEIT06","QTLEIT07","QTLEIT08","QTLEIT09",
        "QTLEIT19","QTLEIT20","QTLEIT21","QTLEIT22","QTLEIT23",
        "QTLEIT32","QTLEIT34","QTLEIT38","QTLEIT39","QTLEIT40",
        "CONTRATM","CONTRATE","ALVARA"
      ),
      names(df)
    )

    if (length(numeric_cols)) {
      df[numeric_cols] <- lapply(df[numeric_cols], function(x) suppressWarnings(as.numeric(x)))
    }

    msg("Bloco CNES concluido.")
    return(df)
  }

  # -------------------- SIHSUS (AIH) --------------------
  if (fonte_grp == "SIHSUS") {

    msg("Processando bloco SIHSUS")

    dict_files <- c(
      "CNES24",
      "UF_BR",
      "MUNICBR",
      "SEXO",
      "CID_COMPLETO",
      "TP_GESTAO",
      "MORTES",
      "PROC_REA",
      "COBRANCA",
      "TP_FIN",
      "TP_GROUP",
      "RACACOR",
      "SEXO_IBGE",
      "ESPEC",
      "IDEN"
    )

    d <- read_dicts(dict_files, base_path)

    cnes24    <- as_lookup(d$CNES24)
    municbr   <- as_lookup(d$MUNICBR)
    sexo      <- as_lookup(d$SEXO)
    sexo_ibge <- as_lookup(d$SEXO_IBGE)
    cid_comp  <- as_lookup(d$CID_COMPLETO)
    gestao    <- as_lookup(d$TP_GESTAO)
    mortes    <- as_lookup(d$MORTES)
    proc_rea  <- as_lookup(d$PROC_REA)
    cobranca  <- as_lookup(d$COBRANCA)
    tp_fin    <- as_lookup(d$TP_FIN)
    tp_group  <- as_lookup(d$TP_GROUP)
    racacor   <- as_lookup(d$RACACOR)
    espec     <- as_lookup(d$ESPEC)
    iden      <- as_lookup(d$IDEN)
    uf_br     <- make_uf_lookup(d$UF_BR) %||% as_lookup(d$UF_BR)

    uf_cols <- intersect(c("UF_ZI", "UF_RES", "SP_UF"), names(df))
    if (length(uf_cols)) {
      for (cc in uf_cols) df[[cc]] <- normalize_uf_code(df[[cc]])
    }

    columns_to_lookup <- list(
      UF_ZI      = uf_br,
      SP_UF      = uf_br,
      UF_RES     = uf_br,

      MUN_RES    = municbr,
      MUNIC_RES  = municbr,
      MUNIC_MOV  = municbr,
      MUN_MOV    = municbr,
      IDENT       = iden,

      CNES       = cnes24,
      SP_CNES    = cnes24,
      CGC_HOSP   = cnes24,

      RACA_COR   = racacor,

      PROC_REA   = proc_rea,
      SP_PROCREA = proc_rea,
      PROC_SOLIC = proc_rea,

      COBRANCA   = cobranca,
      FINANC     = tp_fin,
      SP_FINANC  = tp_fin,

      COMPLEX    = tp_group,
      SP_COMPLEX = tp_group,

      GESTAO     = gestao,
      SP_GESTOR  = gestao,
      SP_GESTAO  = gestao,
      ESPEC      = espec,

      DIAG_PRINC = cid_comp,
      DIAG_SECUN = cid_comp,
      CID_ASSO   = cid_comp,
      CID_MORTE  = cid_comp,
      CID_NOTIF  = cid_comp,
      SP_CIDPRI  = cid_comp,
      SP_CIDSEC  = cid_comp,
      DIAGSEC1   = cid_comp,
      DIAGSEC2   = cid_comp,
      DIAGSEC3   = cid_comp,
      DIAGSEC4   = cid_comp,
      DIAGSEC5   = cid_comp,
      DIAGSEC6   = cid_comp,
      DIAGSEC7   = cid_comp,
      DIAGSEC8   = cid_comp,
      DIAGSEC9   = cid_comp,

      MORTE      = mortes
    )

    df <- apply_lookups(df, columns_to_lookup)

    if ("SEXO" %in% names(df)) {
      df <- apply_lookup(df, "SEXO", sexo_ibge %||% sexo_ibge)
    }

    if (all(c("ANO_CMPT", "MES_CMPT") %in% names(df))) {
      df$CMPT <- sprintf(
        "%04d-%02d",
        suppressWarnings(as.integer(df$ANO_CMPT)),
        suppressWarnings(as.integer(df$MES_CMPT))
      )
    }

    if (all(c("SP_AA", "SP_MM") %in% names(df))) {
      df$SP_CMPT <- sprintf(
        "%04d-%02d",
        suppressWarnings(as.integer(df$SP_AA)),
        suppressWarnings(as.integer(df$SP_MM))
      )
    }

    df <- to_date_ymd8(
      df,
      c("NASC", "DT_INTER", "DT_SAIDA", "GESTOR_DT", "SP_DTINTER", "SP_DTSAIDA"),
      fmt = "%Y%m%d"
    )

    num_cols <- intersect(
      c(
        "UTI_MES_IN","UTI_MES_AN","UTI_MES_AL","UTI_MES_TO",
        "UTI_INT_IN","UTI_INT_AN","UTI_INT_AL","UTI_INT_TO",
        "DIAR_ACOM","QT_DIARIAS",
        "IDADE","DIAS_PERM","NUM_FILHOS",
        "NUM_PROC","TOT_PT_SP","US_TOT",
        "VAL_SH","VAL_SP","VAL_SADT","VAL_RN","VAL_ACOMP","VAL_ORTP","VAL_SANGUE",
        "VAL_SADTSR","VAL_TRANSP","VAL_OBSANG","VAL_PED1AC","VAL_TOT","VAL_UTI",
        "VAL_SH_FED","VAL_SP_FED","VAL_SH_GES","VAL_SP_GES","VAL_UCI",
        "SP_NUM_PR","SP_QTD_ATO","SP_PTSP","SP_VALATO","SP_QT_PROC",
        "SP_M_HOSP","SP_M_PAC"
      ),
      names(df)
    )

    if (length(num_cols)) {
      df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))
    }

    id_cols <- intersect(
      c(
        "N_AIH","SEQ_AIH5","CGC_HOSP","CPF_AUT","GESTOR_CPF","CNPJ_MANT",
        "CEP","IDENT","CAR_INT","HOMONIMO","INSC_PN","VINCPREV","CNAER","CBOR",
        "SEQUENCIA","REMESSA",
        "SP_NAIH","SP_U_AIH","SP_CPFCGC","SP_PF_DOC","SP_PJ_DOC","SP_NF"
      ),
      names(df)
    )

    if (length(id_cols)) {
      df[id_cols] <- lapply(df[id_cols], as.character)
    }

    msg("Bloco SIHSUS concluido.")
    return(df)
  }

  # --------------------------- SIASUS ---------------------------
  if (fonte_grp == "SIASUS") {

    msg("Processando bloco SIASUS...")

    dict_files <- c(
      "CNES24","PA_CNPJ_CC","PA_UFDIF","PA_MNDIF","PA_DOCORIG","MUNICBR","PA_NIVCPL",
      "PA_TPFIN","PA_INCOUT","PA_REGCT","PA_CATEND","PA_MOTSAI","TP_GRUPO","PA_ALTA",
      "PA_OBITO","PA_ENCERR","PA_PERMAN","PA_TRANSF","CID","PA_CBOCOD","PA_TIPPRE",
      "PA_TPUPS","PA_NAT_JUR","PA_MN_IND","PA_CODUNI","SEXO","PA_FLIDADE","UF_BR",
      "AB_PROCAIH","AB_PONTBARR","AB_TABBARR","TP_GESTAO","AP_TPATEND","AP_CATEND",
      "AP_MOTSAI","SIMNAO","AN_HBSAG","AQ_ESTADI","AQ_GRAPHIS","AQ_CONTTR","AQ_MED10",
      "atd_acevas","atd_caract","atd_hbsag","atd_maisne","atd_sitini","atd_sittra",
      "atd_seapto","atd_seperia","DESTINOPAC","PA_SRV","PA_TP_EQP","RACACOR","ORIGEM",
      "TP_GES","REGIAO","AP_COR","NACION","AP_TPAPAC","AR_GRAPHIS","AR_LINFIN",
      "AP_NATJ","PA_CODOCO","PA_CONDIC","PA_PROC_ID"
    )

    d <- read_dicts(dict_files, base_path)

    conv <- function(name) as_lookup(d[[name]])

    cnes24     <- conv("CNES24")
    pa_ufdif   <- conv("PA_UFDIF")
    pa_mndif   <- conv("PA_MNDIF")
    municbr    <- conv("MUNICBR")
    pa_tippre  <- conv("PA_TIPPRE")
    pa_tpups   <- conv("PA_TPUPS")
    pa_nat_jur <- conv("PA_NAT_JUR")
    pa_mn_ind  <- conv("PA_MN_IND")
    sexo       <- conv("SEXO")
    ab_procaih <- conv("AB_PROCAIH")
    ab_tabbarr <- conv("AB_TABBARR")
    ap_tpatend <- conv("AP_TPATEND")
    ap_catend  <- conv("AP_CATEND")
    ap_motsai  <- conv("AP_MOTSAI")
    simnao     <- conv("SIMNAO")
    aq_estadi  <- conv("AQ_ESTADI")
    aq_graphis <- conv("AQ_GRAPHIS")
    aq_med10   <- conv("AQ_MED10")
    an_hbsag   <- conv("AN_HBSAG")
    ar_graphis <- conv("AR_GRAPHIS")
    ar_linfin  <- conv("AR_LINFIN")
    origem     <- conv("ORIGEM")
    destinopac <- conv("DESTINOPAC")
    racacor    <- conv("RACACOR")
    pa_srv     <- conv("PA_SRV")
    pa_tp_eqp  <- conv("PA_TP_EQP")
    tp_ges     <- conv("TP_GES")
    nacion     <- conv("NACION")
    ap_cor     <- conv("AP_COR")
    ap_natj    <- conv("AP_NATJ")
    cid        <- conv("CID")
    pa_proc_id <- conv("PA_PROC_ID")

    if ("PA_CODOCO" %in% names(df) && "PA_FLQT" %in% names(df)) {
      df$PA_CODOCO <- paste0(df$PA_CODOCO, df$PA_FLQT)
    }

    if ("PA_SRV" %in% names(df) && "PA_CLASS_S" %in% names(df)) {
      df$PA_SRV <- paste0(df$PA_SRV, df$PA_CLASS_S)
    }

    map <- list(
      AB_TABBARR = ab_tabbarr,
      AP_CONDIC  = tp_ges,
      AP_PRIPAL  = ab_procaih,
      AP_TPATEND = ap_tpatend,
      AP_CATEND  = ap_catend,
      AP_MOTSAI  = ap_motsai,
      AP_CIDPRI  = cid,
      AP_CIDSEC  = cid,
      AP_CIDCAS  = cid,
      AP_CODUNI  = cnes24,
      AP_TIPRE   = pa_tippre,
      AP_TPUPS   = pa_tpups,
      AP_NAT_JUR = pa_nat_jur,
      AP_NATJUR  = ap_natj,
      AP_MN_IND  = pa_mn_ind,
      AP_RACACOR = ap_cor,
      AP_UFNACIO = nacion,
      AP_SEXO    = sexo,
      AP_MUNPCN  = municbr,
      AP_UFDIF   = pa_ufdif,
      AP_MNDIF   = pa_mndif,
      ACF_DUPLEX = simnao,
      AQ_CID10   = cid,
      AQ_ESTADI  = aq_estadi,
      AQ_GRAPHIS = aq_graphis,
      AQ_MED10   = aq_med10,
      AN_HBSAG   = an_hbsag,
      AR_LINFIN  = ar_linfin,
      ORIGEM_PAC = origem,
      DESTINOPAC = destinopac,
      SEXOPAC    = sexo,
      RACACOR    = racacor,
      PA_SRV     = pa_srv,
      PA_TP_EQP  = pa_tp_eqp,
      PA_CID     = cid,
      PA_PROC_ID = pa_proc_id
    )

    df <- apply_lookups(df, map)

    date_cols <- intersect(
      c("AB_DTCIRUR","AP_DTAUT","AP_DTSOLIC","AP_DTFIM","AP_DTINIC",
        "DTDECLARAC","AR_FIMAR2","AR_FIMAR3","AR_FIMAR1","AR_INIAR1",
        "AR_INIAR2","AR_INIAR3","AR_DTIDEN","AR_DTINTR","AP_DTCOR",
        "ATD_DTP","ATD_DTCLI","ATD_DTPDR","INICIO","FIM","DT_INICIO","DTNASC"),
      names(df)
    )

    if (length(date_cols)) {
      df[date_cols] <- lapply(df[date_cols], function(x) {
        suppressWarnings(as.Date(substr(as.character(x), 1, 8), "%Y%m%d"))
      })
    }

    num_cols <- intersect(c("AQ_TOTMPL","AQ_TOTMAU"), names(df))
    if (length(num_cols)) {
      df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))
    }

    msg("Bloco SIASUS concluido.")
    return(df)
  }

  # --------------------------- SISPRENATAL ---------------------------
  if (fonte_grp == "SISPRENATAL") {

    msg("Processando bloco SISPRENATAL...")

    dict_files <- c("ETNIA","CO_TPO_GRA","CO_TP_RISC","MUNICBR","NACIONAL",
                    "ESCOLA","RACACOR","DS_ACOMPAN","UF_BR")

    d <- read_dicts(dict_files, base_path)

    conv <- function(name) as_lookup(d[[name]])

    etnia       <- conv("ETNIA")
    co_tpo_gra  <- conv("CO_TPO_GRA")
    co_tp_risc  <- conv("CO_TP_RISC")
    municbr     <- conv("MUNICBR")
    nacional    <- conv("NACIONAL")
    escola      <- conv("ESCOLA")
    racacor     <- conv("RACACOR")
    ds_acompan  <- conv("DS_ACOMPAN")
    uf_br       <- conv("UF_BR")

    lookup_map <- list(
      CO_ESC_GP  = escola,
      CO_ETN_GP  = etnia,
      CO_TPO_GRA = co_tpo_gra,
      CO_TP_RISC = co_tp_risc,
      CO_MUN_PPT = municbr,
      CO_MUN_UBS = municbr,
      CO_PAIS    = nacional,
      CO_RAC_GP  = racacor,
      CO_UF_IBGE = uf_br,
      DS_ACOMPAN = ds_acompan
    )

    df <- apply_lookups(df, lookup_map)

    date_cols <- intersect(c("DT_DUM","DT_DPP","DT_INC"), names(df))
    if (length(date_cols)) {
      df[date_cols] <- lapply(df[date_cols], function(x) {
        as.Date(substr(as.character(x), 1, 8), "%Y%m%d")
      })
    }

    num_cols <- intersect(
      c("QT_AB_GER","QT_AB_MOL","QT_MOR_APS","QT_AB_ECT","QT_MOR_PS",
        "QT_NSC_MOR","QT_NSC_VIV","QT_PRT_CIR","QT_PRT_FOR","QT_PRT_VAG",
        "QT_CONS","QT_CONSULT"),
      names(df)
    )

    if (length(num_cols)) {
      df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))
    }

    msg("Bloco SISPRENATAL concluido.")
    return(df)
  }

  # -------------------- SISCOLO (CC / HC) --------------------
  if (fonte_grp == "SISCOLO") {

    msg("Processando bloco SISCOLO...")

    dict_files <- c(
      "UF_BR",
      "MUNICBR",
      "RACACOR",
      "FAIXA_ETARIA",
      "ESCOLARIDADE",
      "CNES24",
      "REGRESID",
      "O_CIT_GLAN",
      "O_CIT_ESCA",
      "CO_CIT_IND",
      "CO_CIT_GLA",
      "CO_CIT_ESC",
      "CO_MARG",
      "CO_RES_LOC",
      "CO_DIF_GRA",
      "CO_COLP",
      "CO_FX_ETAR",
      "DINTTEMPEXAME",
      "DINTRESULT",
      "DINTCOLETA",
      "CO_ATI_ESC",
      "CO_ATI_GLA",
      "CO_ATI_IND",
      "CO_CEL_ESC",
      "CO_CEL_GLA",
      "CO_NEO_MAL",
      "CO_AMOSTRA",
      "CO_ADEQ_MA",
      "CO_BEN_INF",
      "CO_BEN_MET",
      "CO_BEN_REP",
      "CO_BEN_ATR",
      "CO_BEN_RAD",
      "CO_BEN_OUT",
      "CO_MIC_LAC",
      "CO_MIC_COC",
      "CO_MIC_CHL",
      "CO_MIC_ACT",
      "CO_MIC_BAC",
      "CO_MIC_TRI",
      "CO_MIC_HER",
      "CO_MIC_CAN",
      "CO_MIC_GAR",
      "CO_MIC_OUT",
      "CO_ANM_PRE",
      "ST_MON_EXT",
      "TEMPCITANT",
      "CO_RES_TIP",
      "CO_RES_FRA",
      "CO_RES_TAM",
      "ESCOLA",
      "SEXO"
    )

    d <- read_dicts(dict_files, base_path)

    uf_br        <- make_uf_lookup(d$UF_BR) %||% as_lookup(d$UF_BR)
    municbr      <- as_lookup(d$MUNICBR)
    racacor      <- as_lookup(d$RACACOR)
    faixa_etar   <- as_lookup(d$FAIXA_ETARIA) %||% as_lookup(d$CO_FX_ETAR)
    esc          <- as_lookup(d$ESCOLARIDADE)
    cnes24       <- as_lookup(d$CNES24)
    regresid_lkp <- as_lookup(d$REGRESID)
    escolar      <- as_lookup(d$ESCOLA)
    sexo         <- as_lookup(d$SEXO)
    co_cit_esc   <- as_lookup(d$CO_CIT_ESC)
    co_cit_gla   <- as_lookup(d$CO_CIT_GLA)
    co_cit_ind   <- as_lookup(d$CO_CIT_IND)
    o_cit_esca   <- as_lookup(d$O_CIT_ESCA)
    o_cit_glan   <- as_lookup(d$O_CIT_GLAN)
    co_marg      <- as_lookup(d$CO_MARG)
    co_res_loc   <- as_lookup(d$CO_RES_LOC)
    co_dif_gra   <- as_lookup(d$CO_DIF_GRA)
    co_colp      <- as_lookup(d$CO_COLP)
    dinttempex   <- as_lookup(d$DINTTEMPEXAME)
    dintresult   <- as_lookup(d$DINTRESULT)
    dintcoleta   <- as_lookup(d$DINTCOLETA)
    co_ati_esc   <- as_lookup(d$CO_ATI_ESC)
    co_ati_gla   <- as_lookup(d$CO_ATI_GLA)
    co_ati_ind   <- as_lookup(d$CO_ATI_IND)
    co_cel_esc   <- as_lookup(d$CO_CEL_ESC)
    co_cel_gla   <- as_lookup(d$CO_CEL_GLA)
    co_neo_mal   <- as_lookup(d$CO_NEO_MAL)
    co_amostra   <- as_lookup(d$CO_AMOSTRA)
    co_adeq_ma   <- as_lookup(d$CO_ADEQ_MA)
    co_ben_inf   <- as_lookup(d$CO_BEN_INF)
    co_ben_met   <- as_lookup(d$CO_BEN_MET)
    co_ben_rep   <- as_lookup(d$CO_BEN_REP)
    co_ben_atr   <- as_lookup(d$CO_BEN_ATR)
    co_ben_rad   <- as_lookup(d$CO_BEN_RAD)
    co_ben_out   <- as_lookup(d$CO_BEN_OUT)
    co_mic_lac   <- as_lookup(d$CO_MIC_LAC)
    co_mic_coc   <- as_lookup(d$CO_MIC_COC)
    co_mic_chl   <- as_lookup(d$CO_MIC_CHL)
    co_mic_act   <- as_lookup(d$CO_MIC_ACT)
    co_mic_bac   <- as_lookup(d$CO_MIC_BAC)
    co_mic_tri   <- as_lookup(d$CO_MIC_TRI)
    co_mic_her   <- as_lookup(d$CO_MIC_HER)
    co_mic_can   <- as_lookup(d$CO_MIC_CAN)
    co_mic_gar   <- as_lookup(d$CO_MIC_GAR)
    co_mic_out   <- as_lookup(d$CO_MIC_OUT)
    co_anm_pre   <- as_lookup(d$CO_ANM_PRE)
    st_mon_ext   <- as_lookup(d$ST_MON_EXT)
    tempcitant   <- as_lookup(d$TEMPCITANT)
    co_res_tip   <- as_lookup(d$CO_RES_TIP)
    co_res_fra   <- as_lookup(d$CO_RES_FRA)
    co_res_tam   <- as_lookup(d$CO_RES_TAM)

    columns_to_lookup <- list(
      # UFs
      CO_US_UF   = uf_br,
      CO_PAC_UF  = uf_br,
      CLABUF     = uf_br,

      # Municipios
      CO_US_IBGE = municbr,
      CO_PAC_IBG = municbr,
      CLABIBGE   = municbr,
      REGRESID   = regresid_lkp,
      CO_PAC_SEX = sexo,

      # CNES
      CO_CNES    = cnes24,

      # Demograficos
      CO_PAC_RAC = racacor,
      CO_PAC_ESC = esc,
      CO_FX_ETAR = faixa_etar,


      # Campos citologicos / histopatologicos
      CO_CIT_ESC = co_cit_esc,
      CO_ATI_ESC = co_cit_esc,
      CO_CIT_GLA = co_cit_gla,
      CO_ATI_GLA = co_cit_gla,
      CO_CIT_IND = co_cit_ind,
      CO_ATI_IND = co_cit_ind,
      O_CIT_ESCA = o_cit_esca,
      CO_CEL_ESCA = o_cit_esca,
      O_CIT_GLAN = o_cit_glan,
      CO_CEL_GLA = o_cit_glan,

      CO_MARG    = co_marg,
      CO_RES_LOC = co_res_loc,
      CO_DIF_GRA = co_dif_gra,
      CO_COLP    = co_colp,
      DINTTEMPEX = dinttempex,
      DINTRESULT = dintresult,
      DINTCOLETA = dintcoleta,

      # HC
      CO_ATI_ESC = co_ati_esc,
      CO_ATI_GLA = co_ati_gla,
      CO_ATI_IND = co_ati_ind,
      CO_CEL_ESC = co_cel_esc,
      CO_CEL_GLA = co_cel_gla,
      CO_NEO_MAL = co_neo_mal,
      CO_AMOSTRA = co_amostra,
      CO_ADEQ_MA = co_adeq_ma,
      CO_BEN_INF = co_ben_inf,
      CO_BEN_MET = co_ben_met,
      CO_BEN_REP = co_ben_rep,
      CO_BEN_ATR = co_ben_atr,
      CO_BEN_RAD = co_ben_rad,
      CO_BEN_OUT = co_ben_out,
      CO_MIC_LAC = co_mic_lac,
      CO_MIC_COC = co_mic_coc,
      CO_MIC_CHL = co_mic_chl,
      CO_MIC_ACT = co_mic_act,
      CO_MIC_BAC = co_mic_bac,
      CO_MIC_TRI = co_mic_tri,
      CO_MIC_HER = co_mic_her,
      CO_MIC_CAN = co_mic_can,
      CO_MIC_GAR = co_mic_gar,
      CO_MIC_OUT = co_mic_out,
      CO_ANM_PRE = co_anm_pre,
      ST_MON_EXT = st_mon_ext,
      TEMPCITANT = tempcitant,
      CO_RES_TIP = co_res_tip,
      CO_RES_FRA = co_res_fra,
      CO_RES_TAM = co_res_tam
    )

    df <- apply_lookups(df, columns_to_lookup)

    date_cols <- intersect(
      c("DT_PAC_NAS", "DT_ID_COMP", "DT_HIS_EXA", "DT_HIS_REC", "DT_ANM_PRE"),
      names(df)
    )

    if (length(date_cols)) {
      df <- to_date_ymd8(df, date_cols, fmt = "%Y%m%d")
    }

    num_cols <- intersect(
      c(
        "ANO_COMP",
        "CO_PAC_IDA",
        "QTDEXA",
        "QUANTEXAME",
        "DINTCOLETA",
        "DINTRESULT",
        "DINTTEMPEX",
        "TEMPCITANT"
      ),
      names(df)
    )

    if (length(num_cols)) {
      df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))
    }

    id_cols <- intersect(
      c("CO_US", "CO_CNES", "REGUS", "REGRESID", "REGLAB"),
      names(df)
    )

    if (length(id_cols)) {
      df[id_cols] <- lapply(df[id_cols], as.character)
    }

    if (identical(fonte_in, "SISCOLO-CC")) {
      msg("Subtipo SISCOLO-CC detectado.")
    }

    if (identical(fonte_in, "SISCOLO-HC")) {
      msg("Subtipo SISCOLO-HC detectado.")
    }

    msg("Bloco SISCOLO concluido.")
    return(df)
  }

  if (fonte_grp == "SISMAMA") {

    msg("Processando bloco SISMAMA...")

    # nomes dos parquet dictionaries no seu projeto
    dict_files <- c(
      "UF_BR",
      "MUNICBR",
      "RACACOR",
      "FAIXA_ETARIA",
      "ESCOLARIDADE",
      "CNES24",
      "REGRESID",
      "SEXO",

      # SISMAMA específicos
      "RISCOELEVADO",     # CANMPACANC / C_CLI_TCAN / CANMMAMOGR
      "CCLIDESC",         # CO_CLI_DES
      "CCLINODU",         # CO_CLI_NOD
      "CCLIMATMAM",       # CCLIMATMAM
      "CCLIMATDPC",       # CCLIMATDPC
      "CRESADEQ",         # CO_RES_ADE
      "INTCOLETA",        # DINTCOLETA / DINTSOLICT se usar a mesma tabela
      "INTRESULTADO",     # DINTRESULT
      "INTTEMPOEXAME",    # DINTTEMPEX
      "PRESULPAAF",       # PRESULPAAF
      "CRESBENIG",        # CRESBENIG
      "CRESMALIIN",       # CRESMALIIN
      "CRESSUSMAL",       # CRESSUSMAL
      "CRESPOSMAL",       # CRESPOSMAL
      "CRESDERPAP",       # CRESDERPAP

      # citopatológico mama
      "PBENMASTIT",
      "PBENABSUBA",
      "PBENFIBROA",
      "PBENNECGOR",
      "PBENCONDFI",
      "PBENLESEPI",
      "PBENOUTRAS",
      "PMALINTUMP",
      "PMALINTUMF",
      "PMALINOUTR",
      "PSUSLEJPCA",
      "PSUSOUTROS",
      "PPOSMACDUC",
      "PPOSMACLOB",
      "PPOSMACOUT",
      "DEMATACELU",
      "DENEGMALIG",
      "DEMALIINDT",
      "DEPOSMALIG",
      "DELESMALIG",
      "DEPROCINFL",

      # histopatológico mama
      "CNEOMALIG",
      "LESBENIGIN",
      "CBENHIPSAT",
      "CBENHIPCAT",
      "CBENLOBCAT",
      "CBENADENOS",
      "CBENESDERO",
      "CBENFIBROC",
      "CBENFIBROA",
      "CBENSOLITA",
      "CBENMULTI",
      "CBENFLORID",
      "CBENMASTIT",
      "CBENOUTROS",
      "CCLIDETEC",        # C_CLI_DETE
      "CCLILOCA",         # CCLILOCA
      "CCLITPEXA",        # CCLITPEXA
      "CCLITANT",         # CCLITANT
      "CCLIDIAGIM",       # CCLIDIAGIM
      "CO_CLI_MAM",       # CO_CLI_MAM
      "CCLITAM",          # CO_CLI_TAM
      "CTAMTUM",          # CO_TAM_TUM
      "CCLILINFO",        # C_CLI_LINF / CMICMICRCA
      "CCLIMATPROC",      # CCLIMATPRO / CRESPROCIR
      "CRESADEQHISTO",    # CO_RES_ADEQ
      "CHISGRAU",         # CO_HIS_GRA
      "CHISMARG",         # CO_HIS_MAR
      "CHISRECES",        # CHISRECES
      "CHISRECPR",        # CHISRECPR

      # mamografia
      "CANMEXAPR",
      "TEMPOMAMOGANT",
      "INDICLINIC",
      "TIPMAMDIAG",
      "CCLIDIAG",
      "CRADPELE",
      "CRADCOMPC",
      "CATBIRADS",
      "CLINFAUX",
      "CNODTAM",
      "CCONRECOM"
    )

    d <- read_dicts(dict_files, base_path)

    uf_br        <- make_uf_lookup(d$UF_BR) %||% as_lookup(d$UF_BR)
    municbr      <- as_lookup(d$MUNICBR)
    racacor      <- as_lookup(d$RACACOR)
    faixa_etar   <- as_lookup(d$FAIXA_ETARIA)
    esc          <- as_lookup(d$ESCOLARIDADE)
    cnes24       <- as_lookup(d$CNES24)
    regresid_lkp <- as_lookup(d$REGRESID)
    sexo         <- as_lookup(d$SEXO)

    riscoelevado <- as_lookup(d$RISCOELEVADO)
    cclidesc     <- as_lookup(d$CCLIDESC)
    cclinodu     <- as_lookup(d$CCLINODU)
    cclimatmam   <- as_lookup(d$CCLIMATMAM)
    cclimatdpc   <- as_lookup(d$CCLIMATDPC)
    cresadeq     <- as_lookup(d$CRESADEQ)
    intcoleta    <- as_lookup(d$INTCOLETA)
    intresult    <- as_lookup(d$INTRESULTADO)
    inttempo     <- as_lookup(d$INTTEMPOEXAME)
    presulpaaf   <- as_lookup(d$PRESULPAAF)
    cresbenig    <- as_lookup(d$CRESBENIG)
    cresmaliin   <- as_lookup(d$CRESMALIIN)
    cressusmal   <- as_lookup(d$CRESSUSMAL)
    cresposmal   <- as_lookup(d$CRESPOSMAL)
    cresderpap   <- as_lookup(d$CRESDERPAP)

    pbenmastit   <- as_lookup(d$PBENMASTIT)
    pbenabsuba   <- as_lookup(d$PBENABSUBA)
    pbenfibroa   <- as_lookup(d$PBENFIBROA)
    pbennecgor   <- as_lookup(d$PBENNECGOR)
    pbencondfi   <- as_lookup(d$PBENCONDFI)
    pbenlesepi   <- as_lookup(d$PBENLESEPI)
    pbenoutras   <- as_lookup(d$PBENOUTRAS)
    pmalintump   <- as_lookup(d$PMALINTUMP)
    pmalintumf   <- as_lookup(d$PMALINTUMF)
    pmalinoutr   <- as_lookup(d$PMALINOUTR)
    psuslejpca   <- as_lookup(d$PSUSLEJPCA)
    psusoutros   <- as_lookup(d$PSUSOUTROS)
    pposmacduc   <- as_lookup(d$PPOSMACDUC)
    pposmaclob   <- as_lookup(d$PPOSMACLOB)
    pposmacout   <- as_lookup(d$PPOSMACOUT)
    dematacelu   <- as_lookup(d$DEMATACELU)
    denegmalig   <- as_lookup(d$DENEGMALIG)
    demaliindt   <- as_lookup(d$DEMALIINDT)
    deposmalig   <- as_lookup(d$DEPOSMALIG)
    delesmalig   <- as_lookup(d$DELESMALIG)
    deprocinfl   <- as_lookup(d$DEPROCINFL)

    cneomalig    <- as_lookup(d$CNEOMALIG)
    lesbenigin   <- as_lookup(d$LESBENIGIN)
    cbenhipsat   <- as_lookup(d$CBENHIPSAT)
    cbenhipcat   <- as_lookup(d$CBENHIPCAT)
    cbenlobcat   <- as_lookup(d$CBENLOBCAT)
    cbenadenos   <- as_lookup(d$CBENADENOS)
    cbenesdero   <- as_lookup(d$CBENESDERO)
    cbenfibroc   <- as_lookup(d$CBENFIBROC)
    cbenfibroa   <- as_lookup(d$CBENFIBROA)
    cbensolita   <- as_lookup(d$CBENSOLITA)
    cbenmulti    <- as_lookup(d$CBENMULTI)
    cbenflorid   <- as_lookup(d$CBENFLORID)
    cbenmastit   <- as_lookup(d$CBENMASTIT)
    cbenoutros   <- as_lookup(d$CBENOUTROS)
    cclidetec    <- as_lookup(d$CCLIDETEC)
    ccliloca     <- as_lookup(d$CCLILOCA)
    cclitpexa    <- as_lookup(d$CCLITPEXA)
    cclitant     <- as_lookup(d$CCLITANT)
    cclidiagim   <- as_lookup(d$CCLIDIAGIM)
    co_cli_mam   <- as_lookup(d$CO_CLI_MAM)
    cclitam      <- as_lookup(d$CCLITAM)
    ctamtum      <- as_lookup(d$CTAMTUM)
    cclilinfo    <- as_lookup(d$CCLILINFO)
    cclimatproc  <- as_lookup(d$CCLIMATPROC)
    cresadeqh    <- as_lookup(d$CRESADEQHISTO)
    chisgrau     <- as_lookup(d$CHISGRAU)
    chismarg     <- as_lookup(d$CHISMARG)
    chisreces    <- as_lookup(d$CHISRECES)
    chisrecpr    <- as_lookup(d$CHISRECPR)

    canmexapr    <- as_lookup(d$CANMEXAPR)
    tempomamog   <- as_lookup(d$TEMPOMAMOGANT)
    indiclinic   <- as_lookup(d$INDICLINIC)
    tipmamdiag   <- as_lookup(d$TIPMAMDIAG)
    cclidiag     <- as_lookup(d$CCLIDIAG)
    cradpele     <- as_lookup(d$CRADPELE)
    cradcompc    <- as_lookup(d$CRADCOMPC)
    catbirads    <- as_lookup(d$CATBIRADS)
    clinfaux     <- as_lookup(d$CLINFAUX)
    cnodtam      <- as_lookup(d$CNODTAM)
    cconrecom    <- as_lookup(d$CCONRECOM)

    columns_to_lookup <- list(
      # UFs
      CO_US_UF   = uf_br,
      CO_PAC_UF  = uf_br,
      PRESTUF    = uf_br,

      # Municípios / região
      CO_US_IBGE = municbr,
      CO_PAC_IBG = municbr,
      PRESTMUN   = municbr,
      CO_IBGE    = municbr,
      REGUS      = regresid_lkp,
      REGRESID   = regresid_lkp,
      REGLAB     = regresid_lkp,

      # CNES
      CO_CNES    = cnes24,

      # Demográficos
      CO_PAC_SEX = sexo,
      CO_PAC_RAC = racacor,
      CO_PAC_ESC = esc,
      CO_FX_ETAR = faixa_etar,

      # risco
      CANMPACANC = riscoelevado,
      C_CLI_TCAN = riscoelevado,
      CANMMAMOGR = riscoelevado,

      # clínicos
      CO_CLI_DES = cclidesc,
      CO_CLI_NOD = cclinodu,
      CCLIMATMAM = cclimatmam,
      CCLIMATDPC = cclimatdpc,
      CO_RES_ADE = cresadeq,
      CO_RES_ADEQ = cresadeqh,

      # intervalos
      DINTCOLETA = intcoleta,
      DINTSOLICT = intcoleta,
      DINTRESULT = intresult,
      DINTTEMPEX = inttempo,

      # citopatologia / PAAF
      PRESULPAAF = presulpaaf,
      CRESBENIG  = cresbenig,
      CRESMALIIN = cresmaliin,
      CRESSUSMAL = cressusmal,
      CRESPOSMAL = cresposmal,
      CRESDERPAP = cresderpap,

      PBENMASTIT = pbenmastit,
      PBENABSUBA = pbenabsuba,
      PBENFIBROA = pbenfibroa,
      PBENNECGOR = pbennecgor,
      PBENCONDFI = pbencondfi,
      PBENLESEPI = pbenlesepi,
      PBENOUTRAS = pbenoutras,

      PMALINTUMP = pmalintump,
      PMALINTUMF = pmalintumf,
      PMALINOUTR = pmalinoutr,
      PSUSLEJPCA = psuslejpca,
      PSUSOUTROS = psusoutros,
      PPOSMACDUC = pposmacduc,
      PPOSMACLOB = pposmaclob,
      PPOSMACOUT = pposmacout,

      DEMATACELU = dematacelu,
      DENEGMALIG = denegmalig,
      DEMALIINDT = demaliindt,
      DEPOSMALIG = deposmalig,
      DELESMALIG = delesmalig,
      DEPROCINFL = deprocinfl,

      # histopatologia
      CNEOMALIG  = cneomalig,
      C_NEO_MALI = cneomalig,
      LESBENIGIN = lesbenigin,
      CBENHIPSAT = cbenhipsat,
      CBENHIPCAT = cbenhipcat,
      CBENLOBCAT = cbenlobcat,
      CBENADENOS = cbenadenos,
      CBENESDERO = cbenesdero,
      CBENFIBROC = cbenfibroc,
      CBENFIBROA = cbenfibroa,
      CBENSOLITA = cbensolita,
      CBENMULTI  = cbenmulti,
      CBENFLORID = cbenflorid,
      CBENMASTIT = cbenmastit,
      CBENOUTROS = cbenoutros,

      C_CLI_DETE = cclidetec,
      CCLILOCA   = ccliloca,
      CCLITPEXA  = cclitpexa,
      CCLITANT   = cclitant,
      CCLIDIAGIM = cclidiagim,
      CO_CLI_MAM = co_cli_mam,
      CO_CLI_TAM = cclitam,
      CO_TAM_TUM = ctamtum,
      C_CLI_LINF = cclilinfo,
      CMICMICRCA = cclilinfo,
      CCLIMATPRO = cclimatproc,
      CRESPROCIR = cclimatproc,
      CO_HIS_GRA = chisgrau,
      CO_HIS_MAR = chismarg,
      CHISRECES  = chisreces,
      CHISRECPR  = chisrecpr,

      # mamografia
      CANMEXAPR    = canmexapr,
      TEMPOMAMOGANT = tempomamog,
      INDICLINIC   = indiclinic,
      TIPMAMDIAG   = tipmamdiag,
      CO_CLI_DIA   = cclidiag,
      CRADPELED    = cradpele,
      CRADPELEE    = cradpele,
      CRADCOMPCD   = cradcompc,
      CRADCOMPCE   = cradcompc,
      CATBIRADS    = catbirads,
      CLINFAUXD    = clinfaux,
      CLINFAUXE    = clinfaux,
      CLINFAUXDI   = clinfaux,
      CLINFAUXEI   = clinfaux,
      CNODTAMD     = cnodtam,
      CNODTAME     = cnodtam,
      CCONRECOM    = cconrecom
    )

    df <- apply_lookups(df, columns_to_lookup)

    date_cols <- intersect(
      c("DT_ID_COMP"),
      names(df)
    )
    if (length(date_cols)) {
      df <- to_date_ymd8(df, date_cols, fmt = "%Y%m%d")
    }

    num_cols <- intersect(
      c(
        "ANO_COMP", "QUANTEXAME", "DINTCOLETA", "DINTSOLICT",
        "DINTRESULT", "DINTTEMPEX", "CANMPACANC", "TEMPOMAMOGANT"
      ),
      names(df)
    )
    if (length(num_cols)) {
      df[num_cols] <- lapply(df[num_cols], function(x) suppressWarnings(as.numeric(x)))
    }

    id_cols <- intersect(
      c("CO_US", "CO_CNES", "REGUS", "REGRESID", "REGLAB"),
      names(df)
    )
    if (length(id_cols)) {
      df[id_cols] <- lapply(df[id_cols], as.character)
    }

    if (identical(fonte_in, "SISMAMA-CM")) {
      msg("Subtipo SISMAMA citopatologico detectado.")
    }

    if (identical(fonte_in, "SISMAMA-HM")) {
      msg("Subtipo SISMAMA histopatologico detectado.")
    }

    if (identical(fonte_in, "SISMAMA-MM")) {
      msg("Subtipo SISMAMA mamografia detectado.")
    }

    msg("Bloco SISMAMA concluido.")
    return(df)
  }
  # --------------------------- SINAN ---------------------------
  if (fonte_grp == "SINAN") {

    msg("Processando bloco SINAN...")

    dict_files <- c(
      "TP_NOT","ID_AGRAVO","CS_MENING","CS_AIDS",
      "CS_SUSPEIT_Doenca_Exan","CS_SUSPEIT_Meningite","CS_SUSPEIT_AIDS",
      "CS_SUSPEIT_AIDS(banco DBF)","CS_EXAN","UF_BR","SE_UF_NOT",
      "ID_MUNICIP","CS_SEXO","CS_GESTANT","CS_RACA","CS_ESCOL_N","SG_UF",
      "ID_MN_RESI","CS_ZONA","ID_PAIS","NDUPLIC_N","IN_VINCULA","TPAUTOCTO",
      "COUFINF","COPAISINF","COMUNINF","DOENCA_TRA","CS_FLXRET","CNES24",
      "SIM_NAO","DOSE_APP","TEST_RAPIDO","EVOLUCAO","CODPAISRES2","UTILIZACAO",
      "ATIVIDADE","VIA","TPEXP","CIRCUNSTAN"
    )

    d <- read_dicts(dict_files, base_path)
    conv <- function(name) as_lookup(d[[name]])

    tp_not      <- conv("TP_NOT")
    id_agravo   <- conv("ID_AGRAVO")
    cs_mening   <- conv("CS_MENING")
    cs_aids     <- conv("CS_AIDS")
    cs_suspeit_doenca_exan <- conv("CS_SUSPEIT_Doenca_Exan")
    cs_suspeit_meningite   <- conv("CS_SUSPEIT_Meningite")
    cs_suspeit_aids        <- conv("CS_SUSPEIT_AIDS")
    cs_suspeit_aids_banco  <- conv("CS_SUSPEIT_AIDS(banco DBF)")
    cs_exan     <- conv("CS_EXAN")
    uf_br       <- conv("UF_BR")
    se_uf_not   <- conv("SE_UF_NOT")
    id_municip  <- conv("ID_MUNICIP")
    cs_sexo     <- conv("CS_SEXO")
    cs_gestant  <- conv("CS_GESTANT")
    cs_raca     <- conv("CS_RACA")
    cs_escol_n  <- conv("CS_ESCOL_N")
    sg_uf       <- conv("SG_UF")
    id_mn_resi  <- conv("ID_MN_RESI")
    cs_zona     <- conv("CS_ZONA")
    nduplic_n   <- conv("NDUPLIC_N")
    in_vincula  <- conv("IN_VINCULA")
    tpautocto   <- conv("TPAUTOCTO")
    coufinf     <- conv("COUFINF")
    copaisinf   <- conv("COPAISINF")
    comuninf    <- conv("COMUNINF")
    doenca_tra  <- conv("DOENCA_TRA")
    cs_flxret   <- conv("CS_FLXRET")
    cnes24      <- conv("CNES24")
    sim_nao     <- conv("SIM_NAO")
    dose_app    <- conv("DOSE_APP")
    test_rapido <- conv("TEST_RAPIDO")
    evolucao    <- conv("EVOLUCAO")
    codepaires2 <- conv("CODPAISRES2")
    utilizacao  <- conv("UTILIZACAO")
    atividade   <- conv("ATIVIDADE")
    via         <- conv("VIA")
    tpexp       <- conv("TPEXP")
    circunstan  <- conv("CIRCUNSTAN")

    lookup_map <- list(
      TP_NOT = tp_not,
      ID_AGRAVO = id_agravo,
      CS_MENING = cs_mening,
      CS_AIDS = cs_aids,
      `CS_SUSPEIT_AIDS(banco DBF)` = cs_suspeit_aids_banco,
      CS_SUSPEIT_DOENCA_EXAN = cs_suspeit_doenca_exan,
      CS_SUSPEIT_MENINGITE = cs_suspeit_meningite,
      CS_SUSPEIT_AIDS = cs_suspeit_aids,
      CS_EXAN = cs_exan,
      UF_BR = uf_br,
      SE_UF_NOT = se_uf_not,
      ID_MUNICIP = id_municip,
      CS_SEXO = cs_sexo,
      CS_GESTANT = cs_gestant,
      CS_RACA = cs_raca,
      RACA_MAE = cs_raca,
      ESC_MAE_N = cs_escol_n,
      CS_ESCOL_N = cs_escol_n,
      SG_UF = sg_uf,
      ID_MN_RESI = id_mn_resi,
      CS_ZONA = cs_zona,
      NDUPLIC_N = nduplic_n,
      IN_VINCULA = in_vincula,
      TPAUTOCTO = tpautocto,
      COUFINF = coufinf,
      COPAISINF = copaisinf,
      COMUNINF = comuninf,
      DOENCA_TRA = doenca_tra,
      CS_FLXRET = cs_flxret,
      ID_UNIDADE = cnes24,
      ANT_AC = sim_nao, ANT_BC = sim_nao, ANT_BCG = sim_nao, ANT_TRIPLI = sim_nao,
      ANT_HEMO_T = sim_nao, ANT_CONJ_C = sim_nao,
      CLI_FEBRE = sim_nao, CLI_TOSSE = sim_nao, CLI_DISPNE = sim_nao, CLI_RESPI = sim_nao,
      CLI_CEFALE = sim_nao, CLI_MIAL_G = sim_nao, CLI_LOMBAR = sim_nao,
      CLI_ABDOMI = sim_nao, CLI_HIPOTE = sim_nao, CLI_CHOQUE = sim_nao,
      CLI_VOMITO = sim_nao, CLI_DIARRE = sim_nao, CLI_TORACI = sim_nao,
      CLI_TONTUR = sim_nao, CLI_CARDIA = sim_nao, CLI_RENAL = sim_nao,
      CLI_NEUROL = sim_nao, CLI_ASTENI = sim_nao, CLI_PETEQU = sim_nao,
      CLI_HEMO = sim_nao, CLI_H_DESC = sim_nao, CLI_OUTROS = sim_nao,
      CLI_OUT_D = sim_nao,
      HEPATITE_N = sim_nao, HEPATITA = sim_nao, HEPATITB = sim_nao,
      INSTITUCIO = sim_nao, HIV = sim_nao, OUTRA_DST = sim_nao,
      SEXUAL = sim_nao, DOMICILI = sim_nao, OCUPACIO = sim_nao,
      MEDICAMENT = sim_nao, TATU_PIER = sim_nao, MATBIOLOGI = sim_nao,
      INAL_CRACK = sim_nao, ACUPUNTURA = sim_nao, TRANSFUSAO = sim_nao,
      INJETAVEIS = sim_nao, CIRURGICO = sim_nao, AGUA_ALIME = sim_nao,
      DENTARIO = sim_nao, TRESMAIS = sim_nao, HEMODIALIS = sim_nao,
      TRANSPLA = sim_nao, OUTRAS = sim_nao,
      ANT_DOSE_C = dose_app, ANT_DOSE_4 = dose_app, ANT_DOSE_5 = dose_app, ANT_DOSE_3 = dose_app,
      TPRAPIDO1 = test_rapido, TPRAPIDO2 = test_rapido, TPRAPIDO3 = test_rapido,
      UTILIZACAO = utilizacao,
      ATIVIDA_1 = atividade, ATIVIDA_2 = atividade, ATIVIDA_3 = atividade, ATIVIDADE = atividade,
      VIA_1 = via, VIA_2 = via, VIA_3 = via,
      TPEXP = tpexp,
      CIRCUNSTAN = circunstan,
      ID_PAIS = codepaires2
    )

    df <- apply_lookups(df, lookup_map)

    if ("NU_IDADE_N" %in% names(df)) {
      x <- suppressWarnings(as.integer(df$NU_IDADE_N))
      df$NU_IDADE_N <- paste0(
        x %% 1000, " ",
        dplyr::case_when(
          floor(x / 1000) == 1 ~ "hora(s)",
          floor(x / 1000) == 2 ~ "dia(s)",
          floor(x / 1000) == 3 ~ "mes(es)",
          floor(x / 1000) == 4 ~ "ano(s)",
          TRUE ~ NA_character_
        )
      )
    }

    date_cols <- intersect(
      c("DT_NOTIFIC","DT_SIN_PRI","DT_NASC","DT_OBITO","DT_ENCERRA","DT_INVEST"),
      names(df)
    )

    if (length(date_cols)) {
      df[date_cols] <- lapply(df[date_cols], function(x) {
        as.Date(substr(as.character(x), 1, 8), "%Y%m%d")
      })
    }

    return(df)
  }

  # -------------------- default fallback --------------------
  df <- format_nu_idade_n(df)
  df
}


