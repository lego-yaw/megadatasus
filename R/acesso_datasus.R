#' Download and Load DATASUS Data (DBC Files)
#'
#' The \code{acesso_datasus()} function downloads DATASUS datasets directly
#' from the official FTP servers, reads the compressed \code{.dbc} files,
#' and returns them as a combined data.frame.
#'
#' It supports multiple DATASUS systems (e.g., SIM, SINASC, SIHSUS, SIASUS,
#' CNES, e-SUS, etc.) and automatically handles:
#' \itemize{
#'   \item FTP directory selection based on dataset and year
#'   \item File name pattern matching and variations
#'   \item Monthly or yearly data retrieval
#'   \item Reading \code{.dbc} files into R
#'   \item Combining multiple files into a single dataset
#' }
#'
#' @param Fonte Character string indicating the DATASUS source
#'   (e.g., "SIM-DO", "SINASC-DN", "SIHSUS-RD", "CNES-ST", "e-SUS").
#' @param UF Character vector of state abbreviations or \code{"BR"} for nationwide data.
#' @param ano_inicio Integer indicating the first year to download.
#' @param ano_final Integer indicating the last year to download.
#' @param mes_inicial Optional month (numeric "01"-"12" or abbreviated name "Jan"-"Dec")..
#' @param mes_final Optional month (same format as \code{mes_inicial}).
#' @param clean Logical; if TRUE, removes temporary downloaded \code{.dbc} files.
#' @param delay Numeric value (seconds) between downloads to avoid server overload.
#' @param temp_dir Directory used to temporarily store downloaded files.
#' @param quiet Logical; if FALSE, prints progress messages.
#'
#' @return A \code{data.frame} containing the combined data from all downloaded
#'   DATASUS files. Returns an empty data.frame if no data is retrieved.
#'
#' @details
#' This function accesses the DATASUS public FTP server:
#' \url{ftp://ftp.datasus.gov.br}
#'
#' The function dynamically determines:
#' \itemize{
#'   \item The correct FTP directory based on dataset and year
#'   \item File naming conventions (including variations across systems)
#'   \item Whether data is monthly or yearly
#' }
#'
#' Special handling is implemented for certain datasets, including:
#' \itemize{
#'   \item SIM (CID9/CID10 variations)
#'   \item SINASC
#'   \item CNES
#'   \item e-SUS Notifica (ESUSNOTIFICA)
#' }
#'
#' Downloaded files are read using \code{read.dbc::read.dbc()} and combined
#' into a unified dataset with aligned columns.
#'
#' @examples
#' \dontrun{
#' library(megadatasus)
#'
#' # Download SIM data for Rio de Janeiro ("2005")
#' df <- acesso_datasus(
#'   Fonte = "SIM-DO",
#'   UF = "RJ",
#'   ano_inicio = 2005,
#'   ano_final = 2005
#' )
#'
#' # Download SINASC data for Rio de Janeiro (monthly)
#' df <- acesso_datasus(
#'   Fonte = "SINASC-DN",
#'   UF = "MG",
#'   ano_inicio = 2019,
#'   ano_final = 2019,
#'   mes_inicial = "01",
#'   mes_final = "06",
#'   quiet = FALSE
#' )
#'
#' # Download e-SUS data
#' df <- acesso_datasus(
#'   Fonte = "e-SUS",
#'   UF = "BR",
#'   ano_inicio = 2023,
#'   ano_final = 2023
#' )
#' }
#'
#' @seealso \code{\link{Datasus_info}}, \code{\link{clean_table}}, \code{\link{acesso_sinan}}, \code{\link{acesso_IBGE}}
#'
#' @export
acesso_datasus <- function(Fonte, UF = "BR", ano_inicio, ano_final,
                           mes_inicial = NULL, mes_final = NULL,
                           clean = TRUE, delay = 0.5,
                           temp_dir = tempdir(), quiet = TRUE) {

  if (!requireNamespace("RCurl", quietly = TRUE)) stop("Instale o pacote RCurl.")
  if (!requireNamespace("read.dbc", quietly = TRUE)) {
    stop(
      "Install read.dbc from GitHub with: remotes::install_github('danicat/read.dbc')",
      call. = FALSE
    )
  }

  list_uf <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA",
               "PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO","BR")

  if (is.null(UF)) UF <- "BR"
  UF <- toupper(UF)

  if (any(!(UF %in% list_uf))) stop("Uma ou mais UF inválidas.")
  if (!is.numeric(ano_inicio) || !is.numeric(ano_final) || ano_inicio > ano_final) {
    stop("Intervalo de ano inválido.")
  }
  if (!is.logical(clean) || length(clean) != 1) stop("clean deve ser TRUE/FALSE.")
  if (!is.numeric(delay) || length(delay) != 1 || delay < 0) stop("delay deve ser numérico >= 0.")

  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  }

  months_numeric <- sprintf("%02d", 1:12)
  months_map <- c("Jan"="01","Fev"="02","Mar"="03","Abr"="04","Mai"="05","Jun"="06",
                  "Jul"="07","Ago"="08","Set"="09","Out"="10","Nov"="11","Dez"="12")

  convert_month <- function(mes) {
    if (is.null(mes)) return(NULL)
    mes <- as.character(mes)
    if (mes %in% months_numeric) return(mes)
    if (mes %in% names(months_map)) return(unname(months_map[[mes]]))
    stop("Mês inválido. Use '01'..'12' ou 'Jan'..'Dez'.")
  }

  mi <- convert_month(mes_inicial); if (is.null(mi)) mi <- "01"
  mf <- convert_month(mes_final);   if (is.null(mf)) mf <- "12"
  mi_n <- as.integer(mi)
  mf_n <- as.integer(mf)

  msg <- function(...) if (!quiet) message(...)

  # =========================
  # NORMALIZA FONTE
  # =========================
  Fonte0 <- Fonte
  Fonte_up <- toupper(gsub("\\s+", "", Fonte0))
  Fonte_up <- gsub("_", "-", Fonte_up)

  if (Fonte_up %in% c("E-SUS","ESUS","E-SUSNOTIFICA","ESUSNOTIFICA","ESUSNOTIFICA-P","ESUSNOTIFICA_P")) {
    Fonte_up <- "ESUSNOTIFICA"
  }
  Fonte_up <- gsub("^E-SUS", "ESUSNOTIFICA", Fonte_up)

  # =========================
  # SEPARA FAMÍLIA / PREFIXO
  # =========================
  if (grepl("-", Fonte_up, fixed = TRUE)) {
    familia <- sub("-.*$", "", Fonte_up)
    prefixo <- sub("^.*-", "", Fonte_up)
  } else {
    familia <- Fonte_up
    prefixo <- Fonte_up
  }

  esus_sem_prefixo <- (familia == "ESUSNOTIFICA" && prefixo == "ESUSNOTIFICA")

  # =========================
  # FTP DIR
  # =========================
  get_ftp_dir <- function(familia, prefixo, year) {

    if (familia == "SIHSUS") {
      if (year <= 2007) {
        return("ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/")
      } else {
        return("ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/")
      }
    }

    if (familia == "SIASUS") {
      return("ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/")
    }

    if (familia == "ESUSNOTIFICA") {
      return("ftp://ftp.datasus.gov.br/dissemin/publicos/ESUSNOTIFICA/DADOS/PRELIM/")
    }

    if (familia == "SISCOLO") {
      return("ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISCOLO4/Dados/")
    }

    if (familia == "SISMAMA") {
      return("ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISMAMA/Dados/")
    }

    if (familia == "CIH") {
      return("ftp://ftp.datasus.gov.br/dissemin/publicos/CIH/200801_201012/Dados/")
    }

    if (familia == "CIHA") {
      return("ftp://ftp.datasus.gov.br/dissemin/publicos/CIHA/200801_/Dados/")
    }

    if (familia == "PO") {
      return("ftp://ftp.datasus.gov.br/dissemin/publicos/PAINEL_ONCOLOGIA/DADOS/")
    }

    if (familia == "SIM") {
      cid_base <- if (year <= 1995) "CID9" else "CID10"

      if (prefixo %in% c("DO","DOEXT","DOINF","DOMAT","DOREXT")) {
        return(paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/", cid_base, "/DORES/"))
      }

      if (prefixo %in% c("DOFET")) {
        return(paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/", cid_base, "/DOFET/"))
      }

      return(paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/", cid_base, "/DORES/"))
    }

    if (familia == "SINASC") {
      return("ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/")
    }

    if (familia == "RESP") {
      return("ftp://ftp.datasus.gov.br/dissemin/publicos/RESP/DADOS/")
    }

    if (familia == "SISPRENATAL") {
      return("ftp://ftp.datasus.gov.br/dissemin/publicos/SISPRENATAL/201201_/Dados/")
    }

    if (familia == "PCE") {
      return("ftp://ftp.datasus.gov.br/dissemin/publicos/PCE/DADOS/")
    }

    if (familia == "CNES") {
      return(paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/", prefixo, "/"))
    }

    stop(paste0("Família '", familia, "' não mapeada no get_ftp_dir()."))
  }

  # =========================
  # BUILD CANDIDATES
  # =========================
  candidates_builder <- function(familia, prefixo, uf, year, mm) {
    yy <- sprintf("%02d", as.integer(substr(as.character(year), 3, 4)))
    yyyy <- as.character(year)

    mk <- function(...) {
      x <- c(...)
      unique(c(x, toupper(x)))
    }

    if (familia == "ESUSNOTIFICA") {
      return(mk(
        paste0(prefixo, uf, yy, ".dbc"),
        paste0(prefixo, uf, yyyy, ".dbc"),
        paste0(prefixo, uf, yy, ".DBC"),
        paste0(prefixo, uf, yyyy, ".DBC")
      ))
    }

    if (familia %in% c("SIASUS","SIHSUS","CNES","SISCOLO","SISMAMA","SINASC")) {
      return(mk(
        paste0(prefixo, uf, yy, mm, ".dbc"),
        paste0(prefixo, uf, yy, ".dbc"),
        paste0(prefixo, uf, yyyy, mm, ".dbc"),
        paste0(prefixo, uf, yyyy, ".dbc")
      ))
    }

    if (familia == "SIM") {
      alt_prefixos <- prefixo
      if (startsWith(prefixo, "DO")) {
        alt_prefixos <- unique(c(prefixo, sub("^DO", "DOR", prefixo)))
      }

      out <- c()
      for (p in alt_prefixos) {
        out <- c(out, mk(
          paste0(p, uf, yy, mm, ".dbc"),
          paste0(p, uf, yy, ".dbc"),
          paste0(p, uf, yyyy, mm, ".dbc"),
          paste0(p, uf, yyyy, ".dbc")
        ))
      }
      return(unique(out))
    }

    if (familia %in% c("CIH","CIHA")) {
      return(mk(
        paste0(prefixo, uf, yy, mm, ".dbc"),
        paste0(prefixo, uf, yy, ".dbc")
      ))
    }

    if (familia %in% c("RESP","PCE","SISPRENATAL","PO")) {
      return(mk(
        paste0(prefixo, uf, yy, mm, ".dbc"),
        paste0(prefixo, uf, yy, ".dbc"),
        paste0(prefixo, yy, mm, ".dbc"),
        paste0(prefixo, yy, ".dbc"),
        paste0(prefixo, uf, yyyy, ".dbc")
      ))
    }

    return(mk(
      paste0(prefixo, uf, yy, mm, ".dbc"),
      paste0(prefixo, uf, yy, ".dbc"),
      paste0(prefixo, uf, yyyy, ".dbc")
    ))
  }

  # =========================
  # SAFE DOWNLOAD
  # =========================
  safe_download_read <- function(url, dest, fname) {
    if (!dir.exists(dirname(dest))) {
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    }

    ok <- tryCatch({
      utils::download.file(url = url, destfile = dest, mode = "wb", quiet = quiet)
      file.exists(dest)
    }, error = function(e) {
      msg("Erro download ", fname, ": ", e$message)
      FALSE
    })

    if (!ok || !file.exists(dest)) return(NULL)

    df <- tryCatch({
      read.dbc::read.dbc(dest)
    }, error = function(e) {
      msg("Erro lendo ", fname, ": ", e$message)
      NULL
    })

    if (file.exists(dest)) unlink(dest)

    df
  }

  # =========================
  # DOWNLOAD LOOP
  # =========================
  out_list <- list()

  for (year in seq(ano_inicio, ano_final)) {

    ftp_dir <- get_ftp_dir(familia, prefixo, year)

    files_on_ftp <- tryCatch({
      x <- RCurl::getURL(ftp_dir, ftp.use.epsv = FALSE, dirlistonly = TRUE)
      f <- unlist(strsplit(x, "\r\n|\n"))
      f <- trimws(f)
      f[nzchar(f)]
    }, error = function(e) {
      msg("Erro listando FTP: ", ftp_dir, " :: ", e$message)
      character(0)
    })

    if (length(files_on_ftp) == 0) next

    yy <- sprintf("%02d", as.integer(substr(as.character(year), 3, 4)))
    yyyy <- as.character(year)

    for (uf in UF) {

      # ESUS sem prefixo: baixa todos do ano/UF
      if (familia == "ESUSNOTIFICA" && esus_sem_prefixo) {
        patt <- paste0("(", uf, "(", yy, "|", yyyy, "))\\.(dbc|DBC)$")
        pick <- files_on_ftp[grepl(patt, files_on_ftp, ignore.case = TRUE)]

        if (length(pick) == 0) next

        for (fname in pick) {
          Sys.sleep(delay)
          url  <- paste0(ftp_dir, fname)
          dest <- file.path(temp_dir, fname)

          df <- safe_download_read(url, dest, fname)

          if (!is.null(df) && nrow(df) > 0) {
            out_list[[length(out_list) + 1]] <- df
            msg("OK: ", fname, " (", nrow(df), " linhas)")
          }
        }

        next
      }

      # ESUS com prefixo: 1 arquivo por ano
      if (familia == "ESUSNOTIFICA" && !esus_sem_prefixo) {
        candidates <- candidates_builder(familia, prefixo, uf, year, mm = "01")
        fname <- candidates[candidates %in% files_on_ftp][1]

        if (is.na(fname) || length(fname) == 0) next

        Sys.sleep(delay)
        url  <- paste0(ftp_dir, fname)
        dest <- file.path(temp_dir, fname)

        df <- safe_download_read(url, dest, fname)

        if (!is.null(df) && nrow(df) > 0) {
          out_list[[length(out_list) + 1]] <- df
          msg("OK: ", fname, " (", nrow(df), " linhas)")
        }

        next
      }

      # SIM yearly files: do not repeat across months
      if (familia == "SIM") {
        candidates <- candidates_builder(familia, prefixo, uf, year, mm = "01")
        fname <- candidates[candidates %in% files_on_ftp][1]

        if (!is.na(fname) && length(fname) > 0) {
          Sys.sleep(delay)
          url  <- paste0(ftp_dir, fname)
          dest <- file.path(temp_dir, fname)

          df <- safe_download_read(url, dest, fname)

          if (!is.null(df) && nrow(df) > 0) {
            out_list[[length(out_list) + 1]] <- df
            msg("OK: ", fname, " (", nrow(df), " linhas)")
          }
        }

        next
      }

      # Restante: loop mensal
      for (m in seq(mi_n, mf_n)) {
        mm <- sprintf("%02d", m)

        candidates <- candidates_builder(familia, prefixo, uf, year, mm)
        fname <- candidates[candidates %in% files_on_ftp][1]

        if (is.na(fname) || length(fname) == 0) next

        Sys.sleep(delay)
        url  <- paste0(ftp_dir, fname)
        dest <- file.path(temp_dir, fname)

        df <- safe_download_read(url, dest, fname)

        if (!is.null(df) && nrow(df) > 0) {
          out_list[[length(out_list) + 1]] <- df
          msg("OK: ", fname, " (", nrow(df), " linhas)")
        }
      }
    }
  }

  if (length(out_list) == 0) return(data.frame())

  all_cols <- unique(unlist(lapply(out_list, names)))

  out_list <- lapply(out_list, function(d) {
    miss <- setdiff(all_cols, names(d))
    if (length(miss) > 0) d[miss] <- NA
    d[, all_cols, drop = FALSE]
  })

  final <- do.call(rbind, out_list)
  rownames(final) <- NULL

  if (clean && dir.exists(temp_dir)) {
    unlink(list.files(temp_dir, pattern = "\\.dbc$|\\.DBC$", full.names = TRUE))
  }

  return(final)
}


