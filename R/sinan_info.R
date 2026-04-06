#' List Available SINAN Files from DATASUS FTP
#'
#' The \code{sinan_info()} function accesses the public DATASUS FTP directories
#' for SINAN datasets and returns a structured table with the available files
#' in the final, preliminary, or both folders.
#'
#' It performs:
#' \itemize{
#'   \item File listing from SINAN FTP directories
#'   \item Parsing of filenames into dataset code, state, year, and month
#'   \item Standardization into a structured table
#'   \item Optional caching for faster repeated access
#'   \item Deduplication and summary of available years
#' }
#'
#' @param folder Character string indicating which SINAN folder to inspect.
#'   Must be one of \code{"FINAIS"}, \code{"PRELIM"}, or \code{"BOTH"}.
#' @param cache Logical; if TRUE, stores results locally as cache files.
#' @param cache_dir Character string specifying the directory used to store cached results.
#' @param refresh Logical; if TRUE, forces refreshing cached results.
#' @param verbose Logical; if TRUE, prints progress messages.
#'
#' @return A \code{data.table} containing:
#' \itemize{
#'   \item filename: file name
#'   \item ext: file extension (.dbc/.DBC)
#'   \item code: SINAN dataset code
#'   \item uf: state abbreviation, "BR", or \code{NA} when unavailable
#'   \item year: year extracted from the file name
#'   \item month: month extracted from the file name, when available
#'   \item folder: source FTP folder ("FINAIS" or "PRELIM")
#'   \item available_years: comma-separated summary of available years by folder, code, and UF
#' }
#'
#' @details
#' This function uses the public DATASUS FTP server to inspect SINAN files in:
#' \itemize{
#'   \item \code{FINAIS}: finalized datasets
#'   \item \code{PRELIM}: preliminary datasets
#' }
#'
#' Results may be cached locally as RDS files in order to reduce repeated FTP
#' requests. Use \code{refresh = TRUE} to force a new query.
#'
#' File names are parsed using SINAN naming conventions, typically including
#' dataset code, UF, year, and optional month. Rare patterns without a UF
#' identifier are also supported.
#'
#' @examples
#' \dontrun{
#' library(megadatasus)
#'
#' # List finalized SINAN files
#' finais <- sinan_info("FINAIS")
#'
#' # List preliminary SINAN files
#' prelim <- sinan_info("PRELIM")
#'
#' # List both folders
#' all_sinan <- sinan_info("BOTH")
#'
#' # Refresh cache
#' finais <- sinan_info("FINAIS", refresh = TRUE)
#' }
#'
#' @seealso \code{\link{Datasus_info}}, \code{\link{acesso_datasus}}
#'
#' @export
sinan_info <- function(folder = c("FINAIS", "PRELIM", "BOTH"),
                       cache = TRUE,
                       cache_dir = "C:/DATASUS/cache",
                       refresh = FALSE,
                       verbose = TRUE) {

  folder <- match.arg(folder)

  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop("Instale o pacote RCurl: install.packages('RCurl')")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Instale o pacote data.table: install.packages('data.table')")
  }

  ftp_finais <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
  ftp_prelim <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  cache_file <- function(which) {
    file.path(cache_dir, paste0("SINAN_INFO_", which, ".rds"))
  }

  ftp_list <- function(dir_url) {
    x <- tryCatch(
      RCurl::getURL(dir_url, ftp.use.epsv = FALSE, dirlistonly = TRUE),
      error = function(e) ""
    )

    if (!nzchar(x)) return(character(0))

    files <- unlist(strsplit(x, "\r\n|\n"))
    files <- trimws(files)
    files[nzchar(files)]
  }

  parse_sinan_filename <- function(fn) {
    if (!grepl("\\.(dbc|DBC)$", fn)) return(NULL)

    ext <- if (grepl("\\.DBC$", fn)) ".DBC" else ".dbc"
    base <- sub("\\.(dbc|DBC)$", "", fn)
    base_up <- toupper(base)

    # Padrão A: CODE + UF/BR + YEAR + optional MM
    m <- regexec("^([A-Z]+)(BR|[A-Z]{2})(\\d{2}|\\d{4})(\\d{2})?$", base_up)
    r <- regmatches(base_up, m)[[1]]

    if (length(r) > 0) {
      code <- r[2]
      uf <- r[3]
      yyraw <- r[4]
      mm <- if (length(r) >= 5) r[5] else NA_character_
    } else {
      # Padrão B: CODE + YEAR + optional MM
      m2 <- regexec("^([A-Z]+)(\\d{2}|\\d{4})(\\d{2})?$", base_up)
      r2 <- regmatches(base_up, m2)[[1]]

      if (length(r2) == 0) return(NULL)

      code <- r2[2]
      uf <- NA_character_
      yyraw <- r2[3]
      mm <- if (length(r2) >= 4) r2[4] else NA_character_
    }

    year <- suppressWarnings(as.integer(yyraw))
    if (is.na(year)) return(NULL)

    if (nchar(yyraw) == 2) year <- 2000L + year

    month <- suppressWarnings(as.integer(mm))
    if (is.na(month)) month <- NA_integer_

    data.table::data.table(
      filename = fn,
      ext = ext,
      code = code,
      uf = uf,
      year = year,
      month = month
    )
  }

  build_table_from_files <- function(files, which_folder) {
    if (length(files) == 0) return(data.table::data.table())

    parsed <- lapply(files, parse_sinan_filename)
    parsed <- parsed[!vapply(parsed, is.null, logical(1))]

    if (length(parsed) == 0) return(data.table::data.table())

    dt <- data.table::rbindlist(parsed, fill = TRUE)
    dt$folder <- which_folder
    dt
  }

  get_folder_dt <- function(which) {
    cf <- cache_file(which)

    if (cache && !refresh && file.exists(cf)) {
      if (verbose) {
        message("Using cached SINAN_INFO for ", which, " (set refresh=TRUE to update).")
      }
      obj <- readRDS(cf)
      return(data.table::as.data.table(obj))
    }

    if (verbose) message("Listing FTP folder: ", which)

    if (which == "FINAIS") {
      files <- ftp_list(ftp_finais)
      dt <- build_table_from_files(files, "FINAIS")
      if (cache) saveRDS(dt, cf)
      return(dt)
    }

    if (which == "PRELIM") {
      files <- ftp_list(ftp_prelim)
      dt <- build_table_from_files(files, "PRELIM")
      if (cache) saveRDS(dt, cf)
      return(dt)
    }

    stop("Unknown folder: ", which)
  }

  if (folder == "FINAIS") {
    dt <- get_folder_dt("FINAIS")
  } else if (folder == "PRELIM") {
    dt <- get_folder_dt("PRELIM")
  } else {
    dt <- data.table::rbindlist(
      list(get_folder_dt("FINAIS"), get_folder_dt("PRELIM")),
      fill = TRUE
    )
  }

  if (nrow(dt) == 0) {
    if (verbose) {
      message("No .dbc files parsed. If this is unexpected, your network may block FTP or the listing failed.")
    }
    return(dt)
  }

  dt <- unique(dt)

  # resumo de anos disponíveis sem usar :=
  split_keys <- interaction(dt$folder, dt$code, dt$uf, drop = TRUE, lex.order = TRUE)

  available_years_vec <- vapply(
    split(dt$year, split_keys),
    function(x) paste(sort(unique(stats::na.omit(x))), collapse = ", "),
    character(1)
  )

  key_df <- unique(data.frame(
    folder = dt$folder,
    code = dt$code,
    uf = dt$uf,
    split_key = split_keys,
    stringsAsFactors = FALSE
  ))

  key_df$available_years <- available_years_vec[key_df$split_key]
  key_df$split_key <- NULL

  dt_df <- as.data.frame(dt, stringsAsFactors = FALSE)
  dt_df <- merge(
    dt_df,
    key_df,
    by = c("folder", "code", "uf"),
    all.x = TRUE,
    sort = FALSE
  )

  dt <- data.table::as.data.table(dt_df)

  # ordenar colunas
  wanted <- c("folder", "code", "uf", "year", "month", "available_years", "filename", "ext")
  wanted <- wanted[wanted %in% names(dt)]
  dt <- dt[, wanted, with = FALSE]

  dt[]
}


