#' Download and Load SINAN Data from DATASUS FTP
#'
#' The \code{acesso_sinan()} function downloads SINAN datasets directly
#' from the DATASUS FTP server, reads the corresponding \code{.dbc} files,
#' and returns a combined table for the requested source, states, and years.
#'
#' It supports:
#' \itemize{
#'   \item Downloading SINAN datasets in final or preliminary versions
#'   \item Automatic fallback between \code{FINAIS} and \code{PRELIM} folders
#'   \item Local file caching
#'   \item Parallel downloads and file reading
#'   \item Optional cleanup of downloaded files after import
#' }
#'
#' @param Fonte Character string indicating the SINAN source in the format
#'   \code{"SINAN-XXXX"} (e.g., \code{"SINAN-DENG"}, \code{"SINAN-ZIKA"}).
#' @param UF Character vector of state abbreviations or \code{"BR"} for nationwide data.
#' @param ano_inicio Integer indicating the first year to download.
#' @param ano_final Integer indicating the last year to download.
#' @param cache_dir Character string specifying the local cache directory used
#'   to store downloaded files.
#' @param prefer Character string indicating which FTP folder should be tried first.
#'   Must be one of \code{"FINAIS"} or \code{"PRELIM"}.
#' @param delay Numeric value (in seconds) to wait before each download attempt.
#' @param clean Logical; if TRUE, deletes downloaded \code{.dbc} files after reading.
#' @param n_cores Integer specifying the number of CPU cores to use for parallel processing.
#' @param verbose Logical; if TRUE, prints progress messages.
#'
#' @return A \code{data.table} containing the combined SINAN data for all
#'   successfully downloaded files. Returns an empty \code{data.frame} if no
#'   data is retrieved.
#'
#' @details
#' This function accesses the public DATASUS FTP server and searches for SINAN
#' files in:
#' \itemize{
#'   \item \code{FINAIS}: finalized SINAN datasets
#'   \item \code{PRELIM}: preliminary SINAN datasets
#' }
#'
#' For each requested combination of UF and year, the function generates a set
#' of likely file names, attempts to download them from the preferred FTP folder,
#' and falls back to the alternative folder if needed.
#'
#' Downloaded files are read using \code{read.dbc::read.dbc()}, and metadata
#' columns are added:
#' \itemize{
#'   \item \code{SINAN_FONTE}: requested source
#'   \item \code{SINAN_UF}: requested UF
#'   \item \code{SINAN_ANO}: requested year
#' }
#'
#' Parallel execution uses \code{parLapply()} on Windows and \code{mclapply()}
#' on Unix-like systems.
#'
#' @examples
#' \dontrun{
#' library(megadatasus)
#'
#' # Download dengue data for Brazil
#' df <- acesso_sinan(
#'   Fonte = "SINAN-DENG",
#'   UF = "BR",
#'   ano_inicio = 2020,
#'   ano_final = 2021
#' )
#'
#' # Download Zika data for Brazil ( 2020-2021)
#' df <- acesso_sinan(
#'   Fonte = "SINAN-ZIKA",
#'   UF = "BR",
#'   ano_inicio = 2020,
#'   ano_final = 2021
#' )
#'
#' }
#'
#' @seealso \code{\link{sinan_info}}, \code{\link{acesso_datasus}}
#'
#' @export
acesso_sinan <- function(Fonte, UF = "BR", ano_inicio, ano_final, cache_dir = "C:/DATASUS/cache",prefer = c("FINAIS", "PRELIM"),
                          delay = 0, clean = FALSE, n_cores = max(1, parallel::detectCores() - 1),verbose = TRUE) {


  #list_fonte <- c("SINAN-AIDC" ,"SINAN-ANIM" ,"SINAN-ANTR", "SINAN-BOTU", "SINAN-CANC" ,"SINAN-RAIV",
  #          "SINAN-CHAG", "SINAN-CHIK", "SINAN-COLE", "SINAN-COQU", "SINAN-DENG", "SINAN-DERM", "
  #        SINAN-ESPO", "SINAN-ESQU", "SINAN-EXAN", "SINAN-FMAC", "SINAN-FTIF", "SINAN-HANS",
  #         "SINAN-HANT", "SINAN-HEPA", "SINAN-HIVA", "SINAN-HIVC", "SINAN-HIVE", "SINAN-HIVG",
  #          "SINAN-IEXO",  "SINAN-LEPT", "SINAN-LERD", "SINAN-LTAN", "SINAN-MALA", "SINAN-MENI",
  #          "SINAN-MENT", "SINAN-NTRA", "SINAN-PAIR", "SINAN-PEST", "SINAN-PFAN", "SINAN-PNEU",
  #         "SINAN-ROTA", "SINAN-SDTA", "SINAN-SIFA", "SINAN-SIFC", "SINAN-SIFG", "SINAN-SRC",
  #        "SINAN-TETA", "SINAN-TETN", "SINAN-TOXC", "SINAN-TOXG", "SINAN-TRAC", "SINAN-TUBE",
  #       "SINAN-VARC", "SINAN-VIOL", "SINAN-ZIKA","SINAN-AIDA","SINAN-ACGR","SINAN-ACBI")


  prefer <- match.arg(prefer)

  if (!requireNamespace("curl", quietly = TRUE)) stop("Instale o pacote curl.")
  if (!requireNamespace("read.dbc", quietly = TRUE)) stop("Instale o pacote read.dbc.")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Instale o pacote data.table.")

  if (!startsWith(Fonte, "SINAN-")) stop("Use Fonte no formato 'SINAN-XXXX'.")
  code <- sub("^SINAN-", "", Fonte)

  ftp_finais <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
  ftp_prelim <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
  ftp_order <- if (prefer == "FINAIS") c(ftp_finais, ftp_prelim) else c(ftp_prelim, ftp_finais)

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  download_dir <- file.path(cache_dir, Fonte)
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  years <- seq(ano_inicio, ano_final)

  # Build tasks: one per (UF, year)
  tasks <- expand.grid(UF = UF, YEAR = years, stringsAsFactors = FALSE)

  worker <- function(uf, year) {

    if (delay > 0) Sys.sleep(delay)

    yy <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
    yyyy <- as.character(year)

    # Minimal candidates first (most common)
    candidates <- c(
      paste0(code, uf, yy, ".dbc"),
      paste0(code, uf, yy, ".DBC"),
      paste0(code, uf, yyyy, ".dbc"),
      paste0(code, uf, yyyy, ".DBC")
    )

    downloaded_path <- NA_character_


    for (cand in candidates) {
      dest <- file.path(download_dir, cand)

      # cache hit
      if (file.exists(dest) && file.info(dest)$size > 0) {
        downloaded_path <- dest
        break
      }

      ok <- FALSE
      for (base in ftp_order) {
        url <- paste0(base, cand)
        ok <- tryCatch({
          curl::curl_download(url, destfile = dest, quiet = TRUE)
          file.exists(dest) && file.info(dest)$size > 0
        }, error = function(e) FALSE)

        if (ok) {
          downloaded_path <- dest
          break
        }
      }

      if (!is.na(downloaded_path)) break
    }

    if (is.na(downloaded_path)) {
      if (verbose) message(Fonte, ": nao encontrou/baixou UF=", uf, " ano=", year)
      return(NULL)
    }

    df <- tryCatch(read.dbc::read.dbc(downloaded_path), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) {
      if (clean) unlink(downloaded_path, force = TRUE)
      return(NULL)
    }

    df$SINAN_FONTE <- Fonte
    df$SINAN_UF <- uf
    df$SINAN_ANO <- year

    if (clean) unlink(downloaded_path, force = TRUE)
    df
  }

  # --- Parallel execution (Windows-safe) ---
  is_windows <- identical(.Platform$OS.type, "windows")
  results <- NULL

  if (is_windows) {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Load packages on workers
    parallel::clusterEvalQ(cl, {
      library(curl)
      library(read.dbc)
    })

    # Export needed objects/functions
    parallel::clusterExport(
      cl,
      varlist = c("worker", "Fonte", "code", "ftp_order", "download_dir",
                  "prefer", "delay", "clean", "verbose"),
      envir = environment()
    )

    results <- parallel::parLapply(
      cl,
      X = seq_len(nrow(tasks)),
      fun = function(i) worker(tasks$UF[i], tasks$YEAR[i])
    )
  } else {
    # macOS/Linux
    results <- parallel::mclapply(
      X = seq_len(nrow(tasks)),
      FUN = function(i) worker(tasks$UF[i], tasks$YEAR[i]),
      mc.cores = n_cores
    )
  }

  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) return(data.frame())

  data.table::rbindlist(results, fill = TRUE)
}



