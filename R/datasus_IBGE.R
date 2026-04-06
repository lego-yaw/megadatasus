#' Download IBGE Files from DATASUS FTP
#'
#' The \code{acesso_IBGE()} function downloads IBGE-related compressed files
#' made available through the DATASUS FTP server for a specified source,
#' year range, and geographic code.
#'
#' It performs:
#' \itemize{
#'   \item Validation of source, year range, and destination directory
#'   \item Construction of DATASUS FTP file names and URLs
#'   \item Download of ZIP files for the requested years
#'   \item Optional overwrite control for existing files
#'   \item Logging of download status for each requested year
#' }
#'
#' @param Fonte Character string indicating the IBGE source. Must be one of
#'   \code{"IBGE-POP"}, \code{"IBGE-POPS"}, or \code{"IBGE-POPT"}.
#' @param ano_inicio Integer indicating the first year to download.
#' @param ano_final Integer indicating the last year to download.
#' @param dest_dir Character string specifying the destination directory where
#'   ZIP files will be saved.
#' @param UF Character string indicating the state abbreviation or \code{"BR"}
#'   for nationwide files. Default is \code{"BR"}.
#' @param overwrite Logical; if TRUE, existing files are overwritten.
#' @param delay Numeric value indicating the delay in seconds between downloads.
#' @param quiet Logical; if FALSE, prints progress messages.
#'
#' @return A \code{data.frame} containing one row per requested year, with:
#' \itemize{
#'   \item ANO: requested year
#'   \item ZIP: ZIP file name
#'   \item URL: source FTP URL
#'   \item PATH: local file path
#'   \item STATUS: download status
#' }
#'
#' @details
#' This function uses the public DATASUS FTP server to download IBGE population files
#' from one of the following sources:
#' \itemize{
#'   \item \code{IBGE-POP}: general population data
#'   \item \code{IBGE-POPS}: population data from POPSVS
#'   \item \code{IBGE-POPT}: population data from POPTCU
#' }
#'
#' File names are constructed according to the DATASUS naming convention
#' based on source, UF, and two-digit year suffix.
#'
#' Existing files may be skipped unless \code{overwrite = TRUE}.
#'
#' @examples
#' \dontrun{
#' library(megadatasus)
#'
#' # Download nationwide IBGE-POP files for 2010 to 2012
#' res <- acesso_IBGE(
#'   Fonte = "IBGE-POP",
#'   UF = "BR",
#'   ano_inicio = 2010,
#'   ano_final = 2012,
#'   dest_dir = tempdir()
#' )
#'
#' # Download Rio de Janeiro files
#' res <- acesso_IBGE(
#'   Fonte = "IBGE-POPT",
#'   UF = "BR",
#'   ano_inicio = 2018,
#'   ano_final = 2020,
#'   dest_dir = tempdir(),
#'   overwrite = TRUE,
#'   quiet = FALSE
#' )
#' }
#'
#' @seealso \code{\link{Datasus_info}}
#'
#' @export
acesso_IBGE <- function(Fonte, ano_inicio, ano_final,dest_dir,UF = "BR", overwrite = FALSE,
                       delay = 0, quiet = TRUE) {

  # -------------------------
  # Validate inputs
  # -------------------------
  if (!Fonte %in% c("IBGE-POP", "IBGE-POPS", "IBGE-POPT")) {
    stop("Fonte invalida. Use: IBGE-POP, IBGE-POPS ou IBGE-POPT.")
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(dest_dir)) stop("Nao foi possivel criar/acessar dest_dir.")

  msg <- function(...) if (!quiet) message(...)

  # -------------------------
  # Internal FTP mapping
  # -------------------------
  ftp_map <- list(
    "IBGE-POP"  = list(base = "POP",  ftp = "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POP/"),
    "IBGE-POPS" = list(base = "POPS", ftp = "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/"),
    "IBGE-POPT" = list(base = "POPT", ftp = "ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPTCU/")
  )

  base_name <- ftp_map[[Fonte]]$base
  ftp_dir   <- ftp_map[[Fonte]]$ftp

  # -------------------------
  # Download loop
  # -------------------------
  results <- list()

  for (year in seq(ano_inicio, ano_final)) {
    yy <- sprintf("%02d", as.integer(substr(as.character(year), 3, 4)))

    zip_name <- paste0(base_name, UF, yy, ".zip")
    zip_url  <- paste0(ftp_dir, zip_name)
    zip_path <- file.path(dest_dir, zip_name)

    if (file.exists(zip_path) && !overwrite) {
      msg("Already exists (skipped): ", zip_name)
      results[[length(results) + 1]] <- data.frame(
        ANO = year,
        ZIP = zip_name,
        URL = zip_url,
        PATH = zip_path,
        STATUS = "SKIPPED_EXISTS",
        stringsAsFactors = FALSE
      )
      next
    }

    if (delay > 0) Sys.sleep(delay)

    msg("Downloading: ", zip_url)

    ok <- tryCatch({
      utils::download.file(zip_url, zip_path, mode = "wb", quiet = TRUE)
      TRUE
    }, warning = function(w) {
      TRUE
    }, error = function(e) {
      msg("Download error ", zip_name, ": ", e$message)
      FALSE
    })

    if (!ok || !file.exists(zip_path) || is.na(file.info(zip_path)$size) ||
        file.info(zip_path)$size == 0) {

      if (file.exists(zip_path)) unlink(zip_path)

      results[[length(results) + 1]] <- data.frame(
        ANO = year,
        ZIP = zip_name,
        URL = zip_url,
        PATH = zip_path,
        STATUS = "FAILED_NOT_FOUND_OR_EMPTY",
        stringsAsFactors = FALSE
      )
      next
    }

    results[[length(results) + 1]] <- data.frame(
      ANO = year,
      ZIP = zip_name,
      URL = zip_url,
      PATH = zip_path,
      STATUS = "DOWNLOADED",
      stringsAsFactors = FALSE
    )
  }

  if (length(results) == 0) return(data.frame())
  do.call(rbind, results)
}
