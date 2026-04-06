#' List Available DATASUS Files from FTP
#'
#' The \code{Datasus_info()} function connects to the public DATASUS FTP
#' servers and retrieves structured information about available data files
#' for a given source.
#'
#' It performs:
#' \itemize{
#'   \item File listing from DATASUS FTP directories
#'   \item Parsing of filenames (prefix, state, year, month)
#'   \item Standardization into a structured table
#'   \item Source-specific filtering (SIM, SINASC, CNES, etc.)
#'   \item Local caching for improved performance
#' }
#'
#' @param Fonte Character string indicating the DATASUS source
#'   (e.g., "SIM-DO", "SINASC-DN", "SIH", "CNES-DC", "SISCOLO", "SISMAMA", "PO").
#' @param refresh Logical; if TRUE, forces cache refresh.
#' @param cache_dir Character string specifying the directory used to store cached results.
#' @param verbose Logical; if TRUE, prints progress messages.
#'
#' @return A \code{data.table} containing:
#' \itemize{
#'   \item filename: file name
#'   \item ext: file extension (.dbc/.DBC)
#'   \item prefix: dataset prefix (e.g., DO, DN, etc.)
#'   \item uf: state abbreviation or "BR"
#'   \item year: year extracted from filename
#'   \item month: month (if available)
#'   \item fonte: DATASUS source
#'   \item base: FTP directory URL
#' }
#'
#' @details
#' This function uses the official DATASUS FTP server:
#' \url{ftp://ftp.datasus.gov.br}
#'
#' Results are cached locally as RDS files to avoid repeated FTP calls.
#' Use \code{refresh = TRUE} to force updating the cache.
#'
#' The function includes parsing logic adapted to different DATASUS naming conventions,
#' including:
#' \itemize{
#'   \item SIM (CID9 and CID10)
#'   \item SINASC (DN and DNEX)
#'   \item CNES datasets
#'   \item SISCOLO and SISMAMA datasets
#' }
#'
#' @examples
#' \dontrun{
#' library(megadatasus)
#'
#' # List SIM files
#' sim_info <- Datasus_info("SIM")
#'
#' # List SINASC files
#' sinasc_info <- Datasus_info("SINASC-DN")
#'
#' # Refresh cache
#' sim_info <- Datasus_info("SIM", refresh = TRUE)
#' }
#'
#' @seealso \code{\link{clean_table}}, \code{\link{acesso_datasus}}
#'
#' @export
Datasus_info <- function(Fonte,
                         refresh = FALSE,
                         cache_dir = "C:/DATASUS/cache_info",
                         verbose = TRUE) {

  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop("Instale RCurl: install.packages('RCurl')")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Instale data.table: install.packages('data.table')")
  }
  if (!requireNamespace("read.dbc", quietly = TRUE)) {
    stop("Please install read.dbc from GitHub: remotes::install_github('danicat/read.dbc')")
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  ftp_map <- list(
    "SIH" = c(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"
    ),

    "SISCOLO" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISCOLO4/Dados/"),
    "SISMAMA" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/SISCAN/SISMAMA/Dados/"),
    "CIH" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CIH/200801_201012/Dados/"),
    "PO"  = c("ftp://ftp.datasus.gov.br/dissemin/publicos/PAINEL_ONCOLOGIA/DADOS/"),

    "SIM-DO" = c(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DORES/"
    ),
    "SIM-DOFET" = c(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DOFET/"
    ),
    "SIM-DOEXT" = c(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DORES/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DOFET/"
    ),
    "SIM-DOINF" = c(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DORES/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DOFET/"
    ),
    "SIM-DOMAT" = c(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DORES/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DOFET/"
    ),

    "SINASC-DN_9495" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1994_1995/Dados/DNRES/"),
    "SINASC-DN"      = c("ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"),
    "SINASC-DNEX"    = c("ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"),

    "CNES-DC" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/DC/"),
    "CNES-EE" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EE/"),
    "CNES-EF" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EF/"),
    "CNES-EP" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EP/"),
    "CNES-EQ" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EQ/"),
    "CNES-GM" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/GM/"),
    "CNES-HB" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/HB/"),
    "CNES-IN" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/IN/"),
    "CNES-LT" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/LT/"),
    "CNES-PF" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/"),
    "CNES-RC" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/RC/"),
    "CNES-SR" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/SR/"),
    "CNES-ST" = c("ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/")
  )

  if (toupper(Fonte) == "SIM") {
    ftp_map[["SIM"]] <- c(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DORES/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/",
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/DOFET/"
    )
    Fonte <- "SIM"
  }

  if (!(Fonte %in% names(ftp_map))) {
    stop("Fonte nao mapeada. Use uma destas: ", paste(names(ftp_map), collapse = ", "))
  }

  cache_file <- file.path(
    cache_dir,
    paste0("INFO_", gsub("[^A-Za-z0-9\\-]", "_", Fonte), ".rds")
  )

  ftp_list <- function(dir_url, tries = 4, timeout_sec = 20, sleep_sec = 1, verbose = FALSE) {
    for (i in seq_len(tries)) {
      if (verbose) message("  ftp_list try ", i, "/", tries, " -> ", dir_url)

      txt <- tryCatch(
        RCurl::getURL(
          dir_url,
          ftp.use.epsv = FALSE,
          dirlistonly = TRUE,
          .opts = list(timeout = timeout_sec, connecttimeout = timeout_sec)
        ),
        error = function(e) ""
      )

      if (nzchar(txt)) {
        lines <- unlist(strsplit(txt, "\r\n|\n"))
        lines <- trimws(lines)
        lines <- lines[nzchar(lines)]
        files <- vapply(
          strsplit(lines, "\\s+"),
          function(parts) utils::tail(parts, 1),
          character(1)
        )
        files <- files[nzchar(files)]
        return(files)
      }

      Sys.sleep(sleep_sec)
    }

    tmp <- tempfile(fileext = ".txt")
    ok <- FALSE

    try({
      cmd <- sprintf(
        'curl -s --connect-timeout %d --max-time %d "%s" -o "%s"',
        timeout_sec, timeout_sec, dir_url, tmp
      )
      suppressWarnings(system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE))
      ok <- file.exists(tmp) && isTRUE(file.info(tmp)$size > 0)
    }, silent = TRUE)

    if (!ok) return(character(0))

    txt2 <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    if (!nzchar(txt2)) return(character(0))

    lines <- unlist(strsplit(txt2, "\r\n|\n"))
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    files <- vapply(
      strsplit(lines, "\\s+"),
      function(parts) utils::tail(parts, 1),
      character(1)
    )
    files[nzchar(files)]
  }

  parse_filename <- function(fn) {
    if (!grepl("\\.dbc$|\\.DBC$", fn)) return(NULL)

    ext <- if (grepl("\\.DBC$", fn)) ".DBC" else ".dbc"

    if (startsWith(Fonte, "SINASC")) {
      base0 <- toupper(sub("\\.dbc$|\\.DBC$", "", fn))
      base0 <- gsub("[-_\\s]+", "", base0)

      m2 <- regexec("^(DNEX)(\\d{4})$", base0)
      r2 <- regmatches(base0, m2)[[1]]
      if (length(r2) > 0) {
        year <- suppressWarnings(as.integer(r2[3]))
        if (is.na(year)) return(NULL)

        return(data.table::data.table(
          filename = fn,
          ext = ext,
          prefix = "DNEX",
          uf = "BR",
          year = year,
          month = NA_integer_
        ))
      }

      m1 <- regexec("^DN([A-Z]{2})(\\d{4})$", base0)
      r1 <- regmatches(base0, m1)[[1]]
      if (length(r1) > 0) {
        uf <- r1[2]
        year <- suppressWarnings(as.integer(r1[3]))
        if (is.na(year)) return(NULL)

        return(data.table::data.table(
          filename = fn,
          ext = ext,
          prefix = "DN",
          uf = uf,
          year = year,
          month = NA_integer_
        ))
      }

      m3 <- regexec("^DN([A-Z]{3})(\\d{4})$", base0)
      r3 <- regmatches(base0, m3)[[1]]
      if (length(r3) > 0) {
        year <- suppressWarnings(as.integer(r3[3]))
        if (is.na(year)) return(NULL)

        return(data.table::data.table(
          filename = fn,
          ext = ext,
          prefix = paste0("DN", r3[2]),
          uf = "BR",
          year = year,
          month = NA_integer_
        ))
      }

      return(NULL)
    }

    base <- toupper(sub("\\.dbc$|\\.DBC$", "", fn))
    base <- gsub("[-_\\s]+", "", base)

    m <- regexec("^([A-Z]+)(BR|[A-Z]{2})(\\d{2}|\\d{4})(\\d{2})?$", base)
    r <- regmatches(base, m)[[1]]
    if (length(r) == 0) return(NULL)

    prefix <- r[2]
    uf <- r[3]
    token <- r[4]
    mraw <- if (length(r) >= 5) r[5] else ""

    year <- NA_integer_
    month <- NA_integer_

    pivot <- as.integer(format(Sys.Date(), "%y"))
    yy_to_year <- function(yy) if (yy <= pivot) 2000L + yy else 1900L + yy

    parse_mm <- function(x) {
      if (!nzchar(x)) return(NA_integer_)
      mm <- suppressWarnings(as.integer(x))
      if (is.na(mm) || mm < 1 || mm > 12) return(NA_integer_)
      mm
    }

    parse_yymm <- function(yymm) {
      yy <- suppressWarnings(as.integer(substr(yymm, 1, 2)))
      mm <- suppressWarnings(as.integer(substr(yymm, 3, 4)))
      if (is.na(yy) || is.na(mm)) return(list(year = NA_integer_, month = NA_integer_))
      if (mm < 1 || mm > 12) return(list(year = NA_integer_, month = NA_integer_))
      list(year = yy_to_year(yy), month = mm)
    }

    if (Fonte %in% c("SISCOLO", "SISMAMA")) {
      if (nchar(token) == 4) {
        yymm <- parse_yymm(token)
        year <- yymm$year
        month <- yymm$month
      } else if (nchar(token) == 2) {
        yy <- suppressWarnings(as.integer(token))
        if (is.na(yy)) return(NULL)
        year <- yy_to_year(yy)
        month <- parse_mm(mraw)
      } else {
        return(NULL)
      }

    } else if (startsWith(Fonte, "SIM") || Fonte == "SIM") {
      if (nchar(token) == 4) {
        val <- suppressWarnings(as.integer(token))
        if (is.na(val)) return(NULL)
        year <- val
        month <- parse_mm(mraw)
      } else {
        yy <- suppressWarnings(as.integer(token))
        if (is.na(yy)) return(NULL)
        year <- yy_to_year(yy)
        month <- parse_mm(mraw)
      }

    } else if (startsWith(Fonte, "CNES-")) {
      if (nchar(token) == 4) {
        yymm <- parse_yymm(token)
        year <- yymm$year
        month <- yymm$month
      } else if (nchar(token) == 2) {
        yy <- suppressWarnings(as.integer(token))
        if (is.na(yy)) return(NULL)
        year <- yy_to_year(yy)
        month <- parse_mm(mraw)
      } else {
        return(NULL)
      }

    } else {
      if (nchar(token) == 4) {
        yymm <- parse_yymm(token)
        if (!is.na(yymm$year)) {
          year <- yymm$year
          month <- yymm$month
        } else {
          val <- suppressWarnings(as.integer(token))
          if (!is.na(val)) year <- val
          month <- parse_mm(mraw)
        }
      } else {
        val <- suppressWarnings(as.integer(token))
        if (!is.na(val)) {
          year <- if (nchar(token) == 2) yy_to_year(val) else val
        }
        month <- parse_mm(mraw)
      }
    }

    data.table::data.table(
      filename = fn,
      ext = ext,
      prefix = prefix,
      uf = uf,
      year = year,
      month = month
    )
  }

  sim_prefix_allowlist <- function(Fonte) {
    if (Fonte == "SIM") {
      return(c("DORES", "DOFET", "DOEXT", "DOMAT", "DOINF", "DOREXT", "DO", "DOR"))
    }
    if (!startsWith(Fonte, "SIM-")) return(NULL)
    if (Fonte == "SIM-DO") return(c("DORES", "DO", "DOR"))
    sub("^SIM-", "", Fonte)
  }

  sinasc_filter <- function(Fonte, dt) {
    if (!startsWith(Fonte, "SINASC")) return(dt)
    if (!("prefix" %in% names(dt))) return(dt)

    if (Fonte == "SINASC-DN") {
      return(dt[dt$prefix == "DN", , drop = FALSE])
    }
    if (Fonte == "SINASC-DNEX") {
      return(dt[dt$prefix == "DNEX", , drop = FALSE])
    }
    if (Fonte == "SINASC-DN_9495") {
      return(dt[startsWith(dt$prefix, "DN"), , drop = FALSE])
    }
    dt
  }

  if (!refresh && file.exists(cache_file)) {
    if (verbose) message("Using cached INFO for ", Fonte, " (refresh=TRUE to update).")
    return(readRDS(cache_file))
  }

  bases <- ftp_map[[Fonte]]
  out <- list()

  allow_prefix_sim <- sim_prefix_allowlist(Fonte)
  allow_prefix_upper <- if (!is.null(allow_prefix_sim)) toupper(allow_prefix_sim) else NULL

  for (b in bases) {
    if (verbose) message("Listing: ", b)

    files <- ftp_list(b, tries = 4, timeout_sec = 20, sleep_sec = 1, verbose = verbose)
    if (length(files) == 0) next

    parsed <- lapply(files, parse_filename)
    parsed <- parsed[!vapply(parsed, is.null, logical(1))]
    if (length(parsed) == 0) next

    dt <- data.table::rbindlist(parsed, fill = TRUE)

    if (!is.null(allow_prefix_upper) && (startsWith(Fonte, "SIM") || Fonte == "SIM")) {
      keep <- !is.na(dt$prefix) & toupper(dt$prefix) %in% allow_prefix_upper
      dt <- dt[keep, , drop = FALSE]
    }

    if (startsWith(Fonte, "SINASC")) {
      dt <- sinasc_filter(Fonte, dt)
    }

    if (nrow(dt) == 0) next

    dt$fonte <- Fonte
    dt$base <- b

    out[[length(out) + 1]] <- data.table::as.data.table(dt)
  }

  empty_template <- data.table::data.table(
    filename = character(),
    ext = character(),
    prefix = character(),
    uf = character(),
    year = integer(),
    month = integer(),
    fonte = character(),
    base = character()
  )

  res <- if (length(out) > 0) {
    data.table::rbindlist(out, fill = TRUE)
  } else {
    empty_template
  }

  if (nrow(res) > 0) {
    res <- unique(res)
  }

  saveRDS(res, cache_file)
  res[]
}
