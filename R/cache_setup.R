#' Download and Install Package Dictionary Data
#'
#' The \code{setup_data()} function downloads a ZIP archive containing
#' external dictionary data files from Google Drive, extracts them into
#' the package cache directory, and prepares them for use by other
#' functions in the package.
#'
#' It performs:
#' \itemize{
#'   \item Download of a ZIP bundle from Google Drive
#'   \item Validation of the downloaded file
#'   \item Extraction of parquet and related files into a standardized cache path
#'   \item Replacement of previously installed data when requested
#'   \item Creation of a marker file indicating successful installation
#' }
#'
#' @param file_id Character string containing the Google Drive file ID
#'   of the ZIP archive to download.
#' @param overwrite Logical; if TRUE, re-downloads and reinstalls the data
#'   even if it is already present.
#'
#' @return Invisibly returns the path to the installed data directory.
#'
#' @details
#' This function stores the downloaded data in the user cache directory
#' returned by \code{tools::R_user_dir("megadatasus", "cache")}.
#'
#' The extracted files are placed in a standardized \code{data} folder
#' within that cache directory, and a marker file named
#' \code{.data_ready} is created to indicate that installation completed
#' successfully.
#'
#' If the downloaded file is not a valid ZIP archive, or if extraction
#' fails, the function stops with an informative error message.
#'
#' If the Google Drive file is public, authentication is not required.
#' For private files, users may need to authenticate with
#' \code{googledrive::drive_auth()} before running this function.
#'
#' @examples
#' \dontrun{
#' library(megadatasus)
#'
#' # Download and install package dictionary data
#' setup_data()
#'
#' # Force reinstallation
#' setup_data(overwrite = TRUE)
#' }
#'
#' @seealso \code{\link{clean_table}}
#'
#' @export
setup_data <- function(
    file_id = "1g6-P9krJUfFoOgbGLTQut_NQd-VHCUIG",
    overwrite = FALSE
) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Install 'googledrive': install.packages('googledrive')",
      call. = FALSE
    )
  }

  cache_root <- normalizePath(
    tools::R_user_dir("megadatasus", "cache"),
    winslash = "/",
    mustWork = FALSE
  )
  dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)

  final_dir <- file.path(cache_root, "data")
  marker <- file.path(final_dir, ".data_ready")

  if (file.exists(marker) && !overwrite) {
    message("Overwriting parquet data already installed at: ", final_dir)
    return(invisible(final_dir))
  }

  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  zip_path <- file.path(cache_root, paste0("parquet_bundle_", stamp, ".zip"))
  stage_dir <- file.path(cache_root, paste0("stage_", stamp))

  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)

  message("Downloading ZIP from Google Drive...")
  googledrive::drive_deauth()  # public link; if private, user can drive_auth()

  googledrive::drive_download(
    googledrive::as_id(file_id),
    path = zip_path,
    overwrite = TRUE
  )

  if (!file.exists(zip_path) || is.na(file.size(zip_path)) || file.size(zip_path) < 1000) {
    unlink(stage_dir, recursive = TRUE, force = TRUE)
    stop(
      "Download looks incomplete. Check Drive sharing permissions.",
      call. = FALSE
    )
  }

  magic <- rawToChar(readBin(zip_path, "raw", n = 2))
  if (magic != "PK") {
    sig8 <- paste(as.integer(readBin(zip_path, "raw", n = 8)), collapse = " ")
    unlink(stage_dir, recursive = TRUE, force = TRUE)
    stop(
      "Downloaded file is not a ZIP (missing PK signature). Signature bytes: ",
      sig8,
      "\nIf the file is private, run googledrive::drive_auth() first.",
      call. = FALSE
    )
  }

  message("Extracting files...")
  extracted <- tryCatch(
    utils::unzip(zip_path, exdir = stage_dir),
    error = function(e) {
      unlink(stage_dir, recursive = TRUE, force = TRUE)
      stop(
        "Unzip failed. Common cause on Windows: long paths inside ZIP.\n",
        "Fix: re-zip with shorter folder/file names.\n\n",
        "Original error: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (length(extracted) == 0) {
    unlink(stage_dir, recursive = TRUE, force = TRUE)
    stop(
      "Unzip finished but no files were extracted. ZIP may be empty.",
      call. = FALSE
    )
  }

  try(unlink(zip_path, force = TRUE), silent = TRUE)

  if (dir.exists(final_dir)) {
    unlink(final_dir, recursive = TRUE, force = TRUE)
  }

  top <- list.dirs(stage_dir, recursive = FALSE, full.names = TRUE)

  if (length(top) == 1 && dir.exists(top[1])) {
    ok <- file.rename(top[1], final_dir)
  } else {
    ok <- file.rename(stage_dir, final_dir)
  }

  if (!isTRUE(ok)) {
    dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(
      list.files(stage_dir, full.names = TRUE),
      final_dir,
      recursive = TRUE
    )
    unlink(stage_dir, recursive = TRUE, force = TRUE)
  }

  file.create(marker)

  message("Data installed successfully at standardized path: ", final_dir)
  invisible(final_dir)
}




