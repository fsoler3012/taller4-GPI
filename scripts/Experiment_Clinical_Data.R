# Experiment_Clinical_Data.R
# Une procedimientos del experimento con contactos limpios (sin duplicar columnas)


if (!requireNamespace("here", quietly = TRUE)) install.packages("here", repos = "https://cloud.r-project.org")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl", repos = "https://cloud.r-project.org")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx", repos = "https://cloud.r-project.org")


library(here); library(readxl); library(openxlsx)
here::i_am("scripts/Experiment_Clinical_Data.R")

#Importación vieja de datos desde el file simulado
#proc_path <- here::here("data", "raw",   "procedimientos_may_jun_2025.xlsx")

# zenodo_data_acquisition.R
#
# Purpose: This script automates the acquisition of data directly from Zenodo
# repositories using the Zenodo API. It downloads specified datasets and 
# prepares them for analysis.
#
# Dependencies:
# - rjson: For parsing JSON data from API response
# - rio: For importing various data formats
# - tidyr: For data tidying operations
# - dplyr: For data manipulation
# - here: For path management
# - janitor: For cleaning variable names

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rjson, rio, tidyr, dplyr, here, janitor)

#' Download and prepare data from Zenodo repository
#'
#' @param zenodo_id The ID of the Zenodo repository
#' @param api_endpoint The Zenodo API endpoint (default: sandbox environment)
#' @param output_dir Directory to save the downloaded data (default: temporary directory)
#' @param filename Name to save the downloaded file (default: "data.csv")
#' @return A data frame containing the imported and cleaned data
#' @examples
#' # Get data from a specific Zenodo repository
#' survey_data <- get_zenodo_data("169207")
get_zenodo_data <- function(zenodo_id,
                            api_endpoint = "https://zenodo.org/api/records/",
                            output_dir = tempdir(),
                            filename = "data.csv") {
  
  # Dependencias usadas por la función
  if (!requireNamespace("jsonlite", quietly = TRUE))
    install.packages("jsonlite", repos = "https://cloud.r-project.org")
  if (!requireNamespace("rio", quietly = TRUE))
    install.packages("rio", repos = "https://cloud.r-project.org")
  if (!requireNamespace("janitor", quietly = TRUE))
    install.packages("janitor", repos = "https://cloud.r-project.org")
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Normaliza el endpoint (debe terminar en /api/records/)
  api_endpoint <- sub("/+$", "/", api_endpoint)
  if (!grepl("/api/records/?$", api_endpoint)) {
    api_endpoint <- paste0(sub("/+$","", api_endpoint), "/api/records/")
  }
  
  # Deriva ID numérico (recid) si llega un DOI o una URL
  recid <- zenodo_id
  if (grepl("^10\\.", recid))    recid <- sub("^.*zenodo\\.", "", recid)               # 10.5281/zenodo.18915916 -> 18915916
  if (grepl("^https?://", recid)) recid <- sub(".*/records/([0-9]+).*", "\\1", recid)  # URL -> id
  
  # Construye URL de metadatos
  api_url <- paste0(api_endpoint, recid)
  
  # Asegura directorio de salida
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  metadata_file <- file.path(output_dir, "metadata.json")
  
  message("Downloading metadata from Zenodo: ", api_url)
  status <- tryCatch(
    utils::download.file(api_url, destfile = metadata_file, mode = "wb", quiet = TRUE, method = "libcurl"),
    error = function(e) 1
  )
  if (!identical(status, 0L)) stop("Failed to download metadata from Zenodo API: ", api_url)
  
  message("Parsing metadata...")
  metadata <- tryCatch(
    jsonlite::fromJSON(metadata_file, simplifyVector = FALSE),
    error = function(e) stop("Failed to parse metadata JSON: ", conditionMessage(e))
  )
  
  files <- metadata$files
  if (is.null(files) || length(files) == 0) stop("No files found in the Zenodo record.")
  
  # Selecciona por filename (clave 'key' en el JSON)
  keys <- vapply(files, function(f) f$key, "")
  if (!is.null(filename) && !is.na(filename)) {
    idx <- match(filename, keys)
    if (is.na(idx)) {
      stop("El archivo '", filename, "' no existe en el registro.\nDisponibles:\n- ", paste(keys, collapse = "\n- "))
    }
  } else {
    # Si no se indica filename, usa el primero
    idx <- 1L
    message("No 'filename' provided; using the first file in the record: ", keys[idx])
  }
  
  # URL de descarga de contenido (no 'self')
  dl_url <- files[[idx]]$links$download %||% paste0(api_endpoint, recid, "/files/", keys[idx], "/content")
  output_file <- file.path(output_dir, keys[idx])
  
  message("Downloading data file: ", dl_url)
  status <- tryCatch(
    utils::download.file(dl_url, destfile = output_file, mode = "wb", method = "libcurl"),
    error = function(e) 1
  )
  if (!identical(status, 0L)) stop("Failed to download data file from Zenodo: ", dl_url)
  
  # Importa (con fallback a readxl si es XLSX)
  message("Importing data: ", output_file)
  dat <- try(rio::import(output_file), silent = TRUE)
  if (inherits(dat, "try-error")) {
    ext <- tolower(tools::file_ext(output_file))
    if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE))
        install.packages("readxl", repos = "https://cloud.r-project.org")
      dat <- readxl::read_xlsx(output_file)
    } else {
      stop("Could not import '", output_file, "'. Original error: ", attr(dat, "condition")$message)
    }
  }
  
  if (is.data.frame(dat)) dat <- janitor::clean_names(dat)
  return(dat)
}

cont_path <- here::here("data", "processed", "Contact_Clean.xlsx")
out_dir   <- here::here("data", "processed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path  <- file.path(out_dir, "Experiment_Clean.xlsx")

if (interactive()) {
  # Define Zenodo repository information
  ZENODO_ID <- "18915916"  # ID of the specific Zenodo repository
  
  # Get the survey data
  p <- get_zenodo_data(
    zenodo_id = ZENODO_ID,
    filename = "procedimientos_may_jun_2025.xlsx"
  )
}
c <- readxl::read_xlsx(cont_path)

if (all(c("id_electrofisiologo","id_institucion") %in% names(p))) {
  p$id_concat <- paste(p$id_electrofisiologo, p$id_institucion, sep = "_")
}
if (!"id_concat" %in% names(c) && all(c("id_electrofisiologo","id_institucion") %in% names(c))) {
  c$id_concat <- paste(c$id_electrofisiologo, c$id_institucion, sep = "_")
}

# Evitar columnas duplicadas del contacto: sólo unir columnas que NO existen en p
cols_add <- setdiff(names(c), c("id_concat", names(p)))
c2 <- c[, c("id_concat", cols_add), drop = FALSE]

z <- merge(p, c2, by = "id_concat", all.x = TRUE)

openxlsx::write.xlsx(z, file = out_path, overwrite = TRUE)
cat("File de data experimento creado con éxito")


renv::snapshot()