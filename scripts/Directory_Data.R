# Directory_Data.R
# Limpia y enriquece el archivo de contactos a partir de "electrofisiologos.xlsx".

# Paquetes
if (!requireNamespace("here", quietly = TRUE)) install.packages("here", repos = "https://cloud.r-project.org")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl", repos = "https://cloud.r-project.org")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx", repos = "https://cloud.r-project.org")


library(here); library(readxl); library(openxlsx)
# Anclar raíz (ajusta el nombre si cambias este archivo de carpeta/nombre)
here::i_am("scripts/Directory_Data.R")

# Rutas
in_path  <- here::here("data", "raw",  "electrofisiologos.xlsx")
out_dir  <- here::here("data", "processed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, "Contact_Clean.xlsx")
# Leer
x <- readxl::read_xlsx(in_path)

# Limpiar institucion: quedarnos sólo con lo anterior a "- "
if ("nombre_institucion" %in% names(x)) {
  x$nombre_institucion <- sub(" - .*", "", x$nombre_institucion)
}

# Nombres en Title Case
if ("nombre_electrofisiologo" %in% names(x)) {
  x$nombre_electrofisiologo <- tools::toTitleCase(tolower(x$nombre_electrofisiologo))
}

# Columna de concatenación (id_electrofisiologo + id_institucion)
if (all(c("id_electrofisiologo","id_institucion") %in% names(x))) {
  x$id_concat <- paste(x$id_electrofisiologo, x$id_institucion, sep = "_")
}

# Exportar
openxlsx::write.xlsx(x, file = out_path, overwrite = TRUE)
cat("File data directory creado con éxito")

renv::snapshot()
