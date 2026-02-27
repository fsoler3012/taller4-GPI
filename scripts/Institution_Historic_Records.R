# Institution_Historic_Records.R
# Une históricos (ene-abr) con contactos (sin duplicar columnas)



if (!requireNamespace("here", quietly = TRUE)) install.packages("here", repos = "https://cloud.r-project.org")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl", repos = "https://cloud.r-project.org")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx", repos = "https://cloud.r-project.org")

library(here); library(readxl); library(openxlsx)
here::i_am("scripts/Institution_Historic_Records.R")

hist_path <- here::here("data", "raw",   "historicos_ene_abr_2025.xlsx")
cont_path <- here::here("data", "processed", "Contact_Clean.xlsx")
out_dir   <- here::here("data", "processed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path  <- file.path(out_dir, "Historic_Clean.xlsx")

h <- readxl::read_xlsx(hist_path)
c <- readxl::read_xlsx(cont_path)

if (all(c("id_electrofisiologo","id_institucion") %in% names(h))) {
  h$id_concat <- paste(h$id_electrofisiologo, h$id_institucion, sep = "_")
}
if (!"id_concat" %in% names(c) && all(c("id_electrofisiologo","id_institucion") %in% names(c))) {
  c$id_concat <- paste(c$id_electrofisiologo, c$id_institucion, sep = "_")
}

h$tipo_cateter <- "sin mejoras"

# Evitar columnas duplicadas del contacto
cols_add <- setdiff(names(c), c("id_concat", names(h)))
c2 <- c[, c("id_concat", cols_add), drop = FALSE]

z <- merge(h, c2, by = "id_concat", all.x = TRUE)

openxlsx::write.xlsx(z, file = out_path, overwrite = TRUE)
cat("File histórico de procedimientos creado con éxito")


renv::snapshot()
