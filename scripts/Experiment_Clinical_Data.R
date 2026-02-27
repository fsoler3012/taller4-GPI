# Experiment_Clinical_Data.R
# Une procedimientos del experimento con contactos limpios (sin duplicar columnas)


if (!requireNamespace("here", quietly = TRUE)) install.packages("here", repos = "https://cloud.r-project.org")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl", repos = "https://cloud.r-project.org")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx", repos = "https://cloud.r-project.org")


library(here); library(readxl); library(openxlsx)
here::i_am("scripts/Experiment_Clinical_Data.R")

proc_path <- here::here("data", "raw",   "procedimientos_may_jun_2025.xlsx")
cont_path <- here::here("data", "processed", "Contact_Clean.xlsx")
out_dir   <- here::here("data", "processed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path  <- file.path(out_dir, "Experiment_Clean.xlsx")

p <- readxl::read_xlsx(proc_path)
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