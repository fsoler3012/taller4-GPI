# Clean_Sample_Clinical_Results.R
# Appendix of experiments/historic data, simple analysis, and basic statistical models.
# ------------------------------------------------------------------------------
# Packages (install if missing)
if (!requireNamespace("here", quietly = TRUE))      install.packages("here", repos = "https://cloud.r-project.org")
if (!requireNamespace("readxl", quietly = TRUE))    install.packages("readxl", repos = "https://cloud.r-project.org")
if (!requireNamespace("openxlsx", quietly = TRUE))  install.packages("openxlsx", repos = "https://cloud.r-project.org")
if (!requireNamespace("broom", quietly = TRUE))     install.packages("broom", repos = "https://cloud.r-project.org")

library(here)
library(readxl)
library(openxlsx)
library(broom)

# ------------------------------------------------------------------------------
# Project root
here::i_am("scripts/Clean_Sample_Clinical_Results.R")

# ------------------------------------------------------------------------------
# Paths
exp_path  <- here::here("data", "processed", "Experiment_Clean.xlsx")
hist_path <- here::here("data", "processed", "Historic_Clean.xlsx")

# Output directories
res_dir_tables  <- here::here("results","tables")
res_dir_figures <- here::here("results","figures")
res_dir_models  <- here::here("results","models")
for (dd in c(res_dir_tables, res_dir_figures, res_dir_models)) {
  if (!dir.exists(dd)) dir.create(dd, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# Read data (fail friendly)
if (!file.exists(exp_path)) stop("No se encontró: ", exp_path)
if (!file.exists(hist_path)) stop("No se encontró: ", hist_path)

E <- readxl::read_xlsx(exp_path)
H <- readxl::read_xlsx(hist_path)

# ------------------------------------------------------------------------------
# Harmonize columns & append
cols <- intersect(names(E), names(H))
if (length(cols) == 0) stop("Experiment_Clean.xlsx y Historic_Clean.xlsx no comparten columnas.")
E2 <- E[, cols]
H2 <- H[, cols]
D  <- rbind(E2, H2)

# ------------------------------------------------------------------------------
# Date coercion (robust: handles character, Date, POSIXct, and Excel serials)
coerce_to_Date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # Excel serial date (Windows origin)
    return(as.Date(x, origin = "1899-12-30"))
  }
  # Character: try ISO and common formats
  suppressWarnings({
    y <- as.Date(x)
    if (all(is.na(y))) {
      # Try dmy/ymd
      if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate", repos = "https://cloud.r-project.org")
      y <- tryCatch(lubridate::as_date(x), error = function(e) rep(NA, length(x)))
    }
    y
  })
}
if (!"fecha_procedimiento" %in% names(D)) stop("Falta columna 'fecha_procedimiento' en los datos combinados.")
D$fecha_procedimiento <- coerce_to_Date(D$fecha_procedimiento)

# ------------------------------------------------------------------------------
# May–June table by catheter type
mm <- as.integer(format(D$fecha_procedimiento, "%m"))
D_mj <- D[!is.na(mm) & mm %in% c(5,6), , drop = FALSE]

# Promedio de tiempo y conteo de hemorragia por tipo de catéter
# Hemorragia: robust mapping
hemo_raw_mj <- tolower(trimws(as.character(D_mj$presencia_hemorragia)))
hemo_yes_mj <- hemo_raw_mj %in% c("si","sí","yes","true","1","y","t")
mean_time   <- tapply(D_mj$tiempo_duracion_min, D_mj$tipo_cateter, mean, na.rm = TRUE)
count_hemo  <- tapply(hemo_yes_mj, D_mj$tipo_cateter, sum, na.rm = TRUE)

TABLA_MJ <- data.frame(
  tipo_cateter      = names(mean_time),
  promedio_tiempo   = as.numeric(mean_time),
  conteo_hemorragia = as.numeric(count_hemo),
  row.names = NULL
)
print(TABLA_MJ)

# ------------------------------------------------------------------------------
# Period variable (Ene–Abr vs May–Jun)
periodo <- ifelse(as.integer(format(D$fecha_procedimiento, "%m")) %in% 1:4, "Ene-Abr",
                  ifelse(as.integer(format(D$fecha_procedimiento, "%m")) %in% 5:6, "May-Jun", NA))
D$periodo <- factor(periodo, levels = c("Ene-Abr","May-Jun"))

# ------------------------------------------------------------------------------
# Institution-period summaries
mt <- tapply(D$tiempo_duracion_min, list(D$id_institucion, D$periodo), mean, na.rm = TRUE)
# Hemorrhage high/count flag proxy: non-NA volume (keep your original intent)
ha_flag <- !is.na(D$volumen_hemorragia)
ha <- tapply(ha_flag, list(D$id_institucion, D$periodo), sum, na.rm = TRUE)

# Keep consistent 2 columns order if present
keep_cols <- c("Ene-Abr","May-Jun")
mt <- mt[, colnames(mt) %in% keep_cols, drop = FALSE]
ha <- ha[, colnames(ha) %in% keep_cols, drop = FALSE]

# If a column is missing (no data), add NA to keep shape
ensure_cols <- function(mat, cols) {
  out <- mat
  for (cc in cols) {
    if (!cc %in% colnames(out)) out <- cbind(out, setNames(data.frame(NA_real_), cc))
  }
  out[, cols, drop = FALSE]
}
mt2 <- ensure_cols(mt, keep_cols)
ha2 <- ensure_cols(ha, keep_cols)

# ------------------------------------------------------------------------------
# Simple barplots (saved to files)
if (nrow(mt2) > 0) {
  png(filename = file.path(res_dir_figures, "barplot_promedio_tiempo_por_institucion.png"),
      width = 1400, height = 700, res = 150)
  par(mfrow = c(1,1))
  barplot(t(mt2), beside = TRUE, legend.text = TRUE,
          args.legend = list(x = "topright", bty = "n"),
          main = "Promedio de tiempo por institución",
          xlab = "Institución", ylab = "Tiempo (min)")
  abline(h = pretty(range(mt2, na.rm = TRUE)), col = "grey90", lty = "dotted")
  dev.off()
}

if (nrow(ha2) > 0) {
  png(filename = file.path(res_dir_figures, "barplot_hemorragias_por_institucion.png"),
      width = 1400, height = 700, res = 150)
  par(mfrow = c(1,1))
  barplot(t(ha2), beside = TRUE, legend.text = TRUE,
          args.legend = list(x = "topright", bty = "n"),
          main = "Hemorragias (proxy) por institución",
          xlab = "Institución", ylab = "Conteo")
  abline(h = pretty(range(ha2, na.rm = TRUE)), col = "grey90", lty = "dotted")
  dev.off()
}

# ------------------------------------------------------------------------------
# Slope charts (saved to files)
slope_simple <- function(mat, yl, main_txt, outfile) {
  if (nrow(mat) == 0 || all(is.na(mat))) {
    warning("Slope chart omitido: matriz vacía o NA.")
    return(invisible(NULL))
  }
  rng <- range(mat, na.rm = TRUE)
  png(filename = outfile, width = 1400, height = 700, res = 150)
  par(mfrow = c(1,1))
  plot(c(1, 2), rng, type = "n", xaxt = "n", xlab = "", ylab = yl, main = main_txt)
  axis(1, at = 1:2, labels = keep_cols)
  for (i in seq_len(nrow(mat))) {
    segments(1, mat[i, 1], 2, mat[i, 2], col = "gray40", lwd = 2)
    points(1, mat[i, 1], pch = 16, col = "steelblue3")
    points(2, mat[i, 2], pch = 16, col = "tomato3")
  }
  text(2.05, mat[, 2], labels = rownames(mat), cex = 0.75, adj = 0)
  text(0.95, mat[, 1], labels = rownames(mat), cex = 0.6, adj = 1, col = "gray40")
  abline(h = pretty(rng), col = "grey90", lty = "dotted")
  dev.off()
}

slope_simple(mt2, yl = "Tiempo (min)",
             main_txt = "Cambio de tiempo por institución",
             outfile = file.path(res_dir_figures, "slope_tiempo_por_institucion.png"))
slope_simple(ha2, yl = "Conteo (hemorragias)",
             main_txt = "Cambio de hemorragias por institución",
             outfile = file.path(res_dir_figures, "slope_hemorragias_por_institucion.png"))

# ------------------------------------------------------------------------------
