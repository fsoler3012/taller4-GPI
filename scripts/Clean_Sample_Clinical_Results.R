# Clean_Sample_Clinical_Results.R
# Apéndice de experimentos e históricos y análisis simple.



# Paquetes
if (!requireNamespace("here", quietly = TRUE)) install.packages("here", repos = "https://cloud.r-project.org")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl", repos = "https://cloud.r-project.org")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx", repos = "https://cloud.r-project.org")


library(here); library(readxl); library(openxlsx)
# Raíz
here::i_am("scripts/Clean_Sample_Clinical_Results.R")

# Rutas
exp_path  <- here::here("data", "processed", "Experiment_Clean.xlsx")
hist_path <- here::here("data", "processed", "Historic_Clean.xlsx")

# Leer
E <- readxl::read_xlsx(exp_path)
H <- readxl::read_xlsx(hist_path)

# Append
cols <- intersect(names(E), names(H))
E2 <- E[, cols]
H2 <- H[, cols]
D  <- rbind(E2, H2)

# Fechas a Date
if (!inherits(D$fecha_procedimiento, "Date")) {
  D$fecha_procedimiento <- as.Date(D$fecha_procedimiento)
}

# Tabla: sólo mayo y junio
mm <- as.integer(format(D$fecha_procedimiento, "%m"))
D_mj <- D[mm %in% c(5,6), ]

# Promedio tiempo y conteo de hemorragia por tipo de catéter
mean_time <- tapply(D_mj$tiempo_duracion_min, D_mj$tipo_cateter, mean, na.rm = TRUE)
count_hemo <- tapply(D_mj$presencia_hemorragia == "si", D_mj$tipo_cateter, sum, na.rm = TRUE)

TABLA_MJ <- data.frame(
  tipo_cateter = names(mean_time),
  promedio_tiempo = as.numeric(mean_time),
  conteo_hemorragia = as.numeric(count_hemo),
  row.names = NULL
)

print(TABLA_MJ)

# Periodo para gráficos: Ene-Abr vs May-Jun
periodo <- ifelse(as.integer(format(D$fecha_procedimiento, "%m")) %in% 1:4, "Ene-Abr",
                  ifelse(as.integer(format(D$fecha_procedimiento, "%m")) %in% 5:6, "May-Jun", NA))
D$periodo <- factor(periodo, levels = c("Ene-Abr","May-Jun"))

# Promedio tiempo por institucion y periodo
mt <- tapply(D$tiempo_duracion_min, list(D$id_institucion, D$periodo), mean, na.rm = TRUE)
mt <- mt[ , colnames(mt) %in% c("Ene-Abr","May-Jun"), drop = FALSE]

# Conteo de hemorragias ALTO por institucion y periodo
ha_flag <- !is.na(D$volumen_hemorragia) 
ha <- tapply(ha_flag, list(D$id_institucion, D$periodo), sum, na.rm = TRUE)
ha <- ha[ , colnames(ha) %in% c("Ene-Abr","May-Jun"), drop = FALSE]

# Graficos simples (barplot) para comparar periodos por institucion
par(mfrow = c(1,2))
barplot(t(mt), beside = TRUE, legend.text = TRUE, args.legend = list(x = "topright"),
        main = "Promedio de tiempo por institucion", xlab = "Institucion", ylab = "Tiempo (min)")
barplot(t(ha), beside = TRUE, legend.text = TRUE, args.legend = list(x = "topright"),
        main = "Hemorragias ALTO por institucion", xlab = "Institucion", ylab = "Conteo")
par(mfrow = c(1,1))

# Asegurar orden de columnas
keep_cols <- c("Ene-Abr","May-Jun")
mt2 <- mt[, colnames(mt) %in% keep_cols, drop = FALSE]
ha2 <- ha[, colnames(ha) %in% keep_cols, drop = FALSE]

# Si falta alguna columna (por datos), rellenar con NA para mantener estructura
for (cc in keep_cols) {
  if (!cc %in% colnames(mt2)) mt2 <- cbind(mt2, setNames(data.frame(NA_real_), cc))
  if (!cc %in% colnames(ha2)) ha2 <- cbind(ha2, setNames(data.frame(NA_real_), cc))
}
mt2 <- mt2[, keep_cols, drop = FALSE]
ha2 <- ha2[, keep_cols, drop = FALSE]

# Función auxiliar para dibujar un slope chart simple
slope_simple <- function(mat, yl, main_txt) {
  rng <- range(mat, na.rm = TRUE)
  plot(c(1, 2), rng, type = "n", xaxt = "n", xlab = "", ylab = yl, main = main_txt)
  axis(1, at = 1:2, labels = keep_cols)
  # líneas + puntos
  for (i in seq_len(nrow(mat))) {
    segments(1, mat[i, 1], 2, mat[i, 2], col = "gray40", lwd = 2)
    points(1, mat[i, 1], pch = 16, col = "steelblue3")
    points(2, mat[i, 2], pch = 16, col = "tomato3")
  }
  # Etiquetas de instituciones a la derecha (y a la izquierda si quieres)
  text(2.05, mat[, 2], labels = rownames(mat), cex = 0.75, adj = 0)
  text(0.95, mat[, 1], labels = rownames(mat), cex = 0.6, adj = 1, col = "gray40")
  abline(h = pretty(rng), col = "grey90", lty = "dotted")
}

par(mfrow = c(1,2))
slope_simple(mt2, yl = "Tiempo (min)", main_txt = "Cambio de Tiempo por institución")
slope_simple(ha2, yl = "Conteo (hemorragias)", main_txt = "Cambio de Hemorragias por institución")
par(mfrow = c(1,1))

# Carpeta de salida
res_dirt <- here::here("results","tables")
res_dirg <- here::here("results","figures")

# 1) Exportar tabla (usa TABLA_MJ que ya calculaste)
tabla_path <- file.path(res_dirt, "tabla_resultados.xlsx")
openxlsx::write.xlsx(TABLA_MJ, file = tabla_path, overwrite = TRUE)

# 2) Exportar gráfico (re-dibuja tu gráfico dentro del dispositivo)
grafico_path <- file.path(res_dirg, "grafico_resultados.png")
png(grafico_path, width = 1400, height = 700, res = 150)

cat("Análisis creados y exportados con éxito")

renv::snapshot()