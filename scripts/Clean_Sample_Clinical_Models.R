<<<<<<< HEAD
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
=======

# Ensure packages for tidy export
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom", repos = "https://cloud.r-project.org")
}

# Ensure factor structure and binary outcome
D$tipo_cateter   <- as.factor(D$tipo_cateter)
D$id_institucion <- as.factor(D$id_institucion)
D$periodo        <- droplevels(D$periodo)

# Binary hemorrhage outcome from 'presencia_hemorragia'
# Maps yes/true/SI to 1, everything else to 0 (NA becomes 0 by default here; adjust if you prefer NA)
hemo_raw <- tolower(trimws(as.character(D$presencia_hemorragia)))
D$hemo_bin <- ifelse(hemo_raw %in% c("si","sí","yes","true","1","y","t"), 1L, 0L)

# Create results/models directory
res_model_dir <- here::here("results", "models")
if (!dir.exists(res_model_dir)) dir.create(res_model_dir, recursive = TRUE)

#-------------- Model 1: Linear model for procedure time -----------------------
# tiempo_duracion_min ~ tipo_cateter + periodo + id_institucion
m_lm <- lm(tiempo_duracion_min ~ tipo_cateter + periodo + id_institucion, data = D)

# Summaries to TXT
lm_txt_path <- file.path(res_model_dir, "model_linear_tiempo_summary.txt")
capture.output({
  cat("Linear model: tiempo_duracion_min ~ tipo_cateter + periodo + id_institucion\n\n")
  print(summary(m_lm))
  cat("\nANOVA table (sequential):\n")
  print(anova(m_lm))
  cat("\nNote: id_institucion is included as a fixed effect (many dummies). For partial pooling, consider mixed models (lme4::lmer).\n")
}, file = lm_txt_path)

# Tidy coefficients (with 95% CI)
lm_coef <- tryCatch(
  broom::tidy(m_lm, conf.int = TRUE),
  error = function(e) {
    # fallback without broom
    ci <- suppressWarnings(confint(m_lm))
    cf <- summary(m_lm)$coefficients
    out <- data.frame(
      term = rownames(cf),
      estimate = cf[, 1],
      std.error = cf[, 2],
      statistic = cf[, 3],
      p.value = cf[, 4],
      conf.low = ci[, 1],
      conf.high = ci[, 2],
      row.names = NULL
    )
    out
  }
)

#-------------- Model 2: Logistic model for hemorrhage presence ----------------
# hemo_bin ~ tipo_cateter + periodo + id_institucion
m_logit <- glm(hemo_bin ~ tipo_cateter + periodo + id_institucion,
               data = D, family = binomial(link = "logit"))

# Summaries to TXT
logit_txt_path <- file.path(res_model_dir, "model_logit_hemorragia_summary.txt")
capture.output({
  cat("Logistic model: hemo_bin ~ tipo_cateter + periodo + id_institucion\n\n")
  print(summary(m_logit))
  cat("\nLikelihood-ratio test vs. null model:\n")
  print(anova(m_logit, test = "Chisq"))
  cat("\nExponentiated coefficients (odds ratios) with Wald 95% CI:\n")
  est <- coef(m_logit)
  se  <- sqrt(diag(vcov(m_logit)))
  OR  <- exp(est)
  LCL <- exp(est - 1.96*se)
  UCL <- exp(est + 1.96*se)
  or_tab <- data.frame(term = names(OR), OR = OR, CI_low = LCL, CI_high = UCL, row.names = NULL)
  print(or_tab)
}, file = logit_txt_path)

# Tidy OR table (preferred) or fallback
logit_or <- tryCatch(
  broom::tidy(m_logit, conf.int = TRUE, exponentiate = TRUE),
  error = function(e) {
    est <- coef(m_logit)
    se  <- sqrt(diag(vcov(m_logit)))
    data.frame(
      term = names(est),
      estimate = exp(est),
      conf.low = exp(est - 1.96*se),
      conf.high = exp(est + 1.96*se),
      p.value = summary(m_logit)$coefficients[,4],
      row.names = NULL
    )
  }
)

#-------------- Diagnostics (basic figures) ------------------------------------
fig_dir <- here::here("results", "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# LM residual diagnostics
png(filename = file.path(fig_dir, "diagnostics_lm_residuals.png"),
    width = 1400, height = 700, res = 150)
par(mfrow = c(1,2))
plot(m_lm, which = 1)  # Residuals vs Fitted
plot(m_lm, which = 2)  # Normal Q-Q
par(mfrow = c(1,1))
dev.off()

# Logistic model: predicted probability by tipo_cateter x periodo at a reference institution
ref_inst <- levels(D$id_institucion)[1]
newdat <- expand.grid(
  tipo_cateter   = levels(D$tipo_cateter),
  periodo        = levels(D$periodo),
  id_institucion = ref_inst
)
newdat$pred <- predict(m_logit, newdata = newdat, type = "response")

png(filename = file.path(fig_dir, "logit_pred_prob_by_cateter_periodo.png"),
    width = 1400, height = 700, res = 150)
op <- par(mar = c(8,5,4,2))
labs <- paste(newdat$tipo_cateter, newdat$periodo, sep = " / ")
barplot(height = newdat$pred, names.arg = labs, las = 2,
        col = c("steelblue3","tomato3")[as.integer(newdat$periodo)],
        main = "Predicted probability of hemorrhage by catheter type and period\n(ref: one institution)",
        ylab = "Probability", ylim = c(0, 1))
abline(h = seq(0, 1, by = 0.1), col = "grey90", lty = "dotted")
par(op)
dev.off()

#-------------- Export combined tables to Excel --------------------------------
model_xlsx_path <- file.path(res_model_dir, "model_outputs.xlsx")
openxlsx::write.xlsx(
  x = list(
    LM_Coefficients = lm_coef,
    LOGIT_OR        = logit_or,
    Pred_Probs      = newdat
  ),
  file = model_xlsx_path, overwrite = TRUE
)

message("Models fitted and outputs exported:\n- ", lm_txt_path, "\n- ", logit_txt_path, "\n- ", model_xlsx_path)
# --- End models ---------------------------------------------------------------
>>>>>>> modelado
