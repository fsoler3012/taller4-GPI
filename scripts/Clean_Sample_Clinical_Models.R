
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