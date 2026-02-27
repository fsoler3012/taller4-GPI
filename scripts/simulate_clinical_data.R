# simulate_clinical_data

if (!requireNamespace("here", quietly = TRUE)) install.packages("here", repos = "https://cloud.r-project.org")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl", repos = "https://cloud.r-project.org")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx", repos = "https://cloud.r-project.org")


library(here); library(readxl); library(openxlsx)

# ---- Reproducibility ----
set.seed(20260203)  # <- fixed seed for reproducibility

# ---- Small helpers ----
# Apply one of three styles: Title Case, UPPERCASE, lowercase
style_case <- function(s) {
  st <- sample(c("title","upper","lower"), 1)
  if (st == "title") {
    return(tools::toTitleCase(tolower(s)))
  } else if (st == "upper") {
    return(toupper(s))
  } else {
    return(tolower(s))
  }
}

# Build a styled full name
make_name_styled <- function(firsts, lasts) {
  fn <- sample(firsts, 1)
  ln <- sample(lasts, 1)
  style_case(paste(fn, ln))
}

# Clamp utility
clamp <- function(x, minv, maxv) pmax(minv, pmin(maxv, x))

# ---- Institutions & Acronyms (reference; NOT exported) ----
# Numeric IDs for institutions
id_insts <- 1:4
acronyms <- c("HUP","IMC","CHN","HCL")
fullnames <- c(
  "Hospital Universitario del Pacífico",
  "Instituto Médico de Cardiología",
  "Centro Hospitalario Nacional",
  "Hospital Central La Luz"
)

# Mapping table (kept in code; NOT exported)
tabla_correspondencia_acronimos <- data.frame(
  id_institucion = id_insts,
  acronimo = acronyms,
  nombre_completo = fullnames,
  stringsAsFactors = FALSE
)

# Display name for EP table: "ACR - <styled full name>"
inst_display <- paste(acronyms, "-", sapply(fullnames, style_case))

# ---- Electrophysiologists (40 across 4 institutions) ----
first_names <- c("Carlos","María","José","Ana","Luis","Laura","Francisco","Lucía","Javier","Elena",
                 "Diego","Paula","Andrés","Sofía","Miguel","Valentina","Pablo","Camila","Sergio","Isabel")
last_names  <- c("García","Martinez","Rodriguez","Lopez","Gonzalez","Perez","Sanchez","Ramirez","Torres","Flores",
                 "Diaz","Vargas","Castro","Morales","Navarro","Cruz","Rojas","Iglesias","Mendez","Suarez")

# Roles distribution per institution: 4 Fellows, 3 Avanzado, 3 Faculty
role_block <- c(rep("Fellow", 4), rep("Avanzado", 3), rep("Faculty", 3))

EP_list <- list()
for (i in seq_along(id_insts)) {
  roles_i <- sample(role_block)
  years   <- sapply(roles_i, function(r){
    if (r == "Fellow") sample(2:3, 1)
    else if (r == "Avanzado") sample(5:10, 1)
    else sample(11:30, 1)
  })
  countries <- sample(c("Colombia","México","España","Chile","Argentina","Perú"), 10, replace = TRUE)
  EP_list[[i]] <- data.frame(
    id_institucion = rep(id_insts[i], 10),
    nombre_institucion = rep(inst_display[i], 10),  # subtly styled
    id_electrofisiologo = (i-1)*10 + seq_len(10),   # numeric IDs 1..40
    nombre_electrofisiologo = replicate(10, make_name_styled(first_names, last_names)),
    anios_experiencia = years,                      # numeric
    pais_cirujano = countries,
    cargo = roles_i,
    stringsAsFactors = FALSE
  )
}

tabla_electrofisiologos <- do.call(rbind, EP_list)

# ---- Simulation parameters ----
# Mean procedure time in minutes by role (interpreted as baseline without improvements)
base_time <- c(Fellow = 120, Avanzado = 110, Faculty = 105)
# Standard deviation (variability) around the mean time, in minutes, by role
base_sd   <- c(Fellow = 18,  Avanzado = 17,  Faculty = 16)
# Base probability of hemorrhage (p_h) by role (as a proportion 0..1)
base_p_h  <- c(Fellow = 0.18, Avanzado = 0.15, Faculty = 0.12)

# ---- Tuning knobs: hemorrhage prevalence ----
# 1. Historical lift (keep 1.00 if you don't want to change historical frequency)
hist_factor <- 1.00         # e.g., 1.10 = +10% relative increase in historical hemorrhage prob
# 2. Experimental 'sin_mejoras' lift (main driver you asked for)
sin_mejoras_factor <- 1.35  # e.g., 1.35 = +35% relative increase in sin_mejoras hemorrhage prob
# 3. Safety cap so probabilities don't get unrealistic
p_cap <- 0.70               # max allowed probability

# Ergonomic improvement effects + prevalence tuning
get_params <- function(role, tipo){
  mu <- base_time[[role]]
  sd <- base_sd[[role]]
  p  <- base_p_h[[role]]
  
  if (is.na(tipo)) {
    # Historical cases: optional global lift
    p <- p * hist_factor
    
  } else if (tipo == "sin_mejoras") {
    # Experimental control arm: increase hemorrhage probability
    p <- p * sin_mejoras_factor
    
  } else if (tipo == "con_mejoras") {
    # Keep existing ergonomic effects
    if (role == "Fellow") {
      mu <- mu * 0.82
      p  <- p  * 0.92
    } else if (role == "Avanzado") {
      mu <- mu * 0.92
      p  <- p  * 0.65
    } else if (role == "Faculty") {
      mu <- mu * 0.90
      p  <- p  * 0.60
    }
  }
  
  # Safety clamp
  p <- max(0, min(p, p_cap))
  list(mu = mu, sd = sd, p = p)
}

# Hemorrhage volume rule:
# - For improved catheters (con_mejoras): enforce 0% 'alto'; split 50% 'bajo', 50% 'medio'.
# - For non-improved/historical: baseline 50% bajo, 35% medio, 15% alto.
hemorrhage_volume <- function(role, tipo){
  if (!is.na(tipo) && tipo == "con_mejoras") {
    probs <- c(0.5, 0.5, 0.0)  # bajo, medio, alto
  } else {
    probs <- c(0.50, 0.35, 0.15)
  }
  sample(c("bajo","medio","alto"), 1, prob = probs)
}

# ---- Table 2: 160 procedures (40 per institution) with ergonomic vs non-ergonomic ----
procedimientos_erg <- list()
for (i in seq_along(id_insts)) {
  inst_id <- id_insts[i]
  idx_ep  <- which(tabla_electrofisiologos$id_institucion == inst_id)
  n_cases <- 40
  tipos   <- sample(c(rep("con_mejoras", n_cases/2), rep("sin_mejoras", n_cases/2)))
  
  start_date <- as.Date("2025-05-01")
  end_date   <- as.Date("2025-06-30")
  dates <- start_date + sample(0:as.integer(end_date - start_date), n_cases, replace = TRUE)
  
  ep_ids <- sample(tabla_electrofisiologos$id_electrofisiologo[idx_ep], n_cases, replace = TRUE)
  cargos <- tabla_electrofisiologos$cargo[match(ep_ids, tabla_electrofisiologos$id_electrofisiologo)]
  
  # simulate
  times <- integer(n_cases)
  hem   <- character(n_cases)
  vol   <- character(n_cases)
  
  for (k in seq_len(n_cases)) {
    role <- cargos[k]
    tipo <- tipos[k]
    params <- get_params(role, tipo)
    
    tmin <- rnorm(1, mean = params$mu, sd = params$sd)
    tmin <- clamp(round(tmin), 60, 240)
    times[k] <- tmin
    
    has_h <- runif(1) < params$p
    hem[k] <- if (has_h) "si" else "no"
    vol[k] <- if (has_h) hemorrhage_volume(role, tipo) else NA_character_
  }
  
  procedimientos_erg[[i]] <- data.frame(
    id_institucion = rep(inst_id, n_cases),                 # numeric
    id_electrofisiologo = ep_ids,                           # numeric
    fecha_procedimiento = dates,                            # Date
    id_cateter = paste0("CAT", sprintf("%04d", sample(1:9999, n_cases, TRUE))),
    tipo_cateter = tipos,
    tiempo_duracion_min = times,                            # numeric
    presencia_hemorragia = hem,                             # "si"/"no"
    volumen_hemorragia = vol,                               # NA when no
    stringsAsFactors = FALSE
  )
}
procedimientos_erg <- do.call(rbind, procedimientos_erg)

# ---- Table 3: 200 historical procedures (no tipo_cateter) ----
historicos <- list()
for (i in seq_along(id_insts)) {
  inst_id <- id_insts[i]
  idx_ep  <- which(tabla_electrofisiologos$id_institucion == inst_id)
  n_cases <- 50
  
  start_date <- as.Date("2025-01-01")
  end_date   <- as.Date("2025-04-30")
  dates <- start_date + sample(0:as.integer(end_date - start_date), n_cases, replace = TRUE)
  
  ep_ids <- sample(tabla_electrofisiologos$id_electrofisiologo[idx_ep], n_cases, replace = TRUE)
  cargos <- tabla_electrofisiologos$cargo[match(ep_ids, tabla_electrofisiologos$id_electrofisiologo)]
  
  times <- integer(n_cases)
  hem   <- character(n_cases)
  vol   <- character(n_cases)
  
  for (k in seq_len(n_cases)) {
    role <- cargos[k]
    params <- get_params(role, NA)
    
    tmin <- rnorm(1, mean = params$mu, sd = params$sd)
    tmin <- clamp(round(tmin), 60, 240)
    times[k] <- tmin
    
    has_h <- runif(1) < params$p
    hem[k] <- if (has_h) "si" else "no"
    vol[k] <- if (has_h) hemorrhage_volume(role, NA) else NA_character_
  }
  
  historicos[[i]] <- data.frame(
    id_institucion = rep(inst_id, n_cases),
    id_electrofisiologo = ep_ids,
    fecha_procedimiento = dates,
    id_cateter = paste0("CAT", sprintf("%04d", sample(1:9999, n_cases, TRUE))),
    tiempo_duracion_min = times,
    presencia_hemorragia = hem,
    volumen_hemorragia = vol,
    stringsAsFactors = FALSE
  )
}
historicos <- do.call(rbind, historicos)

here::i_am("scripts/simulate_clinical_data.R")

out_dir  <- here::here("data", "raw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ep_path   <- file.path(out_dir, "electrofisiologos.xlsx")
proc_path <- file.path(out_dir, "procedimientos_may_jun_2025.xlsx")
hist_path <- file.path(out_dir, "historicos_ene_abr_2025.xlsx")

openxlsx::write.xlsx(tabla_electrofisiologos, file = ep_path, overwrite = TRUE)
openxlsx::write.xlsx(procedimientos_erg, file = proc_path, overwrite = TRUE)
openxlsx::write.xlsx(historicos, file = hist_path, overwrite = TRUE)

cat("Datos simulados creados con éxito")

renv::snapshot()
