# =============================================================================
# Utilidades para datos clínicos (sin dependencias externas)
# =============================================================================

#' Verifica columnas requeridas y devuelve las faltantes (o character(0) si OK)
assert_cols <- function(df, cols) {
  faltan <- setdiff(cols, names(df))
  if (length(faltan)) {
    warning("Faltan columnas: ", paste(faltan, collapse = ", "))
  }
  invisible(faltan)
}

#' Convierte 'presencia_hemorragia' en binaria (1 = sí), crea 'hemo_bin'
make_hemo_bin <- function(df, input_col = "presencia_hemorragia", out_col = "hemo_bin") {
  if (!input_col %in% names(df)) stop("No existe la columna: ", input_col)
  x <- tolower(trimws(as.character(df[[input_col]])))
  df[[out_col]] <- ifelse(x %in% c("si","sí","yes","true","1","y","t"), 1L, 0L)
  df
}

#' Coacciona fechas robustamente (Date): num (serial Excel), POSIXct, character
coerce_to_Date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  suppressWarnings({
    y <- as.Date(x)
    if (all(is.na(y))) {
      # Intento genérico sin dependencias (puedes adaptar formatos)
      fmts <- c("%Y-%m-%d","%d/%m/%Y","%m/%d/%Y","%d-%m-%Y","%Y/%m/%d")
      for (f in fmts) {
        y <- as.Date(x, format = f)
        if (!all(is.na(y))) break
      }
    }
    y
  })
}