# Estadísticas descriptivas rápidas
summary(D)

# Resumen numérico de variables clave
summary(D[, c("tiempo_duracion_min", "volumen_hemorragia")])

# Conteos de categorías
table(D$tipo_cateter)
table(D$periodo)
table(D$presencia_hemorragia)

# Resumen por tipo de catéter
aggregate(cbind(tiempo_duracion_min, volumen_hemorragia) ~ tipo_cateter,
          data = D, FUN = unction(x) c(mean=mean(x,na.rm=TRUE),
                                        sd=sd(x,na.rm=TRUE),
                                        med=median(x,na.rm=TRUE)))

# Resumen por institución
aggregate(tiempo_duracion_min ~ id_institucion, data = D, mean, na.rm=TRUE)