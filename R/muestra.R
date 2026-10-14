#' Ruta de la base de ejemplo
#'
#' Base SIMULADA (ficticia) de un estudio cuasiexperimental: un programa
#' cognitivo-conductual para síntomas depresivos (PHQ-9) en pacientes con
#' diabetes tipo II, con grupo control en lista de espera. Está en formato
#' largo y "sucia", como llegaría del campo. Hojas: `datos_crudos`,
#' `termometro`, `diccionario` y `LEEME`.
#'
#' @return La ruta (texto) del archivo Excel incluido en el paquete.
#' @examples
#' qx_ejemplo()
#' @export
qx_ejemplo <- function() system.file("extdata", "piensa_salud.xlsx", package = "QuasiEx", mustWork = TRUE)

# Potencia de la t de dos grupos independientes (bilateral)
.potencia_t <- function(d, n1, n2, alfa) {
  gl <- n1 + n2 - 2; ncp <- d * sqrt(n1 * n2 / (n1 + n2)); tc <- stats::qt(1 - alfa / 2, gl)
  1 - stats::pt(tc, gl, ncp) + stats::pt(-tc, gl, ncp)
}

#' Sensibilidad del tamaño de muestra (paso 0)
#'
#' Calcula el efecto más pequeño (d de Cohen) que el estudio puede detectar
#' con el número de participantes que tiene. No calcula cuántos participantes
#' hacen falta: el n se justifica antes, por ejemplo con una revisión de
#' antecedentes; aquí se informa qué puede ver el estudio con ese n.
#'
#' @param n1 Participantes del grupo experimental (uno o varios escenarios).
#' @param n2 Participantes del grupo control. Por defecto, igual a `n1`.
#' @param r Correlación supuesta entre pretest y postest (para el ANCOVA).
#' @param potencia Potencia deseada.
#' @param alfa Nivel de significación (bilateral).
#'
#' @return Un resultado `qx_resultado` (lista con `tabla`, `titulo` y `notas`).
#'   La tabla tiene el d mínimo detectable con la t de dos grupos y con el
#'   ANCOVA (que ajusta por el pretest: d x raíz de 1 - r^2).
#' @examples
#' qx_muestra(n1 = c(12, 11), n2 = c(12, 10))
#' @export
qx_muestra <- function(n1, n2 = n1, r = .60, potencia = .80, alfa = .05) {
  n2 <- rep_len(n2, length(n1))
  d_t <- mapply(function(a, b) stats::uniroot(function(d) .potencia_t(d, a, b, alfa) - potencia,
                                              c(.01, 10))$root, n1, n2)
  d_a <- d_t * sqrt(1 - r^2)
  tabla <- data.frame("n experimental" = n1, "n control" = n2, "d m\u00ednimo (t)" = .f2(d_t),
                      "d m\u00ednimo (ANCOVA)" = .f2(d_a), check.names = FALSE)
  u <- length(n1)
  notas <- list(
    .nota(u, "d m\u00ednimo (ANCOVA)", sprintf(
      "Con %d y %d participantes, el estudio detecta con seguridad (potencia %s) efectos de d \u2265 %s: %s. Un efecto menor puede existir y no verse; por eso se informa el IC del efecto.",
      n1[u], n2[u], .sin0(potencia), .f2(d_a[u]),
      if (d_a[u] >= .8) "solo efectos GRANDES" else if (d_a[u] >= .5) "efectos medianos o grandes" else "incluso efectos peque\u00f1os")),
    .nota(1, "d m\u00ednimo (t)", sprintf(
      "Sin el pretest como covariable (t) har\u00eda falta d \u2265 %s. Ajustar por el pretest (r = %s) hace al estudio m\u00e1s sensible.",
      .f2(d_t[1]), .sin0(r))))
  .resultado(tabla, "Sensibilidad del tama\u00f1o de muestra (qx_muestra)", notas)
}
