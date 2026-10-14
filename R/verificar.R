#' Supuestos del modelo (paso 8)
#'
#' Revisa el modelo de [qx_efecto()]: paralelismo de pendientes (si el efecto
#' del programa cambia según el pretest), normalidad de los residuos
#' (Shapiro-Wilk), homocedasticidad (Breusch-Pagan), igualdad de varianzas por
#' grupo (Levene con la mediana), multicolinealidad (FIV) y casos influyentes
#' (distancia de Cook mayor que 4/n).
#'
#' @param m El resultado de [qx_efecto()].
#' @return Un resultado `qx_resultado` con cada supuesto, su prueba y la lectura.
#' @examples
#' base <- qx_preparar(qx_ejemplo(), items = "PHQ", criterios = c("pre >= 10"))
#' m <- qx_efecto(base, bootstrap = 0)
#' qx_supuestos(m)
#' @export
qx_supuestos <- function(m) {
  .comprobar_efecto(m)
  b2 <- m$modelo; a <- m$datos; n <- nrow(a)
  inter <- stats::anova(b2, stats::update(b2, . ~ . + pre:grupo))
  res <- stats::residuals(b2)
  sw <- stats::shapiro.test(res)
  # Breusch-Pagan studentizado (Koenker): n * R2 de e^2 sobre los predictores
  X <- stats::model.matrix(b2)
  bp <- n * summary(stats::lm(res^2 ~ X[, -1]))$r.squared; bp_gl <- ncol(X) - 1
  bp_p <- stats::pchisq(bp, bp_gl, lower.tail = FALSE)
  # Levene con la mediana (Brown-Forsythe)
  dev <- abs(res - stats::ave(res, a$grupo, FUN = stats::median))
  lev <- stats::anova(stats::lm(dev ~ a$grupo))
  # FIV de cada predictor (sin intercepto)
  Xp <- X[, -1, drop = FALSE]
  fiv <- if (ncol(Xp) > 1) vapply(seq_len(ncol(Xp)), function(j)
    1 / (1 - summary(stats::lm(Xp[, j] ~ Xp[, -j]))$r.squared), numeric(1)) else 1
  cook <- stats::cooks.distance(b2); infl <- a$id[cook > 4 / n]
  p_int <- inter$`Pr(>F)`[2]
  tabla <- data.frame(
    Supuesto = c("Paralelismo de pendientes (pretest \u00d7 grupo)", "Normalidad de los residuos (Shapiro-Wilk)",
                 "Homocedasticidad (Breusch-Pagan)", "Igualdad de varianzas por grupo (Levene)",
                 "Multicolinealidad (FIV m\u00e1ximo)", "Casos influyentes (Cook > 4/n)"),
    Resultado = c(sprintf("F(1, %d) = %s, p %s", inter$Res.Df[2], .f2(inter$F[2]), .fpe(p_int)),
                  sprintf("W = %.3f, p %s", sw$statistic, .fpe(sw$p.value)),
                  sprintf("BP(%d) = %s, p %s", bp_gl, .f2(bp), .fpe(bp_p)),
                  sprintf("F(1, %d) = %s, p %s", lev$Df[2], .f2(lev$`F value`[1]), .fpe(lev$`Pr(>F)`[1])),
                  .f2(max(fiv)), if (length(infl)) paste(infl, collapse = ", ") else "Ninguno"),
    check.names = FALSE)
  notas <- list(
    .nota(1, "Resultado", if (p_int >= .05)
      "p \u2265 .05: no hay evidencia de que el efecto cambie seg\u00fan el pretest. Las pendientes son paralelas y el ANCOVA es v\u00e1lido." else
      "p < .05: el efecto del programa depende del nivel inicial. No basta un solo B: hay que informar la interacci\u00f3n."),
    .nota(5, "Resultado", sprintf("FIV = %s %s 5: %s", .f2(max(fiv)), if (max(fiv) < 5) "<" else "\u2265",
      if (max(fiv) < 5) "las covariables no se pisan entre s\u00ed." else "multicolinealidad: revisa las covariables.")),
    .nota(6, "Resultado", if (length(infl) == 0) "Ning\u00fan participante arrastra el resultado por s\u00ed solo." else
      sprintf("Casos influyentes (%s): rev\u00edsalos e informa el an\u00e1lisis sin ellos como sensibilidad.",
              paste(infl, collapse = ", "))))
  if (sw$p.value < .05 || bp_p < .05) notas[[4]] <- .nota(if (sw$p.value < .05) 2 else 3, "Resultado",
    "Un supuesto no se cumple: el IC por bootstrap del efecto y los errores robustos (qx_sensibilidad) no dependen de \u00e9l.")
  .resultado(tabla, "Supuestos del modelo (qx_supuestos)", notas)
}

# IC de Newcombe (híbrido de Wilson) para la diferencia de dos proporciones
.newcombe <- function(x1, n1, x2, n2, z = 1.96) {
  w <- function(x, n) { p <- x / n; c((p + z^2 / (2 * n) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n),
                                      (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n)) }
  p1 <- x1 / n1; p2 <- x2 / n2; l1 <- w(x1, n1); l2 <- w(x2, n2)
  c(p1 - p2 - sqrt((p1 - l1[1])^2 + (l2[2] - p2)^2), p1 - p2 + sqrt((l1[2] - p1)^2 + (p2 - l2[1])^2))
}

#' Cambio clínicamente significativo (paso 9)
#'
#' Clasifica a cada participante con el índice de cambio fiable (RCI) de
#' Jacobson y Truax: mejoría confiable, sin cambio confiable o deterioro
#' confiable. Si se da un punto de corte clínico, cuenta además la
#' recuperación (mejoría confiable y cruzar el corte). Compara los porcentajes
#' de ambos grupos con el IC de Newcombe.
#'
#' @param m El resultado de [qx_efecto()].
#' @param fiabilidad Fiabilidad del instrumento (por ejemplo, el omega de la
#'   validación en tu población).
#' @param corte Punto de corte clínico (opcional), por ejemplo 10 en el PHQ-9.
#' @param mejora `"baja"` si mejorar es bajar el puntaje (síntomas) o
#'   `"sube"` si es subirlo (bienestar).
#' @return Un resultado `qx_resultado` con los porcentajes por grupo, la
#'   diferencia con su IC y la lectura.
#' @examples
#' base <- qx_preparar(qx_ejemplo(), items = "PHQ", criterios = c("pre >= 10"))
#' m <- qx_efecto(base, bootstrap = 0)
#' qx_clinico(m, fiabilidad = .892, corte = 10)
#' @export
qx_clinico <- function(m, fiabilidad, corte = NULL, mejora = c("baja", "sube")) {
  .comprobar_efecto(m); mejora <- match.arg(mejora)
  a <- m$datos; g <- a$grupo == "Experimental"
  s_dif <- sqrt(2) * .de_comb(a$pre, g) * sqrt(1 - fiabilidad)
  rci <- (a$post - a$pre) / s_dif
  if (mejora == "sube") rci <- -rci
  estado <- ifelse(rci <= -1.96, "Mejor\u00eda confiable", ifelse(rci >= 1.96, "Deterioro confiable", "Sin cambio confiable"))
  cats <- c("Mejor\u00eda confiable", "Sin cambio confiable", "Deterioro confiable")
  ind <- lapply(cats, function(k) estado == k); names(ind) <- cats
  if (!is.null(corte)) {
    cruza <- if (mejora == "baja") a$pre >= corte & a$post < corte else a$pre < corte & a$post >= corte
    ind[["Recuperaci\u00f3n cl\u00ednica"]] <- estado == cats[1] & cruza
  }
  filas <- lapply(names(ind), function(k) {
    x <- ind[[k]]; x1 <- sum(x[g]); n1 <- sum(g); x2 <- sum(x[!g]); n2 <- sum(!g); icn <- .newcombe(x1, n1, x2, n2)
    data.frame("Categor\u00eda" = k, Experimental = sprintf("%d (%.1f %%)", x1, 100 * x1 / n1),
               Control = sprintf("%d (%.1f %%)", x2, 100 * x2 / n2),
               "Diferencia [IC 95 %]" = sprintf("%s %s", .f2(x1 / n1 - x2 / n2), .ic(icn[1], icn[2])),
               p1 = x1 / n1, p2 = x2 / n2, li = icn[1], ls = icn[2], check.names = FALSE)
  })
  t <- do.call(rbind, filas)
  ir <- nrow(t)
  notas <- list(
    .nota(ir, "Diferencia [IC 95 %]", sprintf(
      "%s: %.1f %% del experimental frente a %.1f %% del control (%s puntos porcentuales); el IC %s el cero.",
      t[ir, 1], 100 * t$p1[ir], 100 * t$p2[ir], .f2(100 * (t$p1[ir] - t$p2[ir])),
      if (.incluye0(t$li[ir], t$ls[ir])) "incluye" else "no incluye")),
    .nota(1, "Experimental", sprintf(
      "Mejor\u00eda confiable = %s %s puntos o m\u00e1s (RCI de Jacobson y Truax, fiabilidad %s): un cambio mayor que el error de medida del instrumento.",
      if (mejora == "baja") "bajar" else "subir", .f2(1.96 * s_dif), .f3(fiabilidad))))
  .resultado(t[, 1:4], "Cambio cl\u00ednicamente significativo (qx_clinico)", notas, cambio_minimo = 1.96 * s_dif)
}

# Prueba de Yuen sobre el cambio (medias recortadas al 20 %): experimental - control
.yuen <- function(x, g, tr = .2) {
  par <- lapply(list(x[g], x[!g]), function(v) {
    v <- sort(v[!is.na(v)]); n <- length(v); h <- n - 2 * floor(tr * n)
    lo <- v[floor(tr * n) + 1]; hi <- v[n - floor(tr * n)]
    w <- pmin(pmax(v, lo), hi)
    c(mt = mean(v, trim = tr), d = (n - 1) * stats::var(w) / (h * (h - 1)), h = h)
  })
  dif <- par[[1]]["mt"] - par[[2]]["mt"]; se <- sqrt(par[[1]]["d"] + par[[2]]["d"])
  gl <- (par[[1]]["d"] + par[[2]]["d"])^2 / (par[[1]]["d"]^2 / (par[[1]]["h"] - 1) + par[[2]]["d"]^2 / (par[[2]]["h"] - 1))
  tc <- stats::qt(.975, gl)
  unname(c(dif, dif - tc * se, dif + tc * se))
}

#' Análisis de sensibilidad (paso 10)
#'
#' Repite la estimación del efecto con otras decisiones para ver si la
#' conclusión se mantiene: errores robustos HC3, un modelo mixto grupo x
#' momento con TODOS los asignados (también quienes no tienen postest), el
#' análisis por protocolo (solo quienes asistieron a suficientes sesiones) y la
#' prueba de Yuen sobre el cambio (medias recortadas, robusta a atípicos).
#' Todas las estimaciones son experimental menos control. Incluye además una
#' tabla de amenazas a la validez interna.
#'
#' @param m El resultado de [qx_efecto()].
#' @param asistencia Columna con las sesiones asistidas (solo del grupo
#'   experimental). Si no existe, se omite el análisis por protocolo.
#' @param minimo Sesiones mínimas para el análisis por protocolo. Por
#'   defecto, el 80 % del máximo observado.
#' @return Un resultado `qx_resultado` con cada análisis, su estimación, su IC
#'   y la lectura; en `amenazas`, otro `qx_resultado` con las amenazas a la
#'   validez interna y cómo se abordan.
#' @examples
#' base <- qx_preparar(qx_ejemplo(), items = "PHQ", criterios = c("pre >= 10"))
#' m <- qx_efecto(base, bootstrap = 200)
#' qx_sensibilidad(m)
#' @export
qx_sensibilidad <- function(m, asistencia = "asistencia", minimo = NULL) {
  .comprobar_efecto(m)
  b2 <- m$modelo; a <- m$datos; cv <- m$covariables
  # 1. HC3
  X <- stats::model.matrix(b2); e <- stats::residuals(b2); h <- stats::hatvalues(b2)
  XtXi <- solve(crossprod(X))
  Vhc3 <- XtXi %*% crossprod(X * (e / (1 - h))) %*% XtXi
  se_hc3 <- sqrt(diag(Vhc3))["grupoExperimental"]
  ic_hc3 <- m$B + c(-1, 1) * stats::qt(.975, stats::df.residual(b2)) * se_hc3
  # 2. Modelo mixto con todos los asignados
  base <- as.data.frame(m$base)
  for (v in cv) if (is.logical(base[[v]])) base[[v]] <- as.numeric(base[[v]])
  l <- .largo(m$base)
  if (length(cv)) l <- merge(l, base[, c("id", cv)], by = "id")
  l <- l[!is.na(l$valor), ]
  fm <- stats::reformulate(c("momento * grupo", cv, "(1 | id)"), "valor")
  mx <- suppressMessages(lme4::lmer(fm, data = l))
  cm <- summary(mx)$coefficients["momentoPostest:grupoExperimental", ]
  ic_m <- cm["Estimate"] + c(-1, 1) * 1.96 * cm["Std. Error"]
  filas <- list(
    c("Principal: ANCOVA (IC bootstrap)", nrow(a), m$B,
      if (!anyNA(m$ic_bootstrap)) m$ic_bootstrap else m$ic),
    c("Errores robustos HC3", nrow(a), m$B, ic_hc3),
    c("Modelo mixto grupo \u00d7 momento (todos los asignados)", length(unique(l$id)), cm["Estimate"], ic_m))
  # 3. Por protocolo
  hay_pp <- asistencia %in% names(a) && any(!is.na(a[[asistencia]]))
  if (hay_pp) {
    if (is.null(minimo)) minimo <- ceiling(.8 * max(a[[asistencia]], na.rm = TRUE))
    pp <- a[a$grupo == "Control" | (!is.na(a[[asistencia]]) & a[[asistencia]] >= minimo), ]
    mpp <- stats::update(b2, data = pp)
    filas[[4]] <- c(sprintf("Por protocolo (asistencia \u2265 %s)", minimo), nrow(pp),
                    stats::coef(mpp)["grupoExperimental"], stats::confint(mpp)["grupoExperimental", ])
  }
  # 4. Yuen
  filas[[length(filas) + 1]] <- c("Yuen sobre el cambio (medias recortadas)", nrow(a),
                                  .yuen(a$post - a$pre, a$grupo == "Experimental"))
  t <- as.data.frame(do.call(rbind, filas), stringsAsFactors = FALSE)
  names(t) <- c("An\u00e1lisis", "n", "est", "li", "ls")
  for (v in c("n", "est", "li", "ls")) t[[v]] <- as.numeric(t[[v]])
  tabla <- data.frame("An\u00e1lisis" = t[[1]], n = as.integer(t$n), "Estimaci\u00f3n" = .f2(t$est),
                      "IC 95 %" = .ic(t$li, t$ls), check.names = FALSE)
  im <- 3; ip <- if (hay_pp) 4 else NA
  mismo <- all(sign(t$est) == sign(m$B))
  notas <- list(
    .nota(im, "Estimaci\u00f3n", sprintf(
      "%s: el modelo mixto compara el CAMBIO.%s",
      .f2(t$est[im]), if (abs(t$est[im] - m$B) > .25 * abs(m$B))
        " Difiere del ANCOVA porque los grupos partieron distinto (paradoja de Lord); el principal es el ANCOVA, que compara el postest a igual pretest." else
        " Coincide con el ANCOVA.")))
  if (hay_pp) notas[[2]] <- .nota(ip, "IC 95 %", sprintf("Por protocolo (n = %d): el IC %s el cero.",
    as.integer(t$n[ip]), if (.incluye0(t$li[ip], t$ls[ip])) "incluye o roza" else "no incluye"))
  notas[[length(notas) + 1]] <- .nota(nrow(t), "Estimaci\u00f3n", if (mismo)
    "Todas las estimaciones apuntan en la misma direcci\u00f3n: la conclusi\u00f3n no depende de una sola decisi\u00f3n." else
    "No todas apuntan en la misma direcci\u00f3n: la conclusi\u00f3n depende de decisiones del an\u00e1lisis; inf\u00f3rmalo.")
  perd <- nrow(m$base) - nrow(a)
  amen <- data.frame(
    Amenaza = c("Selecci\u00f3n (grupos no equivalentes)", "Regresi\u00f3n a la media", "Historia", "Maduraci\u00f3n",
                "Mortalidad experimental (p\u00e9rdidas)", "Efecto de la prueba"),
    "C\u00f3mo se aborda" = c("Pretest y covariables en el bloque 1; DME en la equivalencia inicial",
                        "El grupo control la estima y el ANCOVA la descuenta",
                        "Grupo control medido en las mismas fechas",
                        "Mismo intervalo pretest-postest en ambos grupos",
                        sprintf("%d p\u00e9rdida(s); modelo mixto con todos los asignados", perd),
                        "Ambos grupos respondieron el mismo instrumento dos veces"),
    "Queda para Limitaciones" = c("Confusoras no medidas", "", "Sucesos propios de un grupo", "", "", ""),
    check.names = FALSE)
  .resultado(tabla, "An\u00e1lisis de sensibilidad (qx_sensibilidad)", notas,
             amenazas = .resultado(amen, "Amenazas a la validez interna", list(
               .nota(1, "Queda para Limitaciones",
                     "Sin asignaci\u00f3n aleatoria siempre puede haber una confusora que no se midi\u00f3: el dise\u00f1o no la controla y se declara en Limitaciones."),
               .nota(2, "C\u00f3mo se aborda",
                     "La regresi\u00f3n a la media es la amenaza propia de entrar por un puntaje alto: el grupo control permite estimarla."))))
}
