.etiquetas_coef <- function(nombres, logicas) {
  e <- nombres
  e[e == "(Intercept)"] <- "Intercepto"
  e[e == "pre"] <- "Pretest"
  e[e == "grupoExperimental"] <- "Grupo experimental"
  e[e %in% logicas] <- paste0(e[e %in% logicas], " (s\u00ed)")
  e
}

#' Efecto del programa (paso 7)
#'
#' Estima el efecto del programa con una regresión jerárquica (ANCOVA): el
#' bloque 1 tiene el pretest y las covariables (lo que ya separaba a los grupos
#' ANTES del programa) y el bloque 2 agrega el grupo. Informa cuánto aporta el
#' programa (Delta R2), su coeficiente B con IC 95 % clásico y por bootstrap,
#' las medias ajustadas, la d ajustada y el mismo efecto estimado sin ajustar y
#' ajustando solo por el pretest, para ver qué hacen las covariables.
#'
#' @param base La base que devuelve [qx_preparar()].
#' @param covariables Variables de confusión además del pretest (que siempre
#'   entra). Elígelas ANTES, con la teoría y un diagrama causal: deben influir
#'   en quién recibió el programa y en el desenlace. No incluyas mediadoras ni
#'   variables medidas después de empezar el programa.
#' @param bootstrap Número de remuestreos para el IC del efecto (estratificados
#'   por grupo). Con 0 no se calcula.
#' @param semilla Semilla del bootstrap (no altera el generador de la sesión).
#'
#' @return Un objeto `qx_efecto`: lista con `bloques`, `coeficientes`,
#'   `medias` y `estimaciones` (cada uno un `qx_resultado` con tabla y
#'   lectura), el efecto `B`, su `ic_bootstrap`, la `d` ajustada, los modelos
#'   (`modelo`, `bloque1`) y los datos usados. Con `plot()` se obtiene la figura
#'   de las tres estimaciones o la de medias.
#' @examples
#' base <- qx_preparar(qx_ejemplo(), items = "PHQ", criterios = c("pre >= 10"))
#' base$insulina <- base$tratamiento_dm != "Oral"
#' m <- qx_efecto(base, covariables = c("tiempo_dm", "insulina"), bootstrap = 200)
#' m
#' plot(m)
#' @export
qx_efecto <- function(base, covariables = NULL, bootstrap = 5000, semilla = 2027) {
  .comprobar_base(base)
  logicas <- covariables[vapply(covariables, function(v) is.logical(base[[v]]), logical(1))]
  a <- .datos_modelo(base, covariables)
  if (length(unique(a$grupo)) < 2) stop("Hacen falta los dos grupos con pretest y postest.", call. = FALSE)
  f1 <- stats::reformulate(c("pre", covariables), "post")
  f2 <- stats::reformulate(c("pre", covariables, "grupo"), "post")
  b1 <- stats::lm(f1, data = a); b2 <- stats::lm(f2, data = a)
  # la fórmula queda escrita en el modelo (para poder reestimarlo con update())
  b1$call$formula <- f1; b2$call$formula <- f2
  cf <- stats::anova(b1, b2)
  r2 <- c(summary(b1)$r.squared, summary(b2)$r.squared)
  r2a <- c(summary(b1)$adj.r.squared, summary(b2)$adj.r.squared)
  co <- summary(b2)$coefficients; ic <- stats::confint(b2)
  B <- co["grupoExperimental", 1]

  ic_b <- c(NA, NA)
  if (bootstrap > 0) {
    ge <- which(a$grupo == "Experimental"); gc <- which(a$grupo == "Control")
    bs <- .con_semilla(semilla, replicate(bootstrap, {
      i <- c(sample(ge, replace = TRUE), sample(gc, replace = TRUE))
      tryCatch(stats::coef(stats::lm(f2, data = a[i, ]))["grupoExperimental"], error = function(e) NA)
    }))
    ic_b <- stats::quantile(bs, c(.025, .975), na.rm = TRUE, names = FALSE)
  }

  # Medias ajustadas: predicción con pretest y covariables en su media
  X <- stats::model.matrix(b2); V <- stats::vcov(b2)
  xm <- colMeans(X)
  med <- lapply(c("Control", "Experimental"), function(gr) {
    x <- xm; x["grupoExperimental"] <- as.numeric(gr == "Experimental")
    e <- sum(x * stats::coef(b2)); s <- sqrt(as.numeric(t(x) %*% V %*% x))
    tc <- stats::qt(.975, stats::df.residual(b2))
    c(e, s, e - tc * s, e + tc * s)
  })
  de_pre <- .de_comb(a$pre, a$grupo == "Experimental")
  d_aj <- B / de_pre

  m_sin <- stats::lm(post ~ grupo, data = a); m_pre <- stats::lm(post ~ pre + grupo, data = a)
  est <- function(m) c(stats::coef(m)["grupoExperimental"], stats::confint(m)["grupoExperimental", ])
  E <- rbind(est(m_sin), est(m_pre), est(b2))
  nom_cov <- if (length(covariables)) paste(covariables, collapse = " + ") else "sin covariables"

  # --- Tablas con su lectura ---
  t_bloq <- data.frame(Modelo = c(paste("Bloque 1: pretest", if (length(covariables)) paste("+", nom_cov)),
                                  "Bloque 2: + grupo"),
                       "R\u00b2" = .f3(r2), "R\u00b2 aj." = .f3(r2a), "\u0394R\u00b2" = c("", .f3(r2[2] - r2[1])),
                       "F del cambio" = c("", sprintf("F(1, %d) = %s", cf$Res.Df[2], .f2(cf$F[2]))),
                       p = c("", .fp(cf$`Pr(>F)`[2])), check.names = FALSE)
  p_c <- cf$`Pr(>F)`[2]
  n_bloq <- list(
    .nota(2, "\u0394R\u00b2", sprintf(
      "\u0394R\u00b2 = %s: el programa explica un %s %% M\u00c1S de la variaci\u00f3n del postest, por encima de lo que ya explicaban el pretest y las covariables.",
      .f3(r2[2] - r2[1]), .f2(100 * (r2[2] - r2[1])))),
    .nota(2, "p", sprintf("p = %s %s .05: %s Con n peque\u00f1o, mira sobre todo el IC del efecto.", .fp(p_c),
      if (p_c < .05) "<" else "\u2265",
      if (p_c < .05) "ese aporte dif\u00edcilmente se debe al azar." else "el aporte no se distingue del azar.")),
    .nota(1, "R\u00b2", sprintf(
      "R\u00b2 = %s: el bloque 1 (lo que separaba a los grupos ANTES) ya explica el %s %% del postest.",
      .f3(r2[1]), .f2(100 * r2[1]))))

  t_coef <- data.frame(Predictor = .etiquetas_coef(rownames(co), logicas), B = .f2(co[, 1]), EE = .f2(co[, 2]),
                       "IC 95 %" = .ic(ic[, 1], ic[, 2]), t = .f2(co[, 3]), p = .fp(co[, 4]),
                       "IC 95 % bootstrap" = "", check.names = FALSE)
  ig <- which(rownames(co) == "grupoExperimental")
  if (bootstrap > 0) t_coef[ig, "IC 95 % bootstrap"] <- .ic(ic_b[1], ic_b[2])
  icg <- if (bootstrap > 0) ic_b else ic[ig, ]
  n_coef <- list(
    .nota(ig, "B", sprintf(
      "B = %s: a IGUAL %s, el grupo experimental termina con %s puntos %s que el control. Es el efecto del programa.",
      .f2(B), .lista(c("pretest", covariables), prefijo = TRUE),
      .f2(abs(B)), if (B < 0) "MENOS" else "M\u00c1S")),
    .nota(ig, if (bootstrap > 0) "IC 95 % bootstrap" else "IC 95 %", sprintf("IC %s: %s",
      .ic(icg[1], icg[2]), if (.incluye0(icg[1], icg[2])) "incluye el cero: el efecto no se distingue de nulo." else
        sprintf("no incluye el cero; el efecto est\u00e1 entre %s y %s puntos: real, pero mira su amplitud.",
                .f2(min(abs(icg))), .f2(max(abs(icg)))))))
  if (length(covariables)) n_coef[[3]] <- .nota(3, "p",
    "Las covariables est\u00e1n para AJUSTAR, no son hip\u00f3tesis: sus B y p no se interpretan como hallazgos.")

  t_med <- data.frame(Grupo = c("Control", "Experimental"), "Media ajustada" = .f2(sapply(med, `[`, 1)),
                      EE = .f2(sapply(med, `[`, 2)), "IC 95 %" = .ic(sapply(med, `[`, 3), sapply(med, `[`, 4)),
                      check.names = FALSE)
  n_med <- list(
    .nota(2, "Media ajustada", sprintf(
      "%s \u2212 %s = %s = B. La media ajustada es el postest que tendr\u00eda cada grupo si ambos hubieran partido iguales.",
      .f2(med[[2]][1]), .f2(med[[1]][1]), .f2(med[[2]][1] - med[[1]][1]))),
    .nota(1, "Media ajustada", sprintf(
      "d ajustada = %s (B / DE combinada del pretest): efecto %s en desviaciones est\u00e1ndar. Es el tama\u00f1o del efecto que se informa.",
      .f2(d_aj), .magnitud(d_aj))))

  t_est <- data.frame(Modelo = c("Sin ajustar (solo postest)", "Ajustando solo el pretest",
                                 if (length(covariables)) "Pretest + covariables" else "Pretest (modelo principal)"),
                      B = .f2(E[, 1]), "IC 95 %" = .ic(E[, 2], E[, 3]), check.names = FALSE)
  n_est <- list(.nota(3, "B", sprintf(
    "Tres estimaciones del mismo efecto con los mismos datos: %s, %s y %s. Si cambian al ajustar, las variables de partida estaban mezcladas con el efecto. Las covariables se eligen ANTES, con la teor\u00eda, no por el resultado.",
    .f2(E[1, 1]), .f2(E[2, 1]), .f2(E[3, 1]))))

  structure(list(
    bloques = .resultado(t_bloq, "Regresi\u00f3n jer\u00e1rquica: lo que aporta el programa", n_bloq),
    coeficientes = .resultado(t_coef, "Coeficientes del modelo completo", n_coef),
    medias = .resultado(t_med, "Medias ajustadas del postest", n_med),
    estimaciones = .resultado(t_est, "El mismo efecto con distintos ajustes", n_est),
    B = B, ic = ic[ig, ], ic_bootstrap = ic_b, d = d_aj, delta_r2 = r2[2] - r2[1], F = cf$F[2],
    gl = c(1, cf$Res.Df[2]), p = p_c, modelo = b2, bloque1 = b1, datos = a, base = base,
    covariables = covariables, estimaciones_num = E), class = "qx_efecto")
}

#' @export
print.qx_efecto <- function(x, ...) {
  cat("\nEfecto del programa (qx_efecto)\n===============================\n")
  cat(sprintf("B = %s, IC 95 %%%s %s; d ajustada = %s; \u0394R\u00b2 = %s, F(%d, %d) = %s, p %s; n = %d\n",
              .f2(x$B), if (!anyNA(x$ic_bootstrap)) " bootstrap" else "",
              if (!anyNA(x$ic_bootstrap)) .ic(x$ic_bootstrap[1], x$ic_bootstrap[2]) else .ic(x$ic[1], x$ic[2]),
              .f2(x$d), .f3(x$delta_r2), x$gl[1], x$gl[2], .f2(x$F), .fpe(x$p), nrow(x$datos)))
  for (k in c("bloques", "coeficientes", "medias", "estimaciones")) print(x[[k]])
  invisible(x)
}

#' Figuras del efecto del programa
#'
#' @param x El resultado de [qx_efecto()].
#' @param cual `"estimaciones"` (el efecto sin ajustar, con el pretest y con
#'   las covariables) o `"medias"` (medias pre-post por grupo).
#' @param referencia Opcional: un valor de referencia para marcar con una línea
#'   (por ejemplo, el efecto verdadero en datos simulados).
#' @param ... No se usa.
#' @return Un gráfico `ggplot`.
#' @export
plot.qx_efecto <- function(x, cual = c("estimaciones", "medias"), referencia = NULL, ...) {
  cual <- match.arg(cual)
  if (cual == "medias") return(.fig_medias(x$base))
  E <- data.frame(modelo = x$estimaciones$tabla$Modelo, B = x$estimaciones_num[, 1],
                  li = x$estimaciones_num[, 2], ls = x$estimaciones_num[, 3], y = 3:1)
  g <- ggplot2::ggplot(E, ggplot2::aes(B, y)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#969696")
  if (!is.null(referencia))
    g <- g + ggplot2::geom_vline(xintercept = referencia, linetype = "dashed", colour = "#BB1715") +
      ggplot2::annotate("text", x = referencia, y = 3.5, label = "Referencia", colour = "#BB1715", size = 3.8)
  g + ggplot2::geom_errorbar(ggplot2::aes(xmin = li, xmax = ls), width = .15, linewidth = .8, orientation = "y") +
    ggplot2::geom_point(size = 3.5, colour = "#BB1715") +
    ggplot2::geom_text(ggplot2::aes(label = .f2(B)), vjust = -1, size = 4) +
    ggplot2::scale_y_continuous(breaks = E$y, labels = E$modelo, limits = c(.5, 3.7)) +
    ggplot2::labs(x = "Diferencia en el postest (experimental \u2212 control) con IC 95 %", y = NULL) +
    ggplot2::theme_minimal(base_size = 14) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}
