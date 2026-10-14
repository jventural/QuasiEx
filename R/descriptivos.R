#' Equivalencia inicial de los grupos (paso 3)
#'
#' Compara a los grupos ANTES del programa con la diferencia de medias
#' estandarizada (DME), que no depende del tamaño de muestra. No usa valores p:
#' con grupos pequeños, un p "no significativo" no prueba que sean iguales.
#'
#' @param base La base que devuelve [qx_preparar()].
#' @param variables Variables a comparar. Pueden ser numéricas, lógicas
#'   (Sí/No) o categóricas (se muestra cada categoría). El pretest se llama
#'   `pre`. Si se deja vacío, se usan todas las variables de la persona.
#' @param corte Valor de |DME| a partir del cual se considera desequilibrio.
#'
#' @return Un resultado `qx_resultado` con la tabla (media y DE, o n y %, por
#'   grupo, y la DME) y su lectura.
#' @examples
#' base <- qx_preparar(qx_ejemplo(), items = "PHQ", criterios = c("pre >= 10"))
#' qx_equivalencia(base, c("edad", "sexo", "tiempo_dm", "pre"))
#' @export
qx_equivalencia <- function(base, variables = NULL, corte = .10) {
  .comprobar_base(base)
  d <- as.data.frame(base); g <- d$grupo == "Experimental"
  if (is.null(variables)) {
    fuera <- c("id", "grupo", "post", "cambio", grep("_(pre|post)$", names(d), value = TRUE))
    cand <- setdiff(names(d), fuera)
    variables <- cand[vapply(cand, function(v) {
      x <- d[[v]]
      if (all(is.na(x[!g])) || all(is.na(x[g]))) return(FALSE)          # solo un grupo la tiene
      u <- unique(x[!is.na(x)])
      if (length(u) < 2) return(FALSE)                                   # constante
      if (inherits(x, c("Date", "POSIXt")) || grepl("fecha|date", v, ignore.case = TRUE)) return(FALSE)
      if (is.character(x) || is.factor(x)) {
        if (length(u) > 6 || all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", u))) return(FALSE)  # texto libre o fechas
        if (all(table(x, g) %in% c(0, sum(g), sum(!g)))) return(FALSE)   # define al grupo (p. ej., turno)
      }
      TRUE
    }, logical(1))]
    variables <- c(setdiff(variables, "pre"), "pre")
  }
  nombre <- function(v) if (v == "pre") "Pretest" else v
  filas <- list()
  for (v in variables) {
    if (!v %in% names(d)) stop("La variable \"", v, "\" no est\u00e1 en la base.", call. = FALSE)
    x <- d[[v]]
    if (is.numeric(x)) {
      filas[[length(filas) + 1]] <- data.frame(Variable = nombre(v), Experimental = .md(x[g]), Control = .md(x[!g]),
                                               dme = .dme_cont(x, g))
    } else if (is.logical(x)) {
      filas[[length(filas) + 1]] <- data.frame(Variable = paste0(v, " (s\u00ed)"), Experimental = .np(x[g]),
                                               Control = .np(x[!g]), dme = .dme_bin(x, g))
    } else {
      niv <- sort(unique(as.character(x[!is.na(x)])))
      if (length(niv) == 2) niv <- niv[1]
      for (l in niv) {
        b <- as.character(x) == l
        filas[[length(filas) + 1]] <- data.frame(Variable = paste0(v, ": ", l), Experimental = .np(b[g]),
                                                 Control = .np(b[!g]), dme = .dme_bin(b, g))
      }
    }
  }
  t <- do.call(rbind, filas)
  tabla <- data.frame(t[, 1:3], DME = .f2(t$dme), check.names = FALSE)
  ipre <- which(t$Variable == "Pretest")
  otras <- setdiff(seq_len(nrow(t)), ipre)
  notas <- list()
  if (length(otras)) {
    imax <- otras[which.max(abs(t$dme[otras]))]
    notas[[1]] <- .nota(imax, "DME", sprintf(
      "DME = %s: es la mayor diferencia de partida (%s). Si la teor\u00eda la liga al desenlace, es candidata a CONFUSORA y entra como covariable.",
      .f2(t$dme[imax]), t$Variable[imax]))
  }
  if (length(ipre))
    notas[[length(notas) + 1]] <- .nota(ipre, "DME", sprintf(
      "DME = %s en el pretest: %s. Por eso el pretest SIEMPRE entra como covariable.", .f2(t$dme[ipre]),
      if (abs(t$dme[ipre]) <= corte) "los grupos partieron parecidos" else if (t$dme[ipre] > 0)
        "el experimental parti\u00f3 con puntajes M\u00c1S altos" else "el control parti\u00f3 con puntajes m\u00e1s altos"))
  des <- t$Variable[abs(t$dme) > corte]
  resto <- setdiff(otras, if (length(otras)) imax)
  notas[[length(notas) + 1]] <- .nota(if (length(resto)) resto[1] else 1, "DME", sprintf(
    "Regla: |DME| > %s = desequilibrio (aqu\u00ed: %s). No se usa el valor p: con grupos peque\u00f1os, un p \"no significativo\" no prueba que sean iguales.",
    .sin0(corte), if (length(des) > 5) paste(length(des), "variables") else if (length(des)) paste(des, collapse = ", ") else "ninguna variable"))
  .resultado(tabla, "Equivalencia inicial de los grupos (qx_equivalencia)", notas)
}

# omega y alfa sobre la MISMA matriz (policórica; Pearson si no es estimable)
.fiab <- function(x) {
  x <- x[stats::complete.cases(x), , drop = FALSE]
  rho <- tryCatch(suppressWarnings(psych::polychoric(x)$rho), error = function(e) NULL)
  matriz <- "polic\u00f3rica"
  if (is.null(rho) || any(!is.finite(rho)) || any(eigen(rho, only.values = TRUE)$values <= 0)) {
    rho <- stats::cor(x); matriz <- "Pearson"
  }
  k <- ncol(rho); rbar <- mean(rho[lower.tri(rho)])
  lam <- as.numeric(suppressWarnings(psych::fa(rho, nfactors = 1, n.obs = nrow(x), fm = "minres"))$loadings)
  c(n = nrow(x), omega = sum(lam)^2 / (sum(lam)^2 + sum(1 - lam^2)), alfa = k * rbar / (1 + (k - 1) * rbar),
    matriz = matriz)
}

#' Fiabilidad del instrumento en la muestra (paso 5)
#'
#' Calcula omega y alfa sobre la misma matriz de correlaciones policóricas
#' (o de Pearson si la policórica no se puede estimar, lo que se indica). En el
#' pretest usa a todos los evaluados que lo completaron (más casos, estimación
#' más estable); en el postest, a los asignados que lo respondieron.
#'
#' @param base La base que devuelve [qx_preparar()].
#' @return Un resultado `qx_resultado` con n, matriz usada, omega y alfa por
#'   momento, y su lectura.
#' @examples
#' base <- qx_preparar(qx_ejemplo(), items = "PHQ", criterios = c("pre >= 10"))
#' qx_fiabilidad(base)
#' @export
qx_fiabilidad <- function(base) {
  .comprobar_base(base)
  it <- attr(base, "items")
  f1 <- .fiab(attr(base, "evaluados"))
  f2 <- .fiab(as.data.frame(base)[!is.na(base$post), paste0(it, "_post"), drop = FALSE])
  w <- as.numeric(c(f1["omega"], f2["omega"]))
  tabla <- data.frame(Momento = c("Pretest (todos los evaluados)", "Postest"),
                      n = as.integer(c(f1["n"], f2["n"])), Matriz = c(f1["matriz"], f2["matriz"]),
                      "\u03c9" = .f3(w), "\u03b1" = .f3(as.numeric(c(f1["alfa"], f2["alfa"]))), check.names = FALSE)
  lee <- function(x) if (x >= .8) "puntajes fiables para comparar grupos." else if (x >= .7)
    "fiabilidad aceptable; interpretar con algo de cautela." else "fiabilidad baja: interpretar con cautela."
  notas <- list(
    .nota(1, "\u03c9", sprintf("\u03c9 = %s en el pretest: %s", .f3(w[1]), lee(w[1]))),
    .nota(2, "\u03c9", sprintf("\u03c9 = %s en el postest: %s%s", .f3(w[2]), lee(w[2]),
      if (w[2] < w[1] - .05) sprintf(" Es m\u00e1s bajo porque hay solo %s casos y menos variabilidad.", f2["n"]) else "")))
  if ("Pearson" %in% tabla$Matriz) notas[[3]] <- .nota(which(tabla$Matriz == "Pearson")[1], "Matriz",
    "Matriz Pearson: con pocos casos la polic\u00f3rica no fue estimable; se usa Pearson y se declara.")
  .resultado(tabla, "Fiabilidad en la muestra (qx_fiabilidad)", notas)
}

# d_av con IC por bootstrap
.d_av <- function(pre, post, B = 2000) {
  est <- function(i) mean(post[i] - pre[i]) / ((stats::sd(pre[i]) + stats::sd(post[i])) / 2)
  bs <- replicate(B, est(sample(seq_along(pre), replace = TRUE)))
  c(est(seq_along(pre)), stats::quantile(bs, c(.025, .975), na.rm = TRUE))
}

#' Cambio dentro de cada grupo (paso 6)
#'
#' Describe cuánto cambió cada grupo del pretest al postest: medias, cambio
#' medio con IC 95 % y d_av (cambio dividido entre el promedio de las DE de
#' ambos momentos, con IC por bootstrap). Es descriptivo: el cambio del grupo
#' control NO es el efecto del programa.
#'
#' @param base La base que devuelve [qx_preparar()].
#' @param semilla Semilla del bootstrap de d_av (para que el resultado se
#'   repita). No altera el generador aleatorio de la sesión.
#' @return Un resultado `qx_resultado` con la tabla por grupo y su lectura. Con
#'   `plot()` se obtiene la figura de medias pre-post con trayectorias.
#' @examples
#' base <- qx_preparar(qx_ejemplo(), items = "PHQ", criterios = c("pre >= 10"))
#' qx_cambio(base)
#' plot(qx_cambio(base))
#' @export
qx_cambio <- function(base, semilla = 2027) {
  .comprobar_base(base)
  d <- as.data.frame(base)
  res <- .con_semilla(semilla, lapply(c("Experimental", "Control"), function(gr) {
    s <- d[d$grupo == gr & !is.na(d$pre) & !is.na(d$post), ]
    tt <- stats::t.test(s$post, s$pre, paired = TRUE); dv <- .d_av(s$pre, s$post)
    data.frame(Grupo = gr, n = nrow(s), "Pretest M (DE)" = .md(s$pre), "Postest M (DE)" = .md(s$post),
               Cambio = .f2(mean(s$cambio)), "IC 95 %" = .ic(tt$conf.int[1], tt$conf.int[2]),
               d_av = .f2(dv[1]), ic_li = tt$conf.int[1], ic_ls = tt$conf.int[2], dav = dv[1], check.names = FALSE)
  }))
  t <- do.call(rbind, res)
  tabla <- t[, 1:7]
  ic <- which(t$Grupo == "Control"); ie <- which(t$Grupo == "Experimental")
  notas <- list(
    .nota(ic, "Cambio", sprintf(
      "El control cambi\u00f3 %s puntos SIN programa (el IC %s el cero): regresi\u00f3n a la media y paso del tiempo. Por eso el cambio de un grupo NO es el efecto.",
      .f2(as.numeric(t$Cambio[ic])), if (.incluye0(t$ic_li[ic], t$ic_ls[ic])) "incluye" else "no incluye")),
    .nota(ie, "d_av", sprintf(
      "d_av = %s: cambio %s en el experimental, pero mezcla programa + regresi\u00f3n a la media + tiempo. El efecto se estima comparando grupos (qx_efecto).",
      .f2(t$dav[ie]), .magnitud(t$dav[ie]))))
  .resultado(tabla, "Cambio dentro de cada grupo (qx_cambio)", notas, clase = "qx_cambio", base = base)
}

# Datos y medias para la figura pre-post
.largo <- function(base) {
  d <- as.data.frame(base)
  l <- rbind(data.frame(id = d$id, grupo = d$grupo, momento = "Pretest", valor = d$pre),
             data.frame(id = d$id, grupo = d$grupo, momento = "Postest", valor = d$post))
  l$momento <- factor(l$momento, levels = c("Pretest", "Postest"))
  l
}

#' Figura de medias pre-post por grupo
#'
#' @param x El resultado de [qx_cambio()].
#' @param ... No se usa.
#' @return Un gráfico `ggplot`: medias con IC 95 % por grupo y momento, y las
#'   trayectorias individuales.
#' @export
plot.qx_cambio <- function(x, ...) .fig_medias(x$base)

.fig_medias <- function(base) {
  l <- .largo(base); l <- l[!is.na(l$valor), ]
  m <- stats::aggregate(valor ~ grupo + momento, l, function(v)
    c(m = mean(v), ic = stats::qt(.975, length(v) - 1) * stats::sd(v) / sqrt(length(v))))
  m <- data.frame(m[, 1:2], m = m$valor[, "m"], ic = m$valor[, "ic"])
  ggplot2::ggplot(l, ggplot2::aes(as.numeric(momento), valor, colour = grupo)) +
    ggplot2::geom_line(ggplot2::aes(group = id), alpha = .25) +
    ggplot2::geom_line(data = m, ggplot2::aes(y = m, group = grupo), linewidth = 1.3) +
    ggplot2::geom_pointrange(data = m, ggplot2::aes(y = m, ymin = m - ic, ymax = m + ic), size = .6) +
    ggplot2::scale_colour_manual(NULL, values = c(Experimental = "#BB1715", Control = "#595959")) +
    ggplot2::scale_x_continuous(breaks = 1:2, labels = c("Pretest", "Postest"), expand = ggplot2::expansion(add = .3)) +
    ggplot2::labs(x = NULL, y = "Puntaje total") +
    ggplot2::theme_minimal(base_size = 14) + ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank())
}
