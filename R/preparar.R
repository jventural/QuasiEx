#' Preparar la base: limpieza, criterios y puntajes (paso 2)
#'
#' Lee la base tal como llega del campo, en formato largo (una fila por
#' persona y momento), y la deja lista para analizar: unifica cómo se
#' escribieron el grupo, el sexo y las respuestas Sí/No; deja vacías las
#' respuestas fuera de rango; pasa a formato ancho (una fila por persona);
#' calcula el puntaje total del pretest y del postest; y aplica los criterios
#' de elegibilidad en orden, guardando el motivo de cada exclusión para el
#' diagrama de flujo.
#'
#' @param datos Ruta de un archivo Excel (se lee la primera hoja) o un
#'   `data.frame`.
#' @param items Prefijo de los ítems del instrumento. Con `"PHQ"` se usan las
#'   columnas `PHQ1`, `PHQ2`, ...
#' @param criterios Condiciones que debe cumplir una persona para QUEDARSE en
#'   el estudio, escritas como en R, por ejemplo
#'   `c("Edad de 30 a 59" = "edad >= 30 & edad <= 59", "pre >= 10")`. El
#'   puntaje total del pretest se llama `pre`. Los nombres (opcionales) se usan
#'   en el diagrama de flujo. Antes se excluye siempre a quien tiene el pretest
#'   incompleto.
#' @param rango Valores posibles de cada ítem. Lo que cae fuera queda vacío.
#' @param item_alerta Ítem que activa un protocolo de derivación si es mayor
#'   que el mínimo (por ejemplo `"PHQ9"`). Solo avisa, no excluye.
#' @param id,grupo,momento Nombres de las columnas con el código de la
#'   persona, el grupo y el momento (pretest o postest).
#'
#' @details El grupo se reconoce por cómo empieza: "exp..." es experimental y
#'   "con..." es control. El momento: "pre..." y "pos...". El puntaje se calcula
#'   si la persona respondió al menos el 80 % de los ítems (media de los ítems
#'   respondidos multiplicada por el número de ítems); si no, queda vacío. Quien
#'   no tiene postest NO se excluye: es una pérdida.
#'
#' @return Un `data.frame` de clase `qx_base` con una fila por participante
#'   asignado y las columnas `id`, `grupo` (Control, Experimental), `pre`,
#'   `post`, `cambio`, los ítems de cada momento y el resto de variables. Lleva
#'   como atributos el flujo de participantes (`flujo`), los pretest de todos
#'   los evaluados (`evaluados`, para la fiabilidad) y los avisos de la limpieza.
#'
#' @examples
#' base <- qx_preparar(qx_ejemplo(), items = "PHQ",
#'                     criterios = c("Consentimiento" = "consentimiento",
#'                                   "Edad de 30 a 59" = "edad >= 30 & edad <= 59",
#'                                   "Sin psicoterapia" = "!psicoterapia",
#'                                   "PHQ-9 de 10 o más" = "pre >= 10"))
#' base
#' plot(base)
#' @export
qx_preparar <- function(datos, items, criterios = NULL, rango = c(0, 3), item_alerta = NULL,
                        id = "codigo", grupo = "grupo", momento = "momento") {
  if (is.character(datos) && length(datos) == 1) {
    if (!file.exists(datos)) stop("No encuentro el archivo: ", datos, call. = FALSE)
    datos <- readxl::read_excel(datos, sheet = 1)
  }
  d <- as.data.frame(datos, stringsAsFactors = FALSE)
  faltan <- setdiff(c(id, grupo, momento), names(d))
  if (length(faltan)) stop("La base no tiene la(s) columna(s): ", paste(faltan, collapse = ", "),
                           ". Ind\u00edcalas con los argumentos id, grupo o momento.", call. = FALSE)
  cols <- grep(paste0("^", items, "[0-9]+$"), names(d), value = TRUE)
  if (!length(cols)) stop("No hay columnas que empiecen con \"", items, "\" seguidas de un n\u00famero.", call. = FALSE)
  cols <- cols[order(as.numeric(sub(items, "", cols, fixed = TRUE)))]
  k <- length(cols); minimo <- ceiling(.8 * k)
  avisos <- list()

  # 1. Unificar cómo se escribió cada cosa
  g <- tolower(trimws(as.character(d[[grupo]])))
  d$.grupo <- ifelse(grepl("^exp", g), "Experimental", ifelse(grepl("^con", g), "Control", NA))
  if ("sexo" %in% names(d)) {
    s <- tolower(trimws(as.character(d$sexo)))
    d$sexo <- ifelse(grepl("^(fem|muj|f$)", s), "Mujer", ifelse(grepl("^(mas|hom|var|h$)", s), "Hombre", NA))
    if (any(s == "m", na.rm = TRUE)) avisos$sexo <- "\"M\" en sexo es ambiguo (\u00bfmujer o masculino?): qued\u00f3 vac\u00edo."
  }
  for (v in setdiff(names(d), c(cols, id, grupo, momento))) if (.es_si_no(d[[v]])) d[[v]] <- .a_logico(d[[v]])

  # 2. Ítems: a número y fuera de rango → vacío
  fuera <- 0
  for (v in cols) {
    x <- d[[v]]
    if (!is.numeric(x)) x <- suppressWarnings(as.numeric(sub("^\\s*(-?[0-9]+([.][0-9]+)?).*$", "\\1", as.character(x))))
    mal <- !is.na(x) & (x < rango[1] | x > rango[2] | x != round(x))
    fuera <- fuera + sum(mal); x[mal] <- NA; d[[v]] <- x
  }
  avisos$fuera_rango <- fuera

  # 3. De largo a ancho
  mom <- tolower(trimws(as.character(d[[momento]])))
  pre <- d[grepl("^pre", mom), , drop = FALSE]
  post <- d[grepl("^pos", mom), , drop = FALSE]
  if (anyDuplicated(pre[[id]])) stop("Hay c\u00f3digos repetidos en el pretest.", call. = FALSE)
  otras <- setdiff(names(d), c(cols, id, momento))
  de_post <- otras[vapply(otras, function(v) all(is.na(pre[[v]])) && any(!is.na(post[[v]])), logical(1))]
  pre <- pre[, setdiff(names(pre), c(momento, de_post)), drop = FALSE]
  post <- post[, c(id, cols, de_post), drop = FALSE]
  names(pre)[match(cols, names(pre))] <- paste0(cols, "_pre")
  names(post)[match(cols, names(post))] <- paste0(cols, "_post")
  sin_pre <- setdiff(post[[id]], pre[[id]])
  if (length(sin_pre)) avisos$sin_pretest <- sin_pre
  w <- merge(pre, post, by = id, all.x = TRUE, sort = FALSE)
  names(w)[names(w) == id] <- "id"

  # 4. Puntajes
  puntaje <- function(m) { ok <- rowSums(!is.na(m)); ifelse(ok >= minimo, rowMeans(m, na.rm = TRUE) * k, NA) }
  ip <- paste0(cols, "_pre"); iq <- paste0(cols, "_post")
  w$pre <- puntaje(w[, ip, drop = FALSE])
  w$post <- puntaje(w[, iq, drop = FALSE])
  w$cambio <- w$post - w$pre

  # 5. Criterios en orden; cada exclusión con su motivo
  # "Pretest incompleto" va justo antes del primer criterio que usa el puntaje `pre`
  crit <- c("Sin grupo asignado" = "!is.na(.grupo)")
  inc <- c("Pretest incompleto" = "!is.na(pre)")
  if (length(criterios)) {
    nom <- names(criterios); if (is.null(nom)) nom <- rep("", length(criterios))
    nom[nom == ""] <- criterios[nom == ""]
    nom <- paste("No cumple:", nom)
    usa_pre <- grepl("\\bpre\\b", criterios)
    i <- if (any(usa_pre)) which(usa_pre)[1] else length(criterios) + 1
    antes <- seq_len(i - 1); despues <- setdiff(seq_along(criterios), antes)
    crit <- c(crit, stats::setNames(criterios[antes], nom[antes]), inc,
              stats::setNames(criterios[despues], nom[despues]))
  } else crit <- c(crit, inc)
  w$motivo <- NA_character_
  for (j in seq_along(crit)) {
    queda <- tryCatch(eval(parse(text = crit[[j]]), w),
                      error = function(e) stop("No pude evaluar el criterio \"", crit[[j]], "\": ",
                                               conditionMessage(e), call. = FALSE))
    queda <- rep_len(as.logical(queda), nrow(w)); queda[is.na(queda)] <- FALSE
    w$motivo[is.na(w$motivo) & !queda] <- names(crit)[j]
  }
  evaluados <- w[, ip, drop = FALSE]
  asig <- w[is.na(w$motivo), , drop = FALSE]
  asig$grupo <- factor(asig$.grupo, levels = c("Control", "Experimental"))
  asig$.grupo <- NULL; asig$motivo <- NULL
  if (grupo != "grupo") asig[[grupo]] <- NULL
  primero <- c("id", "grupo", "pre", "post", "cambio")
  asig <- asig[, c(primero, setdiff(names(asig), c(primero, ip, iq)), ip, iq)]

  exc <- table(factor(w$motivo, levels = names(crit)))
  exc <- exc[exc > 0 | !names(exc) %in% c("Pretest incompleto", "Sin grupo asignado")]
  ge <- asig$grupo == "Experimental"; tp <- !is.na(asig$post)
  flujo <- data.frame(Etapa = c("Evaluados", names(exc), "Asignados", "Grupo experimental", "Grupo control",
                                "Con postest (experimental)", "Con postest (control)"),
                      n = c(nrow(w), as.integer(exc), nrow(asig), sum(ge), sum(!ge), sum(ge & tp), sum(!ge & tp)),
                      check.names = FALSE)
  avisos$items_vacios_post <- sum(is.na(asig[tp, iq]))
  if (!is.null(item_alerta)) {
    v <- c(w[[paste0(item_alerta, "_pre")]], w[[paste0(item_alerta, "_post")]])
    avisos$alerta <- c(item = item_alerta, n = sum(v > rango[1], na.rm = TRUE))
  }
  rownames(asig) <- NULL
  structure(asig, class = c("qx_base", "data.frame"), flujo = flujo, evaluados = evaluados,
            items = cols, rango = rango, avisos = avisos)
}

#' @export
print.qx_base <- function(x, ...) {
  f <- attr(x, "flujo"); a <- attr(x, "avisos")
  n <- stats::setNames(f$n, f$Etapa)
  titulo <- "Base preparada (qx_preparar)"
  cat("\n", titulo, "\n", strrep("-", nchar(titulo)), "\n", sep = "")
  print(f, row.names = FALSE, right = FALSE)
  notas <- list(.nota(nrow(f) - 1, "n", sprintf(
    "De %d evaluados quedaron %d asignados (%d experimental y %d control); %d y %d tienen postest. Quien no tiene postest es una P\u00c9RDIDA, no una exclusi\u00f3n: sigue en la base.",
    n["Evaluados"], n["Asignados"], n["Grupo experimental"], n["Grupo control"],
    n["Con postest (experimental)"], n["Con postest (control)"])))
  if (a$fuera_rango > 0 || a$items_vacios_post > 0)
    notas <- c(notas, list(.nota(1, "n", sprintf(
      "Limpieza: %d respuesta(s) fuera de rango quedaron vac\u00edas y hay %d \u00edtem(s) vac\u00edo(s) en el postest; el puntaje se calcula si se respondi\u00f3 al menos el 80 %% de los \u00edtems.",
      a$fuera_rango, a$items_vacios_post))))
  if (!is.null(a$alerta) && as.numeric(a$alerta["n"]) > 0)
    notas <- c(notas, list(.nota(1, "n", sprintf(
      "Alerta: %s respuesta(s) con el \u00edtem %s mayor que el m\u00ednimo. Verifica que se aplic\u00f3 el protocolo de derivaci\u00f3n.",
      a$alerta["n"], a$alerta["item"]))))
  if (!is.null(a$sexo)) notas <- c(notas, list(.nota(1, "n", a$sexo)))
  if (!is.null(a$sin_pretest)) notas <- c(notas, list(.nota(1, "n", paste(
    "C\u00f3digos con postest pero sin pretest (\u00bfse escribieron distinto?):", paste(a$sin_pretest, collapse = ", ")))))
  .imprimir_notas(notas)
  cat(sprintf("\nLa base tiene %d filas y %d columnas. Para verla: as.data.frame(base)\n", nrow(x), ncol(x)))
  invisible(x)
}

#' Diagrama de flujo de participantes
#'
#' @param x La base que devuelve [qx_preparar()].
#' @param ... No se usa.
#' @return Un gráfico `ggplot` con el flujo (evaluados, excluidos por motivo,
#'   asignados por grupo y con postest).
#' @export
plot.qx_base <- function(x, ...) {
  f <- attr(x, "flujo"); n <- stats::setNames(f$n, f$Etapa)
  excl <- f[!f$Etapa %in% c("Evaluados", "Asignados", "Grupo experimental", "Grupo control",
                            "Con postest (experimental)", "Con postest (control)"), ]
  txt_excl <- paste0("Excluidos (n = ", sum(excl$n), ")\n",
                     paste(sprintf("%s: %d", sub("^No cumple: ", "", excl$Etapa), excl$n), collapse = "\n"))
  cajas <- data.frame(
    x = c(3, 4.75, 3, 1.4, 4.6, 1.4, 4.6), y = c(6, 5.2, 4.4, 2.9, 2.9, 1.3, 1.3),
    h = c(.5, 0, .5, .5, .5, .5, .5),   # los excluidos se anclan a la izquierda: la flecha llega a su borde
    etiqueta = c(sprintf("Evaluados\nn = %d", n["Evaluados"]), txt_excl,
                 sprintf("Asignados\nn = %d", n["Asignados"]),
                 sprintf("Grupo experimental\nn = %d", n["Grupo experimental"]),
                 sprintf("Grupo control\nn = %d", n["Grupo control"]),
                 sprintf("Con postest\nn = %d (p\u00e9rdidas: %d)", n["Con postest (experimental)"],
                         n["Grupo experimental"] - n["Con postest (experimental)"]),
                 sprintf("Con postest\nn = %d (p\u00e9rdidas: %d)", n["Con postest (control)"],
                         n["Grupo control"] - n["Con postest (control)"])),
    relleno = c("white", "#F2F2F2", "white", "#F8E1E1", "#EDEDED", "#F8E1E1", "#EDEDED"))
  flechas <- data.frame(x = c(3, 3, 3, 3, 1.4, 4.6), y = c(5.65, 5.2, 4.0, 4.0, 2.5, 2.5),
                        xend = c(3, 4.7, 1.4, 4.6, 1.4, 4.6), yend = c(4.8, 5.2, 3.3, 3.3, 1.7, 1.7))
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = flechas, ggplot2::aes(x, y, xend = xend, yend = yend),
                          arrow = ggplot2::arrow(length = ggplot2::unit(.2, "cm"), type = "closed"), colour = "#595959") +
    ggplot2::geom_label(data = cajas, ggplot2::aes(x, y, label = etiqueta, hjust = h), fill = cajas$relleno, size = 4.2,
                        lineheight = .95, label.padding = ggplot2::unit(.45, "lines")) +
    ggplot2::coord_cartesian(xlim = c(0.2, 8.2), ylim = c(.7, 6.5)) + ggplot2::theme_void()
}
