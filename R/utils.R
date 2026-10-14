# Funciones internas comunes (no exportadas)

.f2 <- function(x) formatC(x, format = "f", digits = 2)
.f3 <- function(x) sub("^(-?)0[.]", "\\1.", formatC(x, format = "f", digits = 3))
.fp <- function(p) ifelse(p < .001, "< .001", sub("^0", "", formatC(p, format = "f", digits = 3)))
.fpe <- function(p) ifelse(p < .001, "< .001", paste("=", .fp(p)))
.ic <- function(a, b) sprintf("[%s, %s]", .f2(a), .f2(b))
.md <- function(x) sprintf("%s (%s)", .f2(mean(x, na.rm = TRUE)), .f2(stats::sd(x, na.rm = TRUE)))
.np <- function(x) sprintf("%d (%.1f %%)", sum(x, na.rm = TRUE), 100 * mean(x, na.rm = TRUE))

.dme_cont <- function(x, g) (mean(x[g], na.rm = TRUE) - mean(x[!g], na.rm = TRUE)) /
  sqrt((stats::var(x[g], na.rm = TRUE) + stats::var(x[!g], na.rm = TRUE)) / 2)
.dme_bin <- function(x, g) {
  p1 <- mean(x[g], na.rm = TRUE); p0 <- mean(x[!g], na.rm = TRUE)
  (p1 - p0) / sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
}
.de_comb <- function(x, g) sqrt((stats::var(x[g], na.rm = TRUE) + stats::var(x[!g], na.rm = TRUE)) / 2)
.magnitud <- function(d) {
  d <- abs(d)
  if (is.na(d)) "no estimable" else if (d >= .8) "grande" else if (d >= .5) "mediano" else if (d >= .2) "peque\u00f1o" else "trivial"
}
.incluye0 <- function(li, ls) li * ls <= 0

# "a", "a y b", "a, b y c" (con "e" antes de i-/hi-)
.lista <- function(x, prefijo = FALSE) {
  if (length(x) == 1) return(x)
  conj <- if (grepl("^(i|hi)", tolower(x[length(x)]))) " e " else " y "
  paste0(paste(x[-length(x)], collapse = ", "), conj, x[length(x)])
}
.sin0 <- function(x) sub("^0", "", .f2(x))

# Una nota = una celda de la tabla (fila, columna) + su lectura
.nota <- function(fila, columna, texto) list(fila = fila, columna = columna, texto = texto)

# Resultado estándar: tabla + título + notas ("Cómo se lee")
.resultado <- function(tabla, titulo, notas = list(), clase = NULL, ...) {
  rownames(tabla) <- NULL
  structure(list(tabla = tabla, titulo = titulo, notas = notas, ...),
            class = c(clase, "qx_resultado"))
}

.imprimir_notas <- function(notas) {
  if (!length(notas)) return(invisible())
  cat("\nC\u00f3mo se lee:\n")
  for (k in seq_along(notas)) {
    txt <- strwrap(notas[[k]]$texto, width = 76, exdent = 5)
    txt[1] <- sprintf("  %d. %s", k, txt[1])
    if (length(txt) > 1) txt[-1] <- paste0(" ", txt[-1])
    cat(txt, sep = "\n")
  }
}

#' Imprimir un resultado de QuasiEx
#'
#' Muestra el título, la tabla y la sección "Cómo se lee" con la
#' interpretación de los valores clave.
#'
#' @param x Un resultado de cualquier función `qx_`.
#' @param ... No se usa.
#' @return `x`, de forma invisible.
#' @export
print.qx_resultado <- function(x, ...) {
  cat("\n", x$titulo, "\n", strrep("-", nchar(x$titulo)), "\n", sep = "")
  print(x$tabla, row.names = FALSE, right = FALSE)
  .imprimir_notas(x$notas)
  # resultados anexos (por ejemplo, las amenazas de qx_sensibilidad)
  for (k in setdiff(names(x), c("tabla", "titulo", "notas")))
    if (inherits(x[[k]], "qx_resultado")) print(x[[k]])
  invisible(x)
}

# Ejecuta `expr` con una semilla propia y deja intacto el generador del usuario
.con_semilla <- function(semilla, expr) {
  if (is.null(semilla)) return(expr)
  hay <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (hay) previa <- get(".Random.seed", envir = globalenv())
  on.exit(if (hay) assign(".Random.seed", previa, envir = globalenv())
          else rm(".Random.seed", envir = globalenv()))
  set.seed(semilla)
  expr
}

# Sí/No escrito de varias formas → lógico
.a_logico <- function(x) {
  if (is.logical(x)) return(x)
  v <- tolower(trimws(as.character(x)))
  v <- chartr("\u00ed", "i", v)
  ifelse(is.na(v) | v == "", NA, v %in% c("si", "s", "yes", "y", "1", "true", "verdadero"))
}
.es_si_no <- function(x) {
  if (!is.character(x)) return(FALSE)
  v <- unique(chartr("\u00ed", "i", tolower(trimws(x[!is.na(x) & x != ""]))))
  length(v) > 0 && all(v %in% c("si", "s", "no", "n"))
}

# Base del efecto: quienes tienen pretest y postest, covariables lógicas a 0/1
.datos_modelo <- function(base, covariables) {
  a <- as.data.frame(base)
  a <- a[!is.na(a$pre) & !is.na(a$post), , drop = FALSE]
  for (v in covariables) {
    if (!v %in% names(a)) stop("La covariable \"", v, "\" no est\u00e1 en la base.", call. = FALSE)
    if (is.logical(a[[v]])) a[[v]] <- as.numeric(a[[v]])
  }
  a[stats::complete.cases(a[, c("pre", "post", covariables), drop = FALSE]), , drop = FALSE]
}

.comprobar_base <- function(base) {
  if (!inherits(base, "qx_base"))
    stop("Primero prepara la base con qx_preparar().", call. = FALSE)
}
.comprobar_efecto <- function(m) {
  if (!inherits(m, "qx_efecto"))
    stop("Usa el resultado de qx_efecto().", call. = FALSE)
}

utils::globalVariables(c("x", "y", "xend", "yend", "etiqueta", "grupo", "momento", "valor",
                         "m", "ic", "id", "B", "li", "ls", "fila", "texto", "relleno", "lab", "h"))
