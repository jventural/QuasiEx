#' QuasiEx: análisis sencillo de estudios cuasiexperimentales
#'
#' Pocas funciones, una por paso del análisis de un cuasiexperimento con
#' pretest, postest y grupo control no equivalente. Cada una imprime su tabla
#' y una sección "Cómo se lee" con la interpretación de los valores clave.
#'
#' \itemize{
#'   \item [qx_preparar()]: limpieza, criterios, flujo y puntajes (paso 2).
#'   \item [qx_muestra()]: qué efecto puede detectar el estudio (paso 0).
#'   \item [qx_equivalencia()]: ¿los grupos partían iguales? (paso 3).
#'   \item [qx_fiabilidad()]: omega y alfa en la muestra (paso 5).
#'   \item [qx_cambio()]: cambio dentro de cada grupo (paso 6).
#'   \item [qx_efecto()]: efecto del programa con covariables (paso 7).
#'   \item [qx_supuestos()]: supuestos del modelo (paso 8).
#'   \item [qx_clinico()]: cambio clínicamente significativo (paso 9).
#'   \item [qx_sensibilidad()]: ¿cambia la conclusión? (paso 10).
#'   \item [qx_ejemplo()]: ruta de la base simulada de ejemplo.
#' }
#' @importFrom utils globalVariables
#' @keywords internal
"_PACKAGE"
