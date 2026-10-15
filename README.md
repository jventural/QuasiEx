# QuasiEx

Análisis sencillo de estudios **cuasiexperimentales** con pretest, postest y
grupo control no equivalente. Pocas funciones, una por paso del análisis; cada
una imprime su tabla y una sección **"Cómo se lee"** con la interpretación de
los valores clave.

## Instalación

```r
install.packages("remotes")
remotes::install_github("jventural/QuasiEx")
```

## El análisis completo en pocas líneas

```r
library(QuasiEx)

# Paso 2 · Limpiar, aplicar criterios y calcular puntajes
base <- qx_preparar(qx_ejemplo(), items = "PHQ",
                    criterios = c("Consentimiento"    = "consentimiento",
                                  "Edad de 30 a 59"   = "edad >= 30 & edad <= 59",
                                  "Sin psicoterapia"  = "!psicoterapia",
                                  "PHQ-9 de 10 o más" = "pre >= 10"))
base          # flujo de participantes
plot(base)    # diagrama de flujo

qx_muestra(n1 = 12, n2 = 12)                                  # Paso 0 · qué efecto puede detectar
qx_equivalencia(base, c("edad", "sexo", "tiempo_dm", "pre"))  # Paso 3 · ¿partían iguales?
qx_fiabilidad(base)                                           # Paso 5 · omega y alfa
qx_cambio(base)                                               # Paso 6 · cambio en cada grupo

base$insulina <- base$tratamiento_dm != "Oral"                # una covariable nueva
m <- qx_efecto(base, covariables = c("tiempo_dm", "insulina"))  # Paso 7 · efecto del programa
m
plot(m)                                                       # el efecto con distintos ajustes

qx_supuestos(m)                                               # Paso 8 · supuestos
qx_clinico(m, fiabilidad = .892, corte = 10)                  # Paso 9 · cambio clínico
qx_sensibilidad(m)                                            # Paso 10 · ¿cambia la conclusión?
```

## Tu base

`qx_preparar()` espera la base como llega del campo, en **formato largo** (una
fila por persona y momento), con:

| Columna | Contenido |
|---|---|
| `codigo` | código de la persona (igual en pretest y postest) |
| `grupo` | "Experimental" o "Control" (basta con que empiece así) |
| `momento` | "Pretest" o "Postest" |
| `PHQ1`, `PHQ2`, … | los ítems (el prefijo se indica en `items`) |
| otras | edad, sexo, consentimiento, asistencia… (las que tengas) |

Si tus columnas se llaman distinto, indícalo con `id`, `grupo` y `momento`.

## Funciones

| Función | Paso | Qué hace |
|---|---|---|
| `qx_preparar()` | 2 | limpieza, criterios en orden, flujo y puntajes |
| `qx_muestra()` | 0 | efecto mínimo detectable con tu n |
| `qx_equivalencia()` | 3 | diferencia de medias estandarizada (sin valores p) |
| `qx_fiabilidad()` | 5 | omega y alfa sobre la misma matriz |
| `qx_cambio()` | 6 | cambio dentro de cada grupo, d_av |
| `qx_efecto()` | 7 | regresión jerárquica (ANCOVA), IC bootstrap, medias ajustadas, d |
| `qx_supuestos()` | 8 | paralelismo, normalidad, homocedasticidad, FIV, Cook |
| `qx_clinico()` | 9 | cambio fiable (RCI) y recuperación |
| `qx_sensibilidad()` | 10 | HC3, modelo mixto, por protocolo, Yuen y amenazas |
| `qx_ejemplo()` | — | ruta de la base simulada incluida |

La base de ejemplo es **simulada** (ficticia): no corresponde a ningún paciente.

## Limpieza de datos (LimpiaR)

Al cargar esta librería también tienes las funciones de limpieza de
[LimpiaR](https://github.com/jventural/LimpiaR), las mismas en los tres diseños del curso:
`leer()`, `revisar()`, `renombrar()`, `numero()`, `recodificar()`, `unificar()`,
`validar()`, `calidad()`, `invertir()`, `puntaje()`, `excluir()`, `flujo()` y `guardar()`.
Cada una hace un paso en una línea y muestra en la consola qué hizo.

## Autor

José Ventura-León
