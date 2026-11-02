# scicomm — diseño

**Scientific communication quality and audience assessment**
Estado: diseño, sin código. Fecha: 2026-11-02.

## 1. Qué es y qué no es

Un paquete para evaluar **si una comunicación científica es fiel a la evidencia y adecuada a su público**, combinando tres tipos de prueba, siempre identificados por separado:

| Fuente | Qué puede demostrar |
|---|---|
| Indicadores automáticos | Señales y alertas: legibilidad, lenguaje causal, atenuadores, cifras cambiadas |
| Valoración humana con rúbrica | Juicio experto sobre rigor, narrativa y calidad visual |
| Estudios con lectores | Si la gente **comprende** el mensaje; es la única prueba de comprensión |

No es un medidor de legibilidad más, no puntúa textos con modelos de lenguaje en la versión 0.1 (para que los resultados sean reproducibles) y **no combina las dimensiones en una puntuación única**, igual que OpenResearchMetrics.

Nombre: `scicomm` está libre en CRAN. Prefijo: `sc_`.

## 2. Aportación diferencial: fidelidad entre públicos

El núcleo es comparar cómo se transforma **un mismo resultado** al comunicarlo a especialistas, estudiantes y público general:

1. El resultado de partida se describe de forma estructurada con `sc_finding()`.
2. Cada versión del texto se registra con su público con `sc_version()`.
3. `sc_compare()` produce una matriz con un elemento del resultado por fila (dirección, magnitud, certeza, población, diseño, limitaciones) y una versión por columna, y para cada celda dice si el elemento se **conserva**, **cambia**, **falta** o se **añade** algo que no estaba en la evidencia (exageración).

Así se ve si simplificar el lenguaje ha alterado el significado científico, que es la pregunta que planteas.

## 3. El resultado de partida: `sc_finding()`

| Campo | Ejemplo |
|---|---|
| `claim` | "El programa redujo el abandono escolar" |
| `direction` | `decrease` |
| `effect` | Tamaño con intervalo, en texto o numérico |
| `certainty` | `high`, `moderate`, `low` o `very_low` (escala tipo GRADE) |
| `design` | `rct`, `observational`, `animal`, `in_vitro`, `model`, `qualitative`, `review` |
| `population` | "Estudiantes de secundaria en España" |
| `causal` | `TRUE` o `FALSE` (si el diseño permite una conclusión causal) |
| `limitations` | Vector de limitaciones clave |

## 4. Las seis dimensiones

| Dimensión | Indicadores automáticos | Humano o lectores |
|---|---|---|
| **Claridad** | Longitud de frase; legibilidad por idioma (inglés: Flesch y Flesch-Kincaid; español: Fernández-Huerta y Szigriszt-Pazos/INFLESZ); densidad de jerga; siglas sin definir; voz pasiva aproximada | Rúbrica de claridad; comprensión de la idea principal |
| **Rigor (fidelidad)** | Lenguaje causal con diseño observacional; extrapolación de animales a humanos; palabras de magnitud frente al tamaño del efecto; cifras añadidas, cambiadas o perdidas respecto al resultado | Rúbrica de experto |
| **Accesibilidad** | Legibilidad frente a la banda objetivo de cada público (umbrales configurables); jerga definida o no; riesgo relativo sin riesgo absoluto; porcentajes frente a frecuencias naturales | Rúbrica; comprensión por público |
| **Incertidumbre** | Atenuadores y reforzadores (léxicos tipo Hyland); limitaciones de `sc_finding()` presentes o ausentes; certeza baja expresada con lenguaje rotundo; intervalos o rangos mencionados | Calibración: confianza del lector frente a acierto |
| **Visualización** | Para gráficos ggplot2: barras sin origen en cero, incertidumbre ausente cuando el resultado tiene intervalo, ejes y unidades sin etiqueta, paleta poco distinguible con daltonismo, contraste insuficiente, sin texto alternativo | Lista de comprobación para imágenes |
| **Narrativa** | Posición de la idea principal (al principio o enterrada); presencia de contexto, importancia y siguiente paso, a partir de encabezados o expresiones clave | Rúbrica de estructura y coherencia |

Cada indicador devuelve el valor, el umbral usado, la fuente (`auto`, `human` o `reader`) y una advertencia sobre lo que no mide.

El esquema de lenguaje causal se basará en los usados para estudiar la exageración en notas de prensa, como el de Sumner et al. (2014, *BMJ*). Los léxicos serán ficheros CSV versionados, en inglés y en español, de elaboración propia y con licencia CC BY, para no depender de listas con licencias dudosas.

## 5. Funciones

| Función | Qué hace |
|---|---|
| `sc_finding()` | Describe el resultado científico de partida |
| `sc_version(text, audience, lang)` | Registra una versión para un público |
| `sc_clarity()` | Indicadores de claridad (antes `assess_clarity()`) |
| `sc_fidelity()` | Rigor respecto a `sc_finding()` (antes `assess_scientific_rigor()`) |
| `sc_accessibility()` | Adecuación al público objetivo |
| `sc_uncertainty()` | Comunicación de incertidumbre y limitaciones (antes `assess_uncertainty()`) |
| `sc_visual(plot)` | Comprobaciones sobre un objeto ggplot2 (antes `assess_visual_quality()`) |
| `sc_narrative()` | Estructura del mensaje |
| `sc_compare(finding, versions)` | Matriz de fidelidad entre públicos (antes `compare_audiences()`) |
| `sc_rubric(dimensions, lang)` | Hoja de valoración para evaluadores humanos (CSV y HTML) |
| `sc_ratings(file)` y `sc_agreement()` | Importa valoraciones y calcula el acuerdo entre evaluadores (alfa de Krippendorff) |
| `sc_comprehension_items(finding)` | Borrador de preguntas de comprensión por elemento del resultado; el investigador debe revisarlas |
| `sc_comprehension(data)` | Analiza un estudio con lectores: acierto y confianza por versión y público, con intervalos |
| `sc_report()` | Informe HTML autocontenido (antes `generate_scicomm_report()`) |

## 6. Estudios con lectores

Es la parte que da validez al paquete, porque un algoritmo no puede demostrar que alguien ha comprendido algo:

- `sc_comprehension_items()` propone preguntas sobre la idea principal, la dirección, la certeza y la población a partir de `sc_finding()`.
- Una función de diseño asigna versiones a participantes al azar. Para el tamaño muestral se puede usar abfunnel.
- `sc_comprehension()` analiza acierto y calibración por versión y público.
- El paquete **no recoge datos**: el estudio necesita aprobación ética y se hace con la herramienta de encuestas del investigador.

## 7. Idiomas

Inglés y español desde la versión 0.1, con fórmulas de legibilidad y léxicos propios de cada idioma. Pocas herramientas de comunicación científica funcionan en español, así que esto también te diferencia.

## 8. Dependencias

Solo R base en `Imports`. `Suggests`: ggplot2 (para `sc_visual()`), knitr, rmarkdown y testthat.

## 9. Datos de ejemplo y web

- Un resultado ficticio con sus tres versiones (especialistas, estudiantes y público), en inglés y en español, y una de ellas con exageración deliberada para que el ejemplo enseñe algo. Todo marcado como sintético.
- Un conjunto sintético de respuestas de lectores para el ejemplo de `sc_comprehension()`.
- Web en `pablosancheznunez.com/scicomm/` con un informe de ejemplo.

## 10. Hoja de ruta

| Versión | Contenido |
|---|---|
| 0.1 | `sc_finding()`, `sc_version()`, claridad, accesibilidad, incertidumbre, fidelidad, `sc_compare()` e informe, en inglés y en español |
| 0.2 | `sc_visual()`, rúbricas y acuerdo entre evaluadores, narrativa |
| 0.3 | Herramientas para estudios con lectores |

## 11. Decisiones abiertas

1. **De dónde sale la lista de frecuencias para medir la jerga.** Las listas habituales tienen licencias que hay que revisar; la alternativa es construir una propia a partir de corpus abiertos.
2. **Umbrales por público.** Deben ser configurables y documentarse como convenciones, no como verdades.
3. **Validación de los indicadores.** Comparar los indicadores automáticos con las valoraciones de expertos y con la comprensión real de lectores sería un estudio publicable por sí mismo, y conviene planearlo desde el principio.
4. Si los estudios con lectores deberían entrar antes, en la versión 0.2, dado que son la prueba central.
