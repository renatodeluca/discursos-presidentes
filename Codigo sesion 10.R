#
#  Sesión 10 · Análisis computacional de textos III
#  Integrar y validar: modelos de tópicos semillados, coocurrencias y validación
#  Corpus: mensajes presidenciales de Allende (1971-1973) y Pinochet (1974-1980)
#  Introducción a la Ciencia Social Computacional y Experimental · 2026
#
#  ¿QUÉ HACE ESTE GUION?
#  Cierra la Unidad V. En la Sesión 9 aprendimos dos maneras de medir con
#  palabras: el modelo de tópicos (el computador INDUCE los temas) y el
#  diccionario (nosotros DEFINIMOS los temas). Hoy las combinamos: le damos al
#  modelo de tópicos nuestro diccionario como "semillas" y dejamos que él
#  descubra qué otras palabras las acompañan. Luego validamos (leemos lo que
#  medimos) y dibujamos redes de palabras que aparecen juntas (coocurrencias).
#
#  Este guion se lee JUNTO con la presentación "Sesión 10.pdf". Cada PASO de
#  este guion corresponde a una lámina con el mismo número de paso.
#
#  CÓMO USAR ESTE GUION
#  1. Trabaje en la MISMA carpeta que usó en las sesiones 8 y 9. Ahí deben estar:
#       - "dtm_mensajes_parrafos.rds"   (la matriz que guardó en la Sesión 8)
#       - la carpeta "Corpus Allende-Pinochet" con los diez .txt
#       - "slda_s10.rds"                (NUEVO: descárguelo de u-cursos)
#     En RStudio: Session > Set Working Directory > Choose Directory...
#  2. Corra el guion línea por línea: ponga el cursor en una línea y presione
#     Ctrl + Enter (Windows) o Cmd + Enter (Mac). Lea los comentarios ANTES
#     de correr cada línea: dicen qué debería ver en la consola.
#  3. Las líneas que empiezan con # son comentarios: R no las ejecuta.
#  4. Si algo falla, lea el mensaje de error, revise que está en la carpeta
#     correcta (getwd()) y que los archivos existen (file.exists()).
#


# ---- PASO 0 · Paquetes ------------------------------------------------
# Usamos los mismos paquetes de las sesiones 8 y 9. Solo hay uno que quizás
# no tenga: quanteda.textplots (para dibujar redes). Si no lo tiene, quite el
# # de la línea siguiente y córrala UNA sola vez:
# install.packages(c("quanteda.textplots", "seededlda"))

library(quanteda)            # corpus, tokens, matriz, diccionarios, coocurrencias
library(seededlda)           # textmodel_lda() y textmodel_seededlda()
library(readtext)            # leer los .txt para recuperar el texto de los párrafos
library(quanteda.textplots)  # textplot_network(): dibujar redes de palabras

getwd()                                   # ¿estoy en la carpeta correcta?
file.exists("dtm_mensajes_parrafos.rds")  # debe decir TRUE
file.exists("slda_s10.rds")               # debe decir TRUE (si dice FALSE, ver Paso 3)


# ---- PASO 1 · Recuperar la matriz y los párrafos ----------------------
# a) La matriz documento-término de la Sesión 8: 1.279 párrafos x 1.647 palabras.
#    Cada fila es un párrafo; cada columna, una palabra; cada celda, cuántas
#    veces aparece esa palabra en ese párrafo.
dtm <- readRDS("dtm_mensajes_parrafos.rds")
dtm
table(docvars(dtm, "presidente"))      # 1.022 párrafos de Allende y 257 de Pinochet

# b) El TEXTO de los párrafos, para poder LEERLOS (la matriz es una "bolsa de
#    palabras": perdió el orden y el texto). Son los mismos pasos de la Sesión 9.
archivos <- list.files("Corpus Allende-Pinochet", pattern = "\\.txt$",
                       recursive = TRUE, full.names = TRUE)
mensajes <- readtext(archivos, encoding = "UTF-8")
mensajes$anio <- as.integer(substr(mensajes$doc_id, 1, 4))
mensajes$presidente <- ifelse(mensajes$anio <= 1973, "Allende", "Pinochet")
mensajes$text <- gsub("([[:alpha:]])-\\s*\n\\s*", "\\1", mensajes$text)  # une palabras cortadas con guion
mensajes$text <- gsub("[ \t]+", " ", mensajes$text)                       # espacios dobles
corp <- corpus(mensajes)
docnames(corp) <- paste(corp$presidente, corp$anio, sep = "_")
corp_par <- corpus_reshape(corp, to = "paragraphs")                 # un documento por párrafo
corp_par <- corpus_subset(corp_par, ntoken(tokens(corp_par)) >= 10) # fuera los párrafos de < 10 palabras
corp_par <- corpus_subset(corp_par, grepl("[a-záéíóúñ]", corp_par)) # fuera los títulos en mayúsculas

# Control: los párrafos y las filas de la matriz deben ser exactamente los mismos
identical(docnames(corp_par), docnames(dtm))   # debe decir TRUE
texto <- as.character(corp_par)                # vector con el texto de cada párrafo


# ---- PASO 2 · El diccionario de la Sesión 9 (nuestra teoría) -----------
# Es EXACTAMENTE el mismo diccionario de la Sesión 9: tres marcos discursivos,
# cada uno definido por una lista de palabras. El * es un comodín
# ("socialis*" = socialismo, socialista, socialistas...). El guion bajo une
# expresiones de varias palabras que en la Sesión 8 tratamos como una sola.
dic <- dictionary(list(
  transformacion = c("socialis*", "revoluc*", "capitalis*", "explota*", "clase",
                     "clases", "monopol*", "imperialis*", "latifund*", "oligarq*",
                     "liberación", "nacionalización", "transformaci*",
                     "unidad_popular", "gobierno_popular", "área_de_propiedad_social"),
  orden          = c("patria", "orden", "seguridad", "fuerzas_armadas",
                     "junta_de_gobierno", "reconstrucción", "caos", "disciplina",
                     "soberan*", "autoridad*", "subversi*", "marxis*"),
  mercado        = c("mercado*", "privad*", "inversión", "inversiones", "competencia",
                     "eficien*", "iniciativa*", "libertad", "moderniza*",
                     "subsidiari*", "empresarios")))
dic

# Las palabras de la matriz que calzan con cada lista: ESTAS son las semillas
semillas <- featnames(dfm_select(dtm, pattern = dic))
length(semillas)   # 52 palabras de la matriz calzan con las listas: son las semillas
semillas


# ==== INTEGRAR TEORÍA Y DATOS: EL MODELO SEMILLADO =====================

# ---- PASO 3 · Estimar un modelo de tópicos semillado (seeded LDA) -----
# Diferencia con el LDA de la Sesión 9:
#  - LDA:        "busca 8 temas; tú decides después qué significan".
#  - Seeded LDA: "busca 3 temas que partan de ESTAS palabras (las semillas) y
#                 además 5 temas libres (residual = 5) para todo lo demás".
#  Total: 3 + 5 = 8 tópicos, igual que el LDA de la Sesión 9, para comparar.
#  Las semillas no son una lista cerrada: el modelo agrega a cada tópico las
#  palabras que aparecen JUNTO a las semillas en los mismos párrafos.

# OPCIÓN 1 (RECOMENDADA): cargar el modelo del profesor, para que sus cifras
# coincidan EXACTAMENTE con las de la presentación.
slda <- readRDS("slda_s10.rds")

# OPCIÓN 2: estimarlo usted. Tarda unos 5 segundos. Quite los # para correrla.
# Como vimos en la Sesión 9, el azar se genera distinto en cada computador:
# con la misma semilla, sus cifras pueden variar algo respecto de la lámina.
# set.seed(2026)
# slda <- textmodel_seededlda(dtm, dictionary = dic, residual = 5)
# (NO lo guarde con el nombre "slda_s10.rds": reemplazaría el del profesor.)

slda          # resumen del modelo


# ---- PASO 4 · ¿Qué agregó el modelo a nuestras semillas? ---------------
# a) Las 12 palabras más probables de cada tópico (la tabla phi, como en la Sesión 09).
#    Las tres primeras columnas son NUESTROS marcos; "other1" a "other5" son
#    los tópicos libres.
terms(slda, 12)

# b) Las palabras DESCUBIERTAS: las que el modelo puso entre las 20 más
#    probables de cada marco y que NO estaban en nuestro diccionario.
#    Esto es lo nuevo de hoy: los datos le responden a la teoría.
top20 <- terms(slda, 20)
for (marco in names(dic)) {
  cat("\n", marco, "->", paste(setdiff(top20[, marco], semillas), collapse = ", "), "\n")
}
#  Qué debería ver (con el modelo del profesor):
#   orden   -> chile, país, chilenos, vida, historia, futuro...  el orden viene
#              pegado a la NACIÓN: el mismo tema que el LDA llamó "Chile: nación y patria".
#   mercado -> desarrollo, recursos, sector, industriales, zona, planes...  ¡no es
#              el mercado! Es la planificación regional de Allende: "inversión"
#              era inversión del Estado. La semilla arrastró otro concepto.

# c) ¿Cuánto pesa cada tópico en cada presidente? (promedio de theta, en %)
theta <- slda$theta
prev <- aggregate(theta, by = list(presidente = docvars(dtm, "presidente")), FUN = mean)
rownames(prev) <- prev$presidente
round(t(prev[, -1]) * 100, 1)
#  orden: 11,0 % en Allende y 22,3 % en Pinochet; transformación: 16,9 % y 10,2 %.

# d) Tópico principal de cada párrafo, por presidente (cuántos párrafos)
table(topics(slda), docvars(dtm, "presidente"))

# e) Comparar con el diccionario de la Sesión 9 (palabras por cada 1.000)
marcos <- dfm_lookup(dtm, dictionary = dic)
por_pres <- dfm_group(marcos, groups = presidente)
total_pres <- ntoken(dfm_group(dtm, groups = presidente))
round(as.matrix(por_pres) / total_pres * 1000, 1)
#  El diccionario separa mucho más a los presidentes (transformación 22,0 vs 3,0)
#  que el modelo semillado (16,9 % vs 10,2 %). ¿Por qué? El diccionario cuenta
#  SOLO las semillas; el modelo mide el párrafo entero, incluyendo las palabras
#  vecinas que descubrió (por ejemplo, "régimen" e "institucional", que Pinochet
#  usa al hablar de su "nueva institucionalidad").


# ---- PASO 5 · Validar: leer lo que medimos -----------------------------
# Un modelo semillado también debe validarse: la etiqueta "orden" la pusimos
# nosotros; el modelo no sabe qué significa.

# a) Los tres párrafos MÁS típicos de cada marco (el theta más alto)
for (marco in names(dic)) {
  cat("\n=====", marco, "=====\n")
  top <- order(theta[, marco], decreasing = TRUE)[1:3]
  for (i in top) cat(names(texto)[i], " (theta =", round(theta[i, marco], 2), ")\n",
                     substr(texto[i], 1, 300), "...\n\n")
}
#  "mercado": los tres son de desarrollo regional de Allende (1972). Confirma
#  lo que vimos en el Paso 4b: este tópico NO mide lo que le pusimos de nombre.

# b) ACTIVIDAD 2: la muestra para codificar a mano.
#    Ocho párrafos cuyo tópico principal es "orden": cuatro con theta alto
#    (>= 0,5) y cuatro con theta bajo (< 0,35). Son los de la lámina.
principal <- topics(slda)
alto <- which(principal == "orden" & theta[, "orden"] >= 0.5)
bajo <- which(principal == "orden" & theta[, "orden"] < 0.35)
set.seed(10)
muestra <- c(sample(alto, 4), sample(bajo, 4))
for (i in muestra) cat(names(texto)[i], " (theta =", round(theta[i, "orden"], 2), ")\n",
                       texto[i], "\n\n")
#  Para cada párrafo decida: ¿trata del orden, la autoridad o la nación
#  amenazada? (sí / no / duda). Precisión = número de "sí" / 8.
#  Calcule la precisión por separado para los de theta alto y los de theta bajo.

# c) ¿Hasta dónde llega "orden"? Precisión según el umbral de theta
#    (cuántos párrafos quedan si exigimos un theta mínimo)
sapply(c(0.2, 0.3, 0.4, 0.5, 0.6), function(u) sum(principal == "orden" & theta[, "orden"] >= u))


# ==== MAPAS DE PALABRAS: COOCURRENCIAS ==================================

# ---- PASO 6 · Coocurrencias: qué palabras aparecen juntas --------------
# Es la lógica de la lectura obligatoria (Gómez-Cruz et al., 2024): dos
# palabras están "conectadas" si aparecen cerca. Ellos lo hicieron con las
# palabras clave de 7.814 artículos y VOSviewer; nosotros, con las palabras de
# los discursos y R.
# Tokens limpios, como en la Sesión 8 (sin puntuación, minúsculas, sin stopwords)
toks <- tokens(corp_par, remove_punct = TRUE, remove_numbers = TRUE,
               remove_symbols = TRUE) |>
  tokens_tolower() |>
  tokens_compound(phrase(c("unidad popular", "fuerzas armadas", "gobierno popular",
                           "junta de gobierno", "área de propiedad social"))) |>
  tokens_remove(stopwords("es"))

# a) Vecinos de "patria": las palabras a 5 o menos de distancia (como en la 08)
for (p in c("Allende", "Pinochet")) {
  cerca <- tokens_select(tokens_subset(toks, presidente == p), "patria", window = 5)
  cerca <- tokens_remove(cerca, "patria")
  cat("\n", p, "\n"); print(topfeatures(dfm(cerca), 10))
}

# b) La matriz de coocurrencias (fcm = feature co-occurrence matrix):
#    palabras x palabras; cada celda cuenta cuántas veces dos palabras
#    aparecen a 5 o menos posiciones de distancia.
red_por_presidente <- function(p, n = 30) {
  t_p  <- tokens_subset(toks, presidente == p)
  fcm_p <- fcm(t_p, context = "window", window = 5)
  top  <- names(topfeatures(dfm(t_p), n))       # las n palabras más frecuentes
  fcm_select(fcm_p, pattern = top)               # nos quedamos con ellas
}
fcm_allende  <- red_por_presidente("Allende")
fcm_pinochet <- red_por_presidente("Pinochet")
fcm_allende[1:6, 1:6]     # un trocito de la matriz

# c) Dibujar las redes. Cada punto es una palabra; cada línea, que aparecen
#    juntas; las líneas más gruesas, que aparecen juntas más veces.
#    min_freq = 0.9 dibuja solo el 10 % de conexiones más fuertes (si no, se
#    ve una maraña). Pruebe 0.8 o 0.95 y vea qué cambia.
set.seed(1)   # la posición de los puntos tiene azar
textplot_network(fcm_allende, min_freq = 0.9, edge_color = "#2A5260",
                 vertex_color = "#4E3239", vertex_labelsize = 4)
set.seed(1)
textplot_network(fcm_pinochet, min_freq = 0.9, edge_color = "#2A5260",
                 vertex_color = "#4E3239", vertex_labelsize = 4)
#  Busque los "racimos" (clústeres): grupos de palabras muy conectadas entre
#  sí. Eso es lo que Gómez-Cruz y colegas llaman temas (nivel meso).

# Para guardar una red como imagen (para el Taller 3), por ejemplo:
# png("red_allende.png", width = 1600, height = 1200, res = 200)
# set.seed(1); print(textplot_network(fcm_allende, min_freq = 0.9))
# dev.off()


# ---- PASO 7 · Reproducibilidad: dejar constancia ------------------------
# Siempre informe con qué versiones trabajó (lo aprendimos en la Sesión 9:
# Mac y Windows dan modelos distintos con la misma semilla).
sessionInfo()

# Siguiente paso: el guion "Taller 3 - guion base R.R", que repite todo esto
# con las cuentas públicas de 1990 a 2026.
