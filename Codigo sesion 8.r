# 
#  Sesión 08 · Análisis computacional de textos I
#  Del archivo a la matriz: corpus, unidades, limpieza y preprocesamiento
#  Corpus: mensajes presidenciales de Allende (1971-1973) y Pinochet (1974-1980)
#  Introducción a la Ciencia Social Computacional y Experimental · 2026
# 
#  Cómo usar este guion
#  1. Abra RStudio y fije como directorio de trabajo la carpeta "Sesión 08"
#     (Session > Set Working Directory > Choose Directory...).
#     Dentro de ella debe estar la carpeta "Materiales" con los .txt.
#  2. Corra el guion paso a paso (Ctrl/Cmd + Enter). Cada PASO
#     corresponde a una lámina del Bloque 5.
#  3. La primera vez, instale los paquetes (PASO 0). Toma unos minutos.
# 


# ---- PASO 0 · Paquetes ------------------------------------------------
# Toda la unidad usa la misma familia de paquetes: quanteda.
# install.packages(c("quanteda", "quanteda.textstats", "quanteda.textplots", "readtext))

library(quanteda)            # corpus, tokens, matriz documento-término
library(quanteda.textstats)  # frecuencias y palabras distintivas
library(quanteda.textplots)  # gráficos
library(readtext)            # leer archivos de texto (txt, pdf, docx...)


# ---- PASO 1 · Leer los archivos y sus metadatos ----------------------
# Cada archivo .txt es un mensaje presidencial: es nuestro DOCUMENTO.
# a) Decidimos qué archivos entran al corpus
archivos <- list.files("Corpus Allende-Pinochet", pattern = "\\.txt$",
                       recursive = TRUE, full.names = TRUE)
archivos   # deberían aparecer 10 archivos

# b) Los leemos: una fila por archivo (doc_id = nombre, text = discurso)
mensajes <- readtext(archivos, encoding = "UTF-8")
substr(mensajes$text, 1, 40)          # el comienzo de cada mensaje
any(grepl("\ufeff", mensajes$text))   # control: debe decir FALSE

# c) Los METADATOS salen del nombre del archivo: 19710521c.txt -> año 1971
mensajes$anio <- as.integer(substr(mensajes$doc_id, 1, 4))
mensajes$presidente <- ifelse(mensajes$anio <= 1973, "Allende", "Pinochet")
View(mensajes)


# ---- PASO 2 · Limpiar lo que el escáner dejó mal y armar el corpus ----
# Los textos vienen de documentos impresos digitalizados (OCR).
limpiar <- function(x) {
  x <- gsub("([[:alpha:]])-\\s*\n\\s*", "\\1", x)  # a) une "co-" con "lectiva"
  x <- gsub("[ \t]+", " ", x)                       # b) espacios repetidos
  x
  }
mensajes$text <- limpiar(mensajes$text)

# El corpus: quanteda toma el texto y las demás columnas como metadatos
corp <- corpus(mensajes)
docnames(corp) <- paste(corp$presidente, corp$anio, sep = "_")
summary(corp)   # una fila por documento: tipos, tokens, oraciones

# Comprobación: ¿quedó bien unida la palabra?
kwic(tokens(corp), "colectiva", window = 4)   # 17 apariciones


# ---- PASO 3 · Cambiar la unidad de análisis: del mensaje al párrafo ----
# Diez documentos son muy pocos para comparar. Cada párrafo pasa a ser
# un documento, y conserva los metadatos de su mensaje.
corp_par <- corpus_reshape(corp, to = "paragraphs") # también "sentences" o "documents"
ndoc(corp_par) # Debiera ser igual a 1.488 documentos (i.e., párrafos)

# Los títulos no son párrafos: los sacamos con dos reglas
#  - párrafos de menos de 10 tokens ("ANTE EL CONGRESO PLENO")
#  - párrafos escritos enteramente en mayúsculas (portadas y encabezados:
#    "S.E. EL PRESIDENTE DE LA REPUBLICA ... INFORMA AL PAIS")
#  Sin la segunda regla, "augusto", "informa" y "pais" salen como las
#  palabras más distintivas de Pinochet en el PASO 6.
corp_par <- corpus_subset(corp_par, ntoken(corp_par) >= 10)
corp_par <- corpus_subset(corp_par, grepl("[a-záéíóúñ]", corp_par))
ndoc(corp_par) # Menor que el conteo anterior, igual a 1.279 documentos
table(docvars(corp_par, "presidente"))   # párrafos por presidente

# ¿Qué párrafos quedarán "fuera" del análisis?
fuera <- corpus_subset(corpus_reshape(corp, to = "paragraphs"),
                       ntoken(corpus_reshape(corp, to = "paragraphs")) < 10)
head(as.character(fuera), 20)   # los primeros 20 "párrafos" descartados


# ---- PASO 4 · Tokenizar y preprocesar --------------------------------
# Cada decisión de esta sección es una decisión teórica.
toks <- tokens(corp_par,
               remove_punct   = TRUE,   # puntuación
               remove_numbers = TRUE,   # cifras (¡se pierden 1973, 40 %...)
               remove_symbols = TRUE)  # quita símbolos como "$" o "%"
toks <- tokens_tolower(toks) # Todo a minúsculas, "Pueblo", "pueblo" y "PUEBLO" = misma palabra

# Expresiones de varias palabras que funcionan como una sola
toks <- tokens_compound(toks, phrase(c("unidad popular", "fuerzas armadas",
                                       "gobierno popular", "junta de gobierno",
                                       "área de propiedad social")))

# Palabras funcionales (stopwords) del castellano
head(stopwords("es"), 30) #ver primeras 30 palabras; son 308.
"nosotros" %in% stopwords("es")   # TRUE: ¡ojo si estudiamos populismo!
toks_sin <- tokens_remove(toks, stopwords("es"))

#Podemos agregar palabras al diccionario de stopwords (e.g., "año", "hoy"):
#mis_vacias <- c(stopwords("es"), "año", "hoy", "así", "hacia")
#toks_sin <- tokens_remove(toks, mis_vacias)

# Comprobar corpus con y sin stopwords
sum(ntoken(toks))      # 72.258 palabras con stopwords
sum(ntoken(toks_sin))  # 37.061 sin ellas: se fue el 49 %

# (Opcional) Raíces o "stemming": recortar cada palabra hasta su raíz.
# NO se usa en el resto del análisis. Para probarlo, quite el # de cada línea.
# El resultado queda en un objeto aparte (toks_raiz), así toks_sin no cambia.
# toks_raiz <- tokens_wordstem(toks_sin, language = "spanish")
# head(as.list(toks_sin)[[1]], 8)    # "comparecer" "ustedes" "cumplir" ...
# head(as.list(toks_raiz)[[1]], 8)   # "comparec"   "usted"   "cumpl"   ...
#
# Lo que junta y lo que separa (las reglas son mecánicas):
# char_wordstem(c("trabajador", "trabajadores", "trabajo", "trabajar"),
#               language = "spanish")  # todas -> "trabaj": sujeto y actividad juntos
# char_wordstem(c("popular", "populares"),
#               language = "spanish")  # "popul" y "popular": ¡quedan separadas!
# char_wordstem(c("general", "generalmente"),
#               language = "spanish")  # ambas -> "general": sustantivo y adverbio juntos


# ---- PASO 5 · La matriz documento-término ----------------------------
# Filas = párrafos, columnas = palabras, celdas = cuántas veces aparece.
dtm <- dfm(toks_sin)
dtm                                   # dimensiones y porcentaje de ceros
dtm <- dfm_trim(dtm, min_termfreq = 5)   # palabras que aparecen 5+ veces
dtm #la nueva matriz tiene 1.647 features

#Otras opciones de dfm:
#dfm_trim(dtm, min_docfreq = 3)      # palabras que aparecen en 3 o más párrafos
#dfm_trim(dtm, max_docfreq = 0.5,    # quitar palabras presentes en más del 50 %
#         docfreq_type = "prop")     # de los párrafos (casi stopwords del corpus)


#Para mirar la matriz dtm por dentro:
dtm[1:5, 1:8]           # las 5 primeras filas y 8 primeras columnas
topfeatures(dtm, 15)    # las 15 palabras más frecuentes de todo el corpus
nfeat(dtm); ndoc(dtm)   # número de columnas y de filas


# Lo mismo que el panel Summary de Voyant (por mensaje, no por párrafo)
toks_msg <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) |> tokens_tolower()
resumen <- data.frame(mensaje   = docnames(toks_msg),
                      palabras  = ntoken(toks_msg),
                      distintas = ntype(toks_msg))
resumen$densidad <- round(resumen$distintas / resumen$palabras, 3)
resumen


# ---- PASO 6 · Explorar: lo mismo que Voyant, pero reproducible -------
# 6a. Las palabras más frecuentes de cada presidente
textstat_frequency(dtm, n = 12, groups = presidente)

#Nube de palabras
set.seed(1)
textplot_wordcloud(dtm, max_words = 80,
                   color = rev(hcl.colors(6, "viridis"))) #se lee bien en byw y para personas con daltonismo
                                                          # otras opción es rev(hcl.colors(6, "YlOrRd"))

#Separada por presidente:
set.seed(1)
textplot_wordcloud(dfm_subset(dtm, presidente == "Allende"), max_words = 60, 
                   color = rev(hcl.colors(6, "viridis"))) #Allende

set.seed(1)
textplot_wordcloud(dfm_subset(dtm, presidente == "Pinochet"), max_words = 60,
                   color = rev(hcl.colors(6, "viridis"))) #Pinochet

#Por períodos
set.seed(1)
textplot_wordcloud(dfm_subset(dtm, anio == 1972), max_words = 60,
                   color = rev(hcl.colors(6, "viridis"))) #Solo 1972

set.seed(1)
textplot_wordcloud(dfm_subset(dtm, anio %in% c(1974, 1975, 1976)), max_words = 60,
                   color = rev(hcl.colors(6, "viridis"))) #Solo años 1974, 1975 y 1976

set.seed(1)
textplot_wordcloud(dfm_subset(dtm, anio >= 1977), max_words = 60,
                   color = rev(hcl.colors(6, "viridis"))) #Desde 1977 en adelante

# 6b. Palabras en contexto (Contexts / KWIC en Voyant)
kwic(toks, "pueblo", window = 6) |> head(10)
kwic(toks, "patria", window = 6) |> head(10)

#Para ver todas las apariciones de una vez en una tabla navegable
View(kwic(toks, "pueblo", window = 6))

# Contextos de "pueblo" solo en Allende
kwic(tokens_subset(toks, presidente == "Allende"), "pueblo", window = 6) |> head(10)

# Contextos de "pueblo" solo en Pinochet
kwic(tokens_subset(toks, presidente == "Pinochet"), "pueblo", window = 6) |> head(10)


#Collocates: ¿qué palabras acompañan a “patria” en cada presidente?:
# Palabras a 5 o menos de distancia de "patria", por presidente
for (p in c("Allende", "Pinochet")) {
  cerca <- tokens_select(tokens_subset(toks_sin, presidente == p),
                         "patria", window = 5)
  cerca <- tokens_remove(cerca, "patria")
  cat("\n", p, "\n"); print(topfeatures(dfm(cerca), 8))
}

# Búsqueda por raíz con comodín, como "trabaj*" en Voyant: el * reemplaza
# cualquier terminación. A diferencia del stemming, NO cambia los tokens: solo busca.
kwic(toks, "trabaj*", window = 6) |> head(10)
table(kwic(toks, "trabaj*")$keyword)   # qué palabras encontró: 351 apariciones

# 6c. Palabras distintivas (keyness): ¿qué dice uno que el otro no?
dtm_pres <- dfm_group(dtm, groups = presidente)
key <- textstat_keyness(dtm_pres, target = "Allende")
head(key, 15); tail(key, 15) # muestra las 15 palabras más típicas de Allende (Chi2 positivo) y Pinochet (Chi2 negativo)
textplot_keyness(key, n = 15, color = c("#2A5260", "#4E3239"), # dibuja el gráfico de barras
                 labelsize = 3.5)

# 6d. Frecuencia relativa por año (Trends en Voyant)
#     Por cada 10.000 palabras, para que el largo de los textos no engañe
dtm_anio <- dfm_group(dfm(toks), groups = anio)
relativa <- dfm_weight(dtm_anio,                 # divide cada conteo por el total de palabras de esa fila. 
                       scheme = "prop") * 10000  #El "* 10000" lo convierte en “por cada 10.000 palabras”.
round(as.matrix(relativa[, c("pueblo", "trabajadores", "patria",
                             "orden", "nosotros")]), 1)

# Graficar los "Trends" de 4 palabras a lo largo de los años
m <- as.matrix(relativa[, c("pueblo", "patria", "trabajadores", "orden")])
anios <- as.integer(rownames(m))
colores <- c("#2A5260", "#4E3239", "#9FB4BD", "#172A3A")

matplot(anios, m, type = "b", pch = 19, lty = 1, col = colores,
        xlab = "Año", ylab = "Frecuencia por cada 10.000 palabras",
        xaxt = "n")                       # no dibujar el eje x automático
axis(1, at = anios, labels = anios)       # eje x con todos los años
legend("topright", colnames(m), col = colores, lty = 1, pch = 19)
abline(v = 1973.5, lty = 2)    # la línea punteada marca el cambio de régimen


# Pares de palabras que aparecen juntos más de lo esperable por azar.
textstat_collocations(toks_sin, size = 2, min_count = 10) |> head(15) #atención: usamos toks_sin. 
                                                                      #Sin palabras vacías se juntan pares 
                                                                      #que no estaban seguidos, como "presidente república"

# ---- Para el proyecto ------------------------------------------------
# Guarde el objeto para la Sesión 09 (modelos de tópicos y diccionarios)
saveRDS(dtm, "dtm_mensajes_parrafos.rds")
