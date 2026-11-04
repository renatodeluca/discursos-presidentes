#
#  TALLER 3 · Guion base
#  Treinta y seis años de cuentas públicas (1990-2026)
#  Introducción a la Ciencia Social Computacional y Experimental · 2026
#
#  ¿QUÉ HACE ESTE GUION?
#  Lleva, paso a paso, todo lo necesario para el Taller 3:
#    PASO 1  leer las 37 cuentas públicas y sus metadatos
#    PASO 2  construir los párrafos y la matriz documento-término
#    PASO 3  elegir UNA de las cuatro preguntas (A, B, C o D) y su diccionario
#    PASO 4  estimar el modelo de tópicos semillado
#    PASO 5  interpretar: palabras sembradas y palabras descubiertas
#    PASO 6  medir: peso de cada tópico por gobierno, por año y en las primeras cuentas
#    PASO 7  validar: leer, codificar a mano, kwic() y robustez
#    PASO 8  coocurrencias: la red de una palabra clave en dos gobiernos
#    PASO 9  guardar resultados y dejar constancia de versiones
#  Lea la "Pauta Taller 3" y las láminas 30 a 37 de "Sesión 10.pdf" antes de empezar.
#
#  ANTES DE EMPEZAR
#  1. Descargue de u-cursos la carpeta "Cuentas 1990-2026" (37 archivos .txt)
#     y el archivo "metadatos_cuentas_1990_2026.csv". Póngalos en una carpeta
#     de trabajo, por ejemplo "Taller 3". Dentro de ella debe quedar:
#         Taller 3/
#            Cuentas 1990-2026/      (los 37 .txt, de "1990 Aylwin.txt" a "2026 Kast.txt")
#            metadatos_cuentas_1990_2026.csv
#            Taller 3 - guion base R.R   (este archivo)
#  2. En RStudio: Session > Set Working Directory > Choose Directory... y elija
#     la carpeta "Taller 3".
#  3. Corra línea por línea (Ctrl/Cmd + Enter). Lo que USTEDES deben cambiar
#     está marcado con  >>> CAMBIAR AQUÍ <<<
#
#  SOBRE EL CORPUS: los archivos que teníamos de 2010 y 2011 no eran el discurso
#  de Piñera, sino la "Cuenta Pública Sectorial" (el anexo con el informe de cada
#  ministerio: 216.000 y 342.000 palabras, contra unas 15.000 de un discurso).
#  Se reemplazaron por el discurso leído, convertido a texto desde los PDF del
#  archivo histórico de la Cámara de Diputados. Kast tiene una sola cuenta: es
#  un límite del corpus que deben mencionar en el informe.


# ---- PASO 0 · Paquetes -------------------------------------------------
# Los mismos de las sesiones 8, 9 y 10. Si falta alguno, quite el # y corra UNA vez:
# install.packages(c("quanteda", "seededlda", "readtext", "quanteda.textplots"))
library(quanteda)
library(seededlda)
library(readtext)
library(quanteda.textplots)

getwd()                                              # ¿estoy en "Taller 3"?
length(list.files("Cuentas 1990-2026", pattern = "\\.txt$"))   # debe decir 37
file.exists("metadatos_cuentas_1990_2026.csv")       # debe decir TRUE


# ---- PASO 1 · Leer las cuentas y sus metadatos --------------------------
archivos <- list.files("Cuentas 1990-2026", pattern = "\\.txt$", full.names = TRUE)
cuentas  <- readtext(archivos, encoding = "UTF-8")

# Metadatos: una fila por cuenta pública (año, presidente, gobierno, coalición,
# sector, n° de cuenta dentro del mandato, si es la primera y n° de palabras)
meta <- read.csv("metadatos_cuentas_1990_2026.csv", encoding = "UTF-8")
head(meta)
# Unimos texto y metadatos por el nombre del archivo
cuentas <- merge(cuentas, meta, by.x = "doc_id", by.y = "archivo")
cuentas$text <- gsub("([[:alpha:]])-\\s*\n\\s*", "\\1", cuentas$text)  # palabras cortadas con guion
cuentas$text <- gsub("[ \t]+", " ", cuentas$text)                       # espacios dobles

# El orden de los gobiernos (para que las tablas y gráficos salgan en orden histórico)
orden_gob <- c("Aylwin", "Frei", "Lagos", "Bachelet I", "Piñera I",
               "Bachelet II", "Piñera II", "Boric", "Kast")


# ---- PASO 2 · Párrafos y matriz documento-término ----------------------
# La unidad de análisis es el PÁRRAFO, como en lo vimos en clases (Unidad V).
corp <- corpus(cuentas)
docnames(corp) <- paste(corp$gobierno, corp$anio, sep = "_")
corp_par <- corpus_reshape(corp, to = "paragraphs")
corp_par <- corpus_subset(corp_par, ntoken(tokens(corp_par)) >= 10)  # fuera párrafos < 10 palabras
corp_par <- corpus_subset(corp_par, grepl("[a-záéíóúñ]", corp_par))  # fuera títulos en mayúsculas
ndoc(corp_par)                                      # unos 11.150 párrafos
table(factor(corp_par$gobierno, levels = orden_gob))  # ¡ojo! Kast tiene muy pocos (una sola cuenta)
texto <- as.character(corp_par)                     # el texto de cada párrafo, para leer

# Expresiones de varias palabras que tratamos como una sola (agregue las suyas)
compuestos <- c("derechos humanos", "crimen organizado", "fuerzas armadas",
                "seguridad social", "seguridad pública", "seguridad ciudadana",
                "derechos sociales", "clase media", "medio ambiente",
                "cambio climático", "crisis climática", "pueblos originarios",
                "pueblos indígenas", "estallido social", "estado de derecho",
                "orden público", "detenidos desaparecidos", "protección social",
                "crecimiento económico", "nueva constitución", "libre mercado")

toks <- tokens(corp_par, remove_punct = TRUE, remove_numbers = TRUE,
               remove_symbols = TRUE) |>
  tokens_tolower() |>
  tokens_compound(phrase(compuestos))
toks_sin <- tokens_remove(toks, stopwords("es"))

dtm <- dfm(toks_sin)
dtm <- dfm_trim(dtm, min_termfreq = 5)   # palabras que aparecen 5 o más veces
dtm                                      # unos 11.150 párrafos x 7.200 palabras
topfeatures(dtm, 30)


# ---- PASO 3 · Elegir la pregunta y su diccionario de semillas ----------
# Hay cuatro preguntas. Cada grupo elige UNA (ver la pauta). Cada diccionario es
# una hipótesis: "estas palabras indican este concepto". Pueden AGREGAR o QUITAR
# palabras (máximo 5 cambios por lista), pero deben justificarlo en el informe.

# A. ¿Volvió el lenguaje de Allende?  (el mismo diccionario de las sesiones 9 y 10)
dic_A <- dictionary(list(
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

# B. ¿De quién es la seguridad?  (seguridad y delito frente a derechos sociales)
dic_B <- dictionary(list(
  seguridad = c("delincuen*", "delito*", "crimen", "criminal*", "crimen_organizado",
                "narcotráfico", "narco*", "carabineros", "policía*", "cárcel*",
                "homicidio*", "seguridad_pública", "seguridad_ciudadana"),
  derechos  = c("desigualdad*", "derechos_sociales", "dignidad", "pensiones",
                "gratuidad", "protección_social", "equidad", "inclusión",
                "redistribu*", "abusos")))

# C. Memoria y transición  (el pasado dictatorial y la política de los acuerdos)
dic_C <- dictionary(list(
  memoria    = c("derechos_humanos", "reconciliación", "verdad", "víctimas",
                 "memoria", "detenidos_desaparecidos", "desaparecidos", "tortura",
                 "reparación", "rettig", "valech"),
  transicion = c("consenso*", "acuerdo*", "transición", "gobernabilidad",
                 "diálogo", "unidad", "convivencia", "paz", "entendimiento")))

# D. ¿Dos derechas?  (un lenguaje liberal frente a uno nacional-conservador)
dic_D <- dictionary(list(
  liberal       = c("libertad*", "emprend*", "oportunidades", "innovación",
                    "mercado*", "competencia", "clase_media", "mérito",
                    "productividad", "inversión"),
  conservadora  = c("familia*", "patria", "nación", "soberanía", "frontera*",
                    "orden", "autoridad", "vida", "valores", "orden_público")))

dic <- dic_B          # >>> CAMBIAR AQUÍ <<<  dic_A, dic_B, dic_C o dic_D
marcos <- names(dic)  # los nombres de sus tópicos sembrados
dic

# ¿Qué palabras de la matriz calzan con cada lista? ¡Revíselo siempre!
# Si una semilla tiene 0 apariciones, no sirve: cámbiela.
for (m in marcos) {
  cat("\n", m, "\n")
  print(sort(colSums(dfm_select(dtm, pattern = dic[m])), decreasing = TRUE))
}
semillas <- featnames(dfm_select(dtm, pattern = dic))


# ---- PASO 4 · Estimar el modelo de tópicos semillado --------------------
# residual = 15: además de sus tópicos sembrados, 15 tópicos libres para todo
# lo demás (salud, vivienda, política exterior...). Con pocos tópicos libres,
# los sembrados se "tragan" palabras ajenas. Tarda alrededor de 1 minuto.
set.seed(2026)                                   # >>> puede cambiarla en el Paso 7d
slda <- textmodel_seededlda(dtm, dictionary = dic, residual = 15)
saveRDS(slda, "slda_taller3.rds")   # guárdelo: lo necesitarán para reproducir sus cifras
# Para volver a cargarlo otro día sin estimarlo de nuevo:
# slda <- readRDS("slda_taller3.rds")


# ---- PASO 5 · Interpretar: semillas y palabras descubiertas -------------
terms(slda, 15)[, marcos]     # las 15 palabras más probables de sus tópicos

# Las palabras que el modelo AGREGÓ (entre las 20 más probables, sin ser semillas)
top20 <- terms(slda, 20)
for (m in marcos) cat("\n", m, "->", paste(setdiff(top20[, m], semillas), collapse = ", "), "\n")
# Pregunta para el informe: ¿las palabras descubiertas confirman su concepto,
# lo amplían o muestran que el tópico mide OTRA cosa?


# ---- PASO 6 · Medir: ¿cuánto pesa cada tópico, en quién y cuándo? -------
theta <- slda$theta
gob   <- factor(docvars(dtm, "gobierno"), levels = orden_gob)
anio  <- docvars(dtm, "anio")

# a) Peso promedio (theta, en %) por gobierno
prev_gob <- aggregate(theta[, marcos, drop = FALSE], by = list(gobierno = gob), FUN = mean)
prev_gob[, marcos] <- round(prev_gob[, marcos] * 100, 1)
prev_gob

# b) % de párrafos cuyo tópico PRINCIPAL es cada uno de sus tópicos
principal <- topics(slda)
round(prop.table(table(gob, principal), 1)[, marcos] * 100, 1)

# c) Gráfico por año (una línea por tópico sembrado)
prev_anio <- aggregate(theta[, marcos, drop = FALSE], by = list(anio = anio), FUN = mean)
colores <- c("#4E3239", "#2A5260", "#9FB4BD")[seq_along(marcos)]
matplot(prev_anio$anio, prev_anio[, marcos] * 100, type = "b", pch = 19, lty = 1,
        col = colores, xlab = "Año", ylab = "Peso promedio del tópico (%)")
legend("topleft", marcos, col = colores, lty = 1, pch = 19, bty = "n")
abline(v = c(1993.5, 1999.5, 2005.5, 2009.5, 2013.5, 2017.5, 2021.5, 2025.5),
       lty = 3, col = "grey60")             # cambios de gobierno
# Para guardarlo: en la pestaña Plots, Export > Save as Image...

# d) Solo las PRIMERAS cuentas de cada mandato (la comparación más justa con Kast,
#    que tiene una sola).
primeras <- docvars(dtm, "primera_cuenta") == "sí"
prev_primeras <- aggregate(theta[primeras, marcos, drop = FALSE],
                           by = list(gobierno = gob[primeras]), FUN = mean)
prev_primeras[, marcos] <- round(prev_primeras[, marcos] * 100, 1)
prev_primeras

# e) Contraste teoría-first: el diccionario solo (palabras por cada 1.000)
cuenta_dic <- dfm_group(dfm_lookup(dtm, dic), groups = gobierno)
total_gob  <- ntoken(dfm_group(dtm, groups = gobierno))
round(as.matrix(cuenta_dic) / total_gob * 1000, 1)[orden_gob, ]


# ---- PASO 7 · Validar ------------------------------------------------------
m <- marcos[1]   # >>> CAMBIAR AQUÍ <<<  el tópico que van a validar (marcos[1] o marcos[2]...)

# a) Los 5 párrafos más típicos del tópico (theta más alto): ¿tratan de lo que dice su nombre?
for (i in order(theta[, m], decreasing = TRUE)[1:5]) {
  cat("\n#", names(texto)[i], " theta =", round(theta[i, m], 2), "\n", texto[i], "\n")
}

# b) Muestra al azar para CODIFICAR A MANO: 20 párrafos cuyo tópico principal es m.
#    Se guarda en un archivo .csv que pueden abrir en Excel. En la columna
#    "codigo" escriban 1 (sí trata del concepto), 0 (no) o 9 (duda).
set.seed(3)                                       # no la cambien: así su muestra es reproducible
candidatos <- which(principal == m)
elegidos <- sample(candidatos, min(20, length(candidatos)))
muestra <- data.frame(parrafo = names(texto)[elegidos],
                      gobierno = gob[elegidos],
                      theta = round(theta[elegidos, m], 2),
                      texto = texto[elegidos],
                      codigo = "")
write.csv(muestra, paste0("muestra_validacion_", m, ".csv"), row.names = FALSE,
          fileEncoding = "UTF-8")
# Después de codificar en Excel, guárdenlo como .csv y léanlo de vuelta:
# cod <- read.csv(paste0("muestra_validacion_", m, ".csv"), encoding = "UTF-8")
# mean(cod$codigo == 1)                         # precisión: proporción de "sí"
# tapply(cod$codigo == 1, cod$theta >= 0.4, mean) # ¿mejora con theta alto?
# Idealmente, dos integrantes codifican por separado y comparan:
# mean(cod$codigo_persona1 == cod$codigo_persona2)  # acuerdo simple

# c) Una palabra AMBIGUA en contexto (como "seguridad" en la Sesión 9)
#    >>> CAMBIAR AQUÍ <<< la palabra y los gobiernos que quieran comparar
palabra <- "seguridad"
kwic(tokens_subset(toks, gobierno == "Aylwin"), palabra, window = 5) |> head(10)
kwic(tokens_subset(toks, gobierno == "Boric"),  palabra, window = 5) |> head(10)
# ¿Qué palabra viene justo después? Una tabla rápida de sus "apellidos":
k <- kwic(toks, palabra, window = 2)
sort(table(k$post), decreasing = TRUE)[1:15]   # las dos palabras que siguen

# d) Robustez: cambien UNA decisión y vuelvan a estimar. Elijan una:
#    - otra semilla:         set.seed(1)
#    - otros tópicos libres: residual = 10 o 20
#    - quitar la semilla más dudosa de su diccionario
set.seed(1)                                        # >>> CAMBIAR AQUÍ <<<
slda_rob <- textmodel_seededlda(dtm, dictionary = dic, residual = 15)
terms(slda_rob, 10)[, marcos]
rob_gob <- aggregate(slda_rob$theta[, marcos, drop = FALSE], by = list(gobierno = gob), FUN = mean)
rob_gob[, marcos] <- round(rob_gob[, marcos] * 100, 1)
names(rob_gob)[-1] <- paste0(marcos, "_robustez")
cbind(prev_gob, rob_gob[, -1, drop = FALSE])   # original y robustez, lado a lado
# ¿Se mantiene el ORDEN de los gobiernos? Eso importa más que las cifras exactas.


# ---- PASO 8 · Coocurrencias: la red de una palabra clave -----------------
# Las palabras que aparecen a 5 o menos posiciones de su palabra clave, en dos
# gobiernos. El * incluye los compuestos (seguridad_pública, seguridad_ciudadana...).
clave <- "seguridad*"                  # >>> CAMBIAR AQUÍ <<<  (p. ej., "famili*", "derechos_humanos", "libertad")
gob_1 <- "Aylwin"                      # >>> CAMBIAR AQUÍ <<<
gob_2 <- "Boric"                       # >>> CAMBIAR AQUÍ <<<

red_clave <- function(g, n = 25) {
  cerca <- tokens_select(tokens_subset(toks_sin, gobierno == g), clave, window = 5)
  f <- fcm(cerca, context = "window", window = 5)
  fcm_select(f, pattern = names(topfeatures(dfm(cerca), n)))
}
# a) Los vecinos más frecuentes, en tabla
for (g in c(gob_1, gob_2)) {
  cerca <- tokens_remove(tokens_select(tokens_subset(toks_sin, gobierno == g),
                                       clave, window = 5), clave)
  cat("\n", g, "\n"); print(topfeatures(dfm(cerca), 12))
}
# b) Las dos redes (guárdenlas con Export > Save as Image...)
set.seed(1); textplot_network(red_clave(gob_1), min_freq = 0.5,
                              edge_color = "#2A5260", vertex_color = "#4E3239")
set.seed(1); textplot_network(red_clave(gob_2), min_freq = 0.5,
                              edge_color = "#2A5260", vertex_color = "#4E3239")
# Si la red sale vacía o con muy pocas palabras, la palabra clave es muy poco
# frecuente en ese gobierno: elijan otra o bajen min_freq a 0.2.


# ---- PASO 9 · Guardar y dejar constancia ---------------------------------
write.csv(prev_gob, "resultados_por_gobierno.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(prev_anio, "resultados_por_anio.csv", row.names = FALSE, fileEncoding = "UTF-8")
sessionInfo()      # copien en el anexo del informe las versiones de R, quanteda y seededlda
