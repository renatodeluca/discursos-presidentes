if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  dplyr, 
  quanteda,  # corpus, tokens, matriz, diccionarios, coocurrencias 
  seededlda, # textmodel_lda() y textmodel_seededlda()
  readtext, # leer los .txt para recuperar el texto de los párrafos
  quanteda.textplots,  # textplot_network(): dibujar redes de palabras
  quanteda.textstats  # textplot_network(): dibujar redes de palabras
)

# ---- PASO 1 · Leer las cuentas y sus metadatos --------------------------


## ---- 1.1 Discursos Allende-Pinochet ----

#  El TEXTO de los párrafos, para poder LEERLOS (la matriz es una "bolsa de
#    palabras": perdió el orden y el texto). Son los mismos pasos de la Sesión 9.
archivos_allende_pinochet <- list.files("Corpus Allende-Pinochet", pattern = "\\.txt$",
                       recursive = TRUE, full.names = TRUE)
mensajes <- readtext(archivos_allende_pinochet, encoding = "UTF-8")
mensajes$anio <- as.integer(substr(mensajes$doc_id, 1, 4))
mensajes$presidente <- ifelse(mensajes$anio <= 1973, "Allende", "Pinochet")
mensajes$text <- gsub("([[:alpha:]])-\\s*\n\\s*", "\\1", mensajes$text)  # une palabras cortadas con guion
mensajes$text <- gsub("[ \t]+", " ", mensajes$text)                       # espacios dobles
corp_par_allende_pinochet<- corpus(mensajes)
docnames(corp_par_allende_pinochet) <- paste(corp_par_allende_pinochet$presidente, corp_par_allende_pinochet$anio, sep = "_")
corp_par_allende_pinochet <- corpus_reshape(corp_par_allende_pinochet, to = "paragraphs")                 # un documento por párrafo
corp_par_allende_pinochet <- corpus_subset(corp_par_allende_pinochet, ntoken(tokens(corp_par_allende_pinochet)) >= 10) # fuera los párrafos de < 10 palabras
corp_par_allende_pinochet <- corpus_subset(corp_par_allende_pinochet, grepl("[a-záéíóúñ]", corp_par_allende_pinochet)) # fuera los títulos en mayúsculas

## ---- 1.2 Cuentas públicas 1990-2026 ----
# Leer archivos
archivos_1990_2026 <- list.files("Cuentas 1990-2026", pattern = "\\.txt$", full.names = TRUE)
cuentas  <- readtext(archivos_1990_2026, encoding = "UTF-8")

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

## Discursos Allende-Pinochet

# Expresiones de varias palabras que tratamos como una sola (AGREGAR!!!!)
compuestos <- c("derechos humanos", "crimen organizado", "fuerzas armadas",
                "seguridad social", "seguridad pública", "seguridad ciudadana",
                "derechos sociales", "clase media", "medio ambiente",
                "cambio climático", "crisis climática", "pueblos originarios",
                "pueblos indígenas", "estallido social", "estado de derecho",
                "orden público", "detenidos desaparecidos", "protección social",
                "crecimiento económico", "nueva constitución", "libre mercado")

toks_allende_pinochet <- tokens(corp_par_allende_pinochet, remove_punct = TRUE, remove_numbers = TRUE,
               remove_symbols = TRUE) |>
  tokens_tolower() |>
  tokens_compound(phrase(compuestos))
toks_allende_pinochet_sin <- tokens_remove(toks_allende_pinochet, stopwords("es"))

dtm_allende_pinochet <- dfm(toks_allende_pinochet_sin)
dtm_allende_pinochet <- dfm_trim(dtm_allende_pinochet, min_termfreq = 5)   # palabras que aparecen 5 o más veces
dtm_allende_pinochet                                      # unos 11.150 párrafos x 7.200 palabras
topfeatures(dtm_allende_pinochet, 30)


## 2.2 Cuentas públicas 1990-2026
corp_1990_2026 <- corpus(cuentas)
docnames(corp_1990_2026) <- paste(corp_1990_2026$gobierno, corp_1990_2026$anio, sep = "_")
corp_par_1990_2026 <- corpus_reshape(corp_1990_2026, to = "paragraphs")
corp_par_1990_2026 <- corpus_subset(corp_par_1990_2026, ntoken(tokens(corp_par_1990_2026)) >= 10)  # fuera párrafos < 10 palabras
corp_par_1990_2026 <- corpus_subset(corp_par_1990_2026, grepl("[a-záéíóúñ]", corp_par_1990_2026))  # fuera títulos en mayúsculas
ndoc(corp_par_1990_2026)                                      # unos 11.150 párrafos
table(factor(corp_par_1990_2026$gobierno, levels = orden_gob))  # ¡ojo! Kast tiene muy pocos (una sola cuenta)
texto <- as.character(corp_par_1990_2026)                     # el texto de cada párrafo, para leer

# Expresiones de varias palabras que tratamos como una sola (AGREGAR!!!!)
compuestos <- c("derechos humanos", "crimen organizado", "fuerzas armadas",
                "seguridad social", "seguridad pública", "seguridad ciudadana",
                "derechos sociales", "clase media", "medio ambiente",
                "cambio climático", "crisis climática", "pueblos originarios",
                "pueblos indígenas", "estallido social", "estado de derecho",
                "orden público", "detenidos desaparecidos", "protección social",
                "crecimiento económico", "nueva constitución", "libre mercado")

toks_1990_2026 <- tokens(corp_par_1990_2026, remove_punct = TRUE, remove_numbers = TRUE,
               remove_symbols = TRUE) |>
  tokens_tolower() |>
  tokens_compound(phrase(compuestos))
toks_1990_2026_sin <- tokens_remove(toks_1990_2026, stopwords("es"))

dtm_1990_2026 <- dfm(toks_1990_2026_sin)
dtm_1990_2026 <- dfm_trim(dtm_1990_2026, min_termfreq = 5)   # palabras que aparecen 5 o más veces
dtm_1990_2026                                      # unos 11.150 párrafos x 7.200 palabras
topfeatures(dtm_1990_2026, 30)
saveRDS(dtm_1990_2026, "dtm_1990_2026.rds") 
saveRDS(dtm_allende_pinochet, "dtm_allende_pinochet.rds") 


# ---- PASO 3 · Elegir la pregunta y su diccionario de semillas ----------

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

dic <- dic_A         # >>> CAMBIAR AQUÍ <<<  dic_A, dic_B, dic_C o dic_D
marcos <- names(dic)  # los nombres de sus tópicos sembrados
dic

# ¿Qué palabras de la matriz calzan con cada lista? ¡Revíselo siempre!
# Si una semilla tiene 0 apariciones, no sirve: cámbiela.
for (m in marcos) {
  cat("\n", m, "\n")
  print(sort(colSums(dfm_select(dtm_1990_2026, pattern = dic[m])), decreasing = TRUE))
}
semillas <- featnames(dfm_select(dtm_1990_2026, pattern = dic))

for (m in marcos) {
  cat("\n", m, "\n")
  print(sort(colSums(dfm_select(dtm_allende_pinochet, pattern = dic[m])), decreasing = TRUE))
}
semillas <- featnames(dfm_select(dtm_allende_pinochet, pattern = dic))

# ---- PASO 4 · Estimar el modelo de tópicos semillado --------------------
# residual = 15: además de sus tópicos sembrados, 15 tópicos libres para todo
# lo demás (salud, vivienda, política exterior...). Con pocos tópicos libres,
# los sembrados se "tragan" palabras ajenas. Tarda alrededor de 1 minuto.
set.seed(2026)                                   # >>> puede cambiarla en el Paso 7d
slda_allende_pinochet <- textmodel_seededlda(dtm_allende_pinochet, dictionary = dic, residual = 15)
saveRDS(slda_allende_pinochet, "slda_allende_pinochet.rds")   # guárdelo: lo necesitarán para reproducir sus cifras
# Para volver a cargarlo otro día sin estimarlo de nuevo:
# slda <- readRDS("slda_taller3.rds")
terms(slda_allende_pinochet, 15)[, marcos]     # las 15 palabras más probables de sus tópicos

## ---- 4.2 Cuentas publicas 1990-2026
set.seed(2026)                                   # >>> puede cambiarla en el Paso 7d
slda_1990_2026 <- textmodel_seededlda(dtm_1990_2026, dictionary = dic, residual = 15)
# Para volver a cargarlo otro día sin estimarlo de nuevo:
# slda <- readRDS("slda_taller3.rds")
terms(slda_1990_2026, 15)[, marcos]     # las 15 palabras más probables de sus tópicos
