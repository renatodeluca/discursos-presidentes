if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  dplyr, 
  quanteda,  # corpus, tokens, matriz, diccionarios, coocurrencias 
  seededlda, # textmodel_lda() y textmodel_seededlda()
  readtext, # leer los .txt para recuperar el texto de los párrafos
  quanteda.textplots,  # textplot_network(): dibujar redes de palabras
  quanteda.textstats  # textplot_network(): dibujar redes de palabras
)

# cargar matrices con los datos
slda_allende_pinochet <- readRDS("data/slda_allende_pinochet")
slda_1990_2026 <- readRDS("data/slda_1990_2026.rds")

# cargar modelos
dtm_1990_2026 <- readRDS("data/dtm_1990_2026.rds")
dtm_allende_pinochet <- readRDS("data/dtm_allende_pinochet.rds")

