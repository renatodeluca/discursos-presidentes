if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  dplyr, 
  quanteda,  # corpus, tokens, matriz, diccionarios, coocurrencias 
  seededlda, # textmodel_lda() y textmodel_seededlda()
  readtext, # leer los .txt para recuperar el texto de los párrafos
  quanteda.textplots,  # textplot_network(): dibujar redes de palabras
  quanteda.textstats  # textplot_network(): dibujar redes de palabras
)

slda_allende_pinochet <- readRDS("slda_allende_pinochet")
slda_1990_2026 <- readRDS("slda_1990_2026.rds")

dtm_1990_2026 <- readRDS("dtm_1990_2026.rds")
dtm_allende_pinochet <- readRDS("dtm_allende_pinochet.rds")

