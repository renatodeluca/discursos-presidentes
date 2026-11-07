if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
 
pacman::p_load(
  dplyr, 
  quanteda,  # corpus, tokens, matriz, diccionarios, coocurrencias 
  seededlda, # textmodel_lda() y textmodel_seededlda()
  readtext, # leer los .txt para recuperar el texto de los párrafos
  quanteda.textplots,  # textplot_network(): dibujar redes de palabras
  quanteda.textstats,  # textplot_network(): dibujar redes de palabras
  ggplot2,  # gráficos
  tidyr     # pivot_longer()
)
 
# cargar matrices con los datos
slda_allende_pinochet <- readRDS("data/slda_allende_pinochet.rds")
slda_1990_2026 <- readRDS("data/slda_1990_2026.rds")
 
# cargar modelos
dtm_1990_2026 <- readRDS("data/dtm_1990_2026.rds")
dtm_allende_pinochet <- readRDS("data/dtm_allende_pinochet.rds")
 
# otros objetos guardados en el script de procesamiento
toks_1990_2026_sin <- readRDS("data/toks_1990_2026_sin.rds")
corp_par_1990_2026 <- readRDS("data/corp_par_1990_2026.rds")
pred_cuentas <- readRDS("data/pred_cuentas_desde_AP.rds")   # tópicos de Allende-Pinochet proyectados sobre las cuentas
 
dir.create("figuras", showWarnings = FALSE)
 
# El orden de los gobiernos (para que las tablas y gráficos salgan en orden histórico)
orden_gob <- c("Aylwin", "Frei", "Lagos", "Bachelet I", "Piñera I",
               "Bachelet II", "Piñera II", "Boric", "Kast")
 
# El diccionario (el mismo del script de procesamiento; va completo en el Anexo)
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
marcos <- names(dic)
 
 
# ---- PASO 5 · Medición integrada (Parte 2) ------------------------------
 
## ---- 5.1 Las 15 palabras más probables: semillas vs. descubiertas ----
# "semilla" = la palabra calza con el diccionario de ese tópico;
# "descubierta" = el modelo la asoció al tópico sin que estuviera en el diccionario.
for (m in marcos) {
  cat("\n", m, "- Allende-Pinochet\n")
  pal <- terms(slda_allende_pinochet, 15)[, m]
  sem <- featnames(dfm_select(dtm_allende_pinochet, pattern = dic[m]))
  print(data.frame(palabra = pal, tipo = ifelse(pal %in% sem, "semilla", "descubierta")))
}
 
for (m in marcos) {
  cat("\n", m, "- Cuentas 1990-2026\n")
  pal <- terms(slda_1990_2026, 15)[, m]
  sem <- featnames(dfm_select(dtm_1990_2026, pattern = dic[m]))
  print(data.frame(palabra = pal, tipo = ifelse(pal %in% sem, "semilla", "descubierta")))
}
 
## ---- 5.2 Tópicos de Allende proyectados sobre las cuentas + metadatos ----
# pred_cuentas tiene una fila por párrafo (los que quedaron con palabras tras dfm_match)
pred_df <- as.data.frame(pred_cuentas[, marcos])
idx <- match(rownames(pred_cuentas), docnames(dtm_1990_2026))
pred_df$gobierno       <- docvars(dtm_1990_2026, "gobierno")[idx]
pred_df$anio           <- docvars(dtm_1990_2026, "anio")[idx]
pred_df$primera_cuenta <- docvars(dtm_1990_2026, "primera_cuenta")[idx]
 
nrow(pred_df); ndoc(dtm_1990_2026)   # cuántos párrafos se perdieron al proyectar (menciónelo en la Parte 5)
 
## ---- 5.3 Tabla por gobierno ----
tab_gob <- pred_df |>
  group_by(gobierno) |>
  summarise(across(all_of(marcos), mean), n_parrafos = n()) |>
  mutate(gobierno = factor(gobierno, levels = orden_gob)) |>
  arrange(gobierno)
tab_gob
 
# Lo mismo con el modelo estimado directamente en las cuentas (slda_1990_2026)
# OJO: su tópico "transformacion" salió con palabras de tiempo (años, meses, días): no mide ideología.
theta_propio <- as.data.frame(slda_1990_2026$theta[, marcos])
theta_propio$gobierno <- docvars(dtm_1990_2026, "gobierno")
tab_gob_propio <- theta_propio |>
  group_by(gobierno) |>
  summarise(across(all_of(marcos), mean), n_parrafos = n()) |>
  mutate(gobierno = factor(gobierno, levels = orden_gob)) |>
  arrange(gobierno)
tab_gob_propio
 
## ---- 5.4 Gráfico por año ----
graf_anio <- pred_df |>
  group_by(anio) |>
  summarise(across(all_of(marcos), mean)) |>
  pivot_longer(-anio, names_to = "topico", values_to = "proporcion") |>
  ggplot(aes(anio, proporcion, color = topico)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(x = NULL, y = "Proporción media del tópico (modelo Allende-Pinochet)", color = NULL) +
  theme_minimal()
graf_anio
ggsave("figuras/grafico_por_anio.png", graf_anio, width = 9, height = 5, dpi = 300)
 
## ---- 5.5 Comparación con las primeras cuentas de cada mandato ----
tab_primeras <- pred_df |>
  group_by(gobierno, primera_cuenta) |>
  summarise(across(all_of(marcos), mean), n_parrafos = n(), .groups = "drop") |>
  mutate(gobierno = factor(gobierno, levels = orden_gob)) |>
  arrange(gobierno, primera_cuenta)
tab_primeras
 
 
# ---- PASO 6 · Validación (Parte 3) --------------------------------------
# Texto de cada párrafo, alineado con las filas de pred_df
texto_pred <- as.character(corp_par_1990_2026)[rownames(pred_cuentas)]
tema <- "transformacion"      # >>> CAMBIAR AQUÍ <<< el tópico que va a validar
 
## ---- 6.1 (a) Los 5 párrafos más típicos del tópico ----
top5 <- order(pred_df[[tema]], decreasing = TRUE)[1:5]
for (i in top5) {
  cat("\n[", pred_df$gobierno[i], pred_df$anio[i], "] theta =", round(pred_df[[tema]][i], 3), "\n")
  cat(texto_pred[i], "\n")
}
 
## ---- 6.2 (b) 20 párrafos para codificar a mano ----
# Se toman al azar entre los párrafos cuyo tópico dominante es el elegido.
dominante <- colnames(pred_cuentas)[max.col(pred_cuentas, ties.method = "first")]
candidatos <- which(dominante == tema)
length(candidatos)    # si son muy pocos, justifíquelo en el informe
set.seed(1)
muestra <- sample(candidatos, min(20, length(candidatos)))
validacion <- data.frame(id = rownames(pred_cuentas)[muestra],
                         gobierno = pred_df$gobierno[muestra],
                         anio = pred_df$anio[muestra],
                         texto = texto_pred[muestra],
                         codigo = NA)    # 1 = sí, 0 = no, 9 = duda
write.csv(validacion, "validacion_20.csv", row.names = FALSE, fileEncoding = "UTF-8")
# Abra validacion_20.csv, complete la columna "codigo" (1/0/9), guárdelo y corra:
# v <- read.csv("validacion_20.csv", encoding = "UTF-8")
# sum(v$codigo == 1) / sum(v$codigo %in% c(0, 1))      # precisión (los 9 se excluyen)
# table(v$codigo)
 
## ---- 6.3 (c) kwic de una palabra ambigua ----
# Se usan los tokens SIN quitar stopwords, para que el contexto se pueda leer.
toks_kwic <- tokens(corp_par_1990_2026, remove_punct = TRUE)
kw <- kwic(toks_kwic, "seguridad", window = 8)
nrow(kw)
set.seed(1)
kw[sample(nrow(kw), 20), ]    # 20 casos al azar (puede cambiar la palabra)
 
## ---- 6.4 (d) Robustez: cambiar UNA sola decisión (residual = 15 -> 8) ----
# Se reestima el modelo de Allende-Pinochet con residual = 8, se proyecta de nuevo
# sobre las cuentas y se compara el orden de los gobiernos.
set.seed(2026)
slda_rob <- textmodel_seededlda(dtm_allende_pinochet, dictionary = dic, residual = 8)
terms(slda_rob, 15)[, marcos]
 
dtm_cuentas_match <- dfm_match(dtm_1990_2026, featnames(dtm_allende_pinochet))
dtm_cuentas_match <- dfm_subset(dtm_cuentas_match, ntoken(dtm_cuentas_match) > 0)
set.seed(2026)
pred_rob <- textmodel_lda(dtm_cuentas_match, model = slda_rob, update_model = FALSE)$theta
 
pred_rob_df <- as.data.frame(pred_rob[, marcos])
pred_rob_df$gobierno <- docvars(dtm_1990_2026, "gobierno")[match(rownames(pred_rob), docnames(dtm_1990_2026))]
tab_rob <- pred_rob_df |>
  group_by(gobierno) |>
  summarise(across(all_of(marcos), mean)) |>
  mutate(gobierno = factor(gobierno, levels = orden_gob)) |>
  arrange(gobierno)
tab_rob
 
# ¿Se mantiene el orden de los gobiernos? (ranking de 1 = más alto, y correlación de Spearman)
for (m in marcos) {
  cat("\n", m, "\n")
  print(data.frame(gobierno = tab_gob$gobierno,
                   rank_base = rank(-tab_gob[[m]]),
                   rank_rob  = rank(-tab_rob[[m]])))
  cat("Spearman:", round(cor(tab_gob[[m]], tab_rob[[m]], method = "spearman"), 3), "\n")
}
 
 
# ---- PASO 7 · Coocurrencias (Parte 4) ----------------------------------
palabra <- "seguridad"     # >>> CAMBIAR AQUÍ <<< la palabra clave
gov1 <- "Aylwin"           # >>> CAMBIAR AQUÍ <<< los dos gobiernos a comparar
gov2 <- "Boric"
 
vecinos <- list()
fcms <- list()
for (g in c(gov1, gov2)) {
  tk <- tokens_subset(toks_1990_2026_sin, gobierno == g)
  f <- fcm(tk, context = "window", window = 5, tri = FALSE)   # ventana de 5 palabras
  fila <- f[palabra, ]
  v <- setNames(as.numeric(as.matrix(fila)), colnames(fila))
  v <- sort(v, decreasing = TRUE)
  v <- v[names(v) != palabra]
  vecinos[[g]] <- head(v, 30)
  fcms[[g]] <- f
}
 
## ---- 7.1 Tabla de vecinos ----
tabla_vecinos <- data.frame(
  puesto = 1:15,
  v1 = names(vecinos[[gov1]])[1:15], frec1 = as.numeric(vecinos[[gov1]][1:15]),
  v2 = names(vecinos[[gov2]])[1:15], frec2 = as.numeric(vecinos[[gov2]][1:15]))
names(tabla_vecinos) <- c("puesto", gov1, paste0("frec_", gov1), gov2, paste0("frec_", gov2))
tabla_vecinos
 
## ---- 7.2 Dos redes ----
for (g in c(gov1, gov2)) {
  feats <- c(palabra, names(vecinos[[g]]))
  f_sub <- fcm_select(fcms[[g]], pattern = feats, selection = "keep", case_insensitive = FALSE)
  red <- textplot_network(f_sub, min_freq = 0.5)   # suba/baje min_freq si la red sale muy llena o muy vacía
  print(red)
  ggsave(paste0("figuras/red_", gsub(" ", "_", g), ".png"), red, width = 8, height = 7, dpi = 300)
}
 
 
# ---- ANEXO · Reproducibilidad -------------------------------------------
sessionInfo()
dic               # diccionario final completo
# semilla: set.seed(2026) · residual = 15 (robustez: residual = 8)