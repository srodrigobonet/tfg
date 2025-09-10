library(simona)
library(clue)
library(igraph)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
setwd("~/Escritorio/tfg mates/R/postTFG")



# cargamos la ontologia ---------------------------------------------------------------------
rdf_data3 <- read.csv("snomed.ttl", stringsAsFactors = FALSE)
g3 <- graph_from_data_frame(d = rdf_data3[, c("Sujeto", "Objeto")], directed = TRUE)
E(g3)$label <- rdf_data3$Predicado
ontology3 <- create_ontology_DAG_from_igraph(g3, E(g3)$label)

# función de similitud
get_similarity <- function(t1, t2, ontology, graph, method = "Sim_Wang_2007") {
  if (t1 == t2) return(1.0)
  tryCatch({
    sim_matrix <- if (method == "Sim_Wang_2007") {
      term_sim(ontology, terms = c(t1, t2), method = method,
               control = list(contribution_factor = c("hasFindingSite" = 0.8, "Isa" = 0.85)))
    } else {
      term_sim(ontology, terms = c(t1, t2), method = method)
    }
    if (is.null(sim_matrix) || !is.matrix(sim_matrix)) return(0)
    return(sim_matrix[1, 2])
  }, error = function(e) {
    message("Error con términos: ", t1, " y ", t2)
    return(0)
  })
}

# cargamos las cormobilidades
comorb <- read_excel("10casos.xlsx", skip = 4, .name_repair = "minimal")
comorb <- comorb %>%
  select(Comorbilidades, SNOMED)
comorb <- comorb %>% mutate(index = row_number())

# cargamos la codificacion erp-snomed
codigos_map <- read_excel("codigosCasos.xlsx", , skip = 1)
codigos_map <- codigos_map %>%
  rename(
    erp = "ERA\r\nPRD Code\r\n",
    SNOMED_CT_ID = "SNOMED CT concept identifier" ,
    SNOMED_CT_Name = "SNOMED CT Fully Specified Name"
  )
codigos_map <- codigos_map %>%
  select(erp, SNOMED_CT_ID, SNOMED_CT_Name)

# cargamos las historias clínicas de los pacientes y mapeamos a snomed
df_casos <- read.csv("+casos.csv", stringsAsFactors = FALSE)
df_limpio <- df_casos %>%
  filter(!is.na(erp) & erp != "0") %>%
  select(erp, id_paciente, starts_with("Comorbilidad___"))
df_limpio$erp <- as.character(df_limpio$erp)
df_limpio <- df_limpio %>%
  left_join(codigos_map %>% select(erp, SNOMED_CT_ID), by = "erp")

# construimos df
comorb_cols <- grep("^comorbilidad___\\d+$", names(df_limpio), value = TRUE)
snomed_names <- comorb %>%
  arrange(index) %>%
  slice(1:length(comorb_cols)) %>%
  pull(SNOMED)
names(df_limpio)[names(df_limpio) %in% comorb_cols] <- snomed_names
df_limpio <- df_limpio %>%
  select(SNOMED_CT_ID, everything()) %>%  
  select(-erp)                            
# -------------------------------------------------------------------------------------------


# heatmap de similitud entre comorbilidades -------------------------------------------------
comorb_cols <- setdiff(colnames(df_limpio), c("SNOMED_CT_ID", "id_paciente"))

# matriz de similitud entre comorbilidades
comorb_ids <- as.character(comorb_cols)
n <- length(comorb_ids)

sim_matrix <- matrix(0, nrow = n, ncol = n)
rownames(sim_matrix) <- colnames(sim_matrix) <- comorb_ids

for (i in 1:n) {
  for (j in 1:n) {
    sim_matrix[i, j] <- get_similarity(comorb_ids[i], comorb_ids[j], ontology3, g3)
  }
}

sim_comorb <- sim_matrix

# cambiar a nombre de comorb
nombres_legibles <- comorb %>% filter(SNOMED %in% comorb_ids)
comorb_names <- setNames(nombres_legibles$Comorbilidades, nombres_legibles$SNOMED)
rownames(sim_matrix) <- colnames(sim_matrix) <- comorb_names[rownames(sim_matrix)]

# Heatmap
sim_df <- melt(sim_matrix)
colnames(sim_df) <- c("Comorbilidad_A", "Comorbilidad_B", "Similitud")

ggplot(sim_df, aes(x = Comorbilidad_A, y = Comorbilidad_B, fill = Similitud)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Similitud, 2)), size = 3, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Similitud semántica entre todas las comorbilidades",
    x = "Comorbilidad A",
    y = "Comorbilidad B",
    fill = "Similitud"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# -------------------------------------------------------------------------------------------


# diagnósticos principales ------------------------------------------------------------------
diag1 <- "10751000203109"
diag2 <- "28728008"

# pacientes con ese diagnóstico
grupo_diag1 <- df_limpio %>% filter(SNOMED_CT_ID == diag1)
pacientes1 <- grupo_diag1$id_paciente
grupo_diag2 <- df_limpio %>% filter(SNOMED_CT_ID == diag2)
pacientes2 <- grupo_diag2$id_paciente

# matriz de similitud entre pacientes
sim_pacientes <- matrix(NA, nrow = length(pacientes1), ncol = length(pacientes2))
rownames(sim_pacientes) <- paste0(pacientes1)
colnames(sim_pacientes) <- paste0(pacientes2)

# funcion de sim con emparejamiento 
sim_pacientes_max_matching <- function(paciente_a, paciente_b, grupo_diag1, grupo_diag2, sim_comorb, comorb_cols) {
  comorb_i <- comorb_cols[which(grupo_diag1[paciente_a, comorb_cols] == 1)]
  comorb_j <- comorb_cols[which(grupo_diag2[paciente_b, comorb_cols] == 1)]
  
  if (length(comorb_i) == 0 || length(comorb_j) == 0) {
    return(NA)
  }
  
  # cada comorbilidad de i busca la más parecida en j
  sim_i_to_j <- sapply(comorb_i, function(a) {
    max(sapply(comorb_j, function(b) sim_comorb[as.character(a), as.character(b)]), na.rm = TRUE)
  })
  
  # cada comorbilidad de j busca la más parecida en i
  sim_j_to_i <- sapply(comorb_j, function(b) {
    max(sapply(comorb_i, function(a) sim_comorb[as.character(a), as.character(b)]), na.rm = TRUE)
  })
  
  return(mean(c(sim_i_to_j, sim_j_to_i), na.rm = TRUE))
}

# media de sim entre comorb de los dos pacientes
for (i in seq_along(pacientes1)) {
  for (j in seq_along(pacientes2)) {
    sim_pacientes[i, j] <- sim_pacientes_max_matching(i, j, grupo_diag1, grupo_diag2, sim_comorb, comorb_cols)
  }
}


# Heatmap de similitud por paciente
nombre_diag1 <- codigos_map %>%
  filter(SNOMED_CT_ID == diag1) %>%
  pull(SNOMED_CT_Name) %>%
  unique() %>%
  .[1]
nombre_diag2 <- codigos_map %>%
  filter(SNOMED_CT_ID == diag2) %>%
  pull(SNOMED_CT_Name) %>%
  unique() %>%
  .[1]
sim_pacientes_df <- melt(sim_pacientes, varnames = c("Paciente_A", "Paciente_B"), value.name = "Similitud")

ggplot(sim_pacientes_df, aes(x = Paciente_A, y = Paciente_B, fill = Similitud)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(Similitud), "", round(Similitud, 2))), size = 3, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
  labs(
    title = paste("Similitud media (por emparejamiento) entre pacientes"),
    x = paste("Paciente A con", nombre_diag1),
    y = paste("Paciente B con", nombre_diag2),
    fill = "Similitud"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# -------------------------------------------------------------------------------------------


# heatmap de casos mas bajos, altos e intermedios -------------------------------------------

sim_pacientes_df$Paciente_A <- as.character(sim_pacientes_df$Paciente_A)
sim_pacientes_df$Paciente_B <- as.character(sim_pacientes_df$Paciente_B)
sim_ordenadas <- sim_pacientes_df %>%
  filter(!is.na(Similitud)) %>%
  arrange(Similitud)

# detectar pares con máxima, mínima, media similitud
max_pair <- sim_pacientes_df %>%
  filter(!is.na(Similitud)) %>%
  arrange(desc(Similitud)) %>%
  slice(1)

min_pair <- sim_pacientes_df %>%
  filter(!is.na(Similitud)) %>%
  arrange(Similitud) %>%
  slice(1)

fila_intermedia <- sim_ordenadas[ceiling(nrow(sim_ordenadas)/2), ]

# extraer comorbilidades por paciente id
get_comorbilidades_activas <- function(df, id_paciente, comorb_cols) {
  df %>%
    filter(id_paciente == !!id_paciente) %>%
    select(all_of(comorb_cols)) %>%
    pivot_longer(cols = everything(), names_to = "Comorbilidad", values_to = "Presente") %>%
    filter(Presente == 1) %>%
    pull(Comorbilidad)
}

pac_max_a <- max_pair$Paciente_A
pac_max_b <- max_pair$Paciente_B
max <- max_pair$Similitud
comorb_max_a <- get_comorbilidades_activas(grupo_diag1, pac_max_a, comorb_cols)
comorb_max_b <- get_comorbilidades_activas(grupo_diag2, pac_max_b, comorb_cols)

pac_min_a <- min_pair$Paciente_A
pac_min_b <- min_pair$Paciente_B
min <- min_pair$Similitud
comorb_min_a <- get_comorbilidades_activas(grupo_diag1, pac_min_a, comorb_cols)
comorb_min_b <- get_comorbilidades_activas(grupo_diag2, pac_min_b, comorb_cols)

pac_med_a <- fila_intermedia$Paciente_A
pac_med_b <- fila_intermedia$Paciente_B
med <- fila_intermedia$Similitud
comorb_med_a <- get_comorbilidades_activas(grupo_diag1, pac_med_a, comorb_cols)
comorb_med_b <- get_comorbilidades_activas(grupo_diag2, pac_med_b, comorb_cols)

# heatmap similitud entre comorbilidades de pacientes
graficar_similitud_comorb <- function(pac_a, comorb_a, pac_b, comorb_b, sim_matrix, nombre_diag1, nombre_diag2, title = "", color_low = "", color_high = "") {
  df <- expand.grid(Comorbilidad_A = comorb_a, Comorbilidad_B = comorb_b)
  df$Similitud <- mapply(function(a, b) sim_matrix[a, b], df$Comorbilidad_A, df$Comorbilidad_B)
  
  # reemplazar SNOMED con nombres
  df$Comorbilidad_A <- comorb_names[df$Comorbilidad_A]
  df$Comorbilidad_B <- comorb_names[df$Comorbilidad_B]
  
  ggplot(df, aes(x = Comorbilidad_A, y = Comorbilidad_B, fill = Similitud)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Similitud, 2)), size = 3, color = "black") +
    scale_fill_gradient(low = color_low, high = color_high, na.value = "grey90") +
    labs(
      title = title,
      x = paste0("Paciente ", pac_a, " con ", nombre_diag1),
      y = paste0("Paciente ", pac_b, " con ", nombre_diag2),
      fill = "Similitud"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

graficar_similitud_comorb(
  pac_max_a, comorb_max_a, pac_max_b, comorb_max_b, sim_comorb, nombre_diag1, nombre_diag2,
  title = paste("Similitud entre comorbilidades (máxima similitud entre pacientes: ", round(max,2), ")"),
  color_low = "palegreen1", color_high = "palegreen4"
)

graficar_similitud_comorb(
  pac_min_a, comorb_min_a, pac_min_b, comorb_min_b, sim_comorb, nombre_diag1, nombre_diag2,
  title = paste("Similitud entre comorbilidades (mínima similitud entre pacientes: ", round(min,2), ")"),
  color_low = "lightyellow", color_high = "brown4"
)

graficar_similitud_comorb(
  pac_med_a, comorb_med_a, pac_med_b, comorb_med_b, sim_comorb, nombre_diag1, nombre_diag2,
  title = paste("Similitud entre comorbilidades (similitud intermedia entre pacientes: ", round(med,2), ")"),
  color_low = "wheat", color_high = "saddlebrown"
)
# -------------------------------------------------------------------------------------------