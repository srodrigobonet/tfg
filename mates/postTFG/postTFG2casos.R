library(simona)
library(clue)
library(igraph)
library(readxl)
library(dplyr)
library(ggplot2)
setwd("~/Escritorio/tfg mates/R/postTFG")



# cargamos la ontologia
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



# cargamos las historias clínicas de dos pacienetes
df <- read_excel("2casos.xlsx", skip = 2, .name_repair = "minimal")

# diagnóstico principal
diag_A <- colnames(df)[4]
diag_B <- colnames(df)[5]

colnames(df)[4:5] <- c("Paciente_A", "Paciente_B")

# comorbilidades y sus puntuaciones
comorbs_A <- df %>%
  filter(Paciente_A == "Si") %>%
  select(SNOMED, Comorbilidades)
comorbs_A$SNOMED <- as.character(comorbs_A$SNOMED)

comorbs_B <- df %>%
  filter(Paciente_B == "Si") %>%
  select(SNOMED, Comorbilidades)
comorbs_B$SNOMED <- as.character(comorbs_B$SNOMED)



# similitud entre diagnósticos principales
sim_principal <- get_similarity(diag_A, diag_B, ontology3, g3)

# matriz de similitud entre comorbilidades
sim_matrix <- matrix(0, nrow = nrow(comorbs_A), ncol = nrow(comorbs_B))
rownames(sim_matrix) <- comorbs_A$SNOMED
colnames(sim_matrix) <- comorbs_B$SNOMED

for (i in 1:nrow(comorbs_A)) {
  for (j in 1:nrow(comorbs_B)) {
    sim_matrix[i, j] <- get_similarity(comorbs_A$SNOMED[i], comorbs_B$SNOMED[j], ontology3, g3)
  }
}

# emparejamiento óptimo
matches <- sapply(1:ncol(sim_matrix), function(j) {
  best_i <- which.max(sim_matrix[, j])
  return(best_i)
})

# df con resultados añadiendo concept_name
emparejamientos <- data.frame(
  cod_A = rownames(sim_matrix)[matches],
  cod_B = colnames(sim_matrix),
  similitud = mapply(function(i, j) sim_matrix[i, j], matches, 1:length(matches))
)

emparejamientos <- emparejamientos %>%
  mutate(
    nombre_A = sapply(cod_A, function(c) df$Comorbilidades[df$SNOMED == c][1]),
    nombre_B = sapply(cod_B, function(c) df$Comorbilidades[df$SNOMED == c][1]),
  )

# resultado de similitud entre diagnósticos
sim_media <- mean(emparejamientos$similitud)



# visualización
ggplot(emparejamientos, aes(x = nombre_A, y = nombre_B, fill = similitud)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(similitud, 2)), size = 3, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Similitud entre comorbilidades de dos pacientes",
    subtitle = paste0(
      "Similitud entre diagnósticos principales: ", round(sim_principal, 3),
      "   |   Similitud media por comorbilidades: ", round(sim_media, 3)
    ),
    x = paste("Comorbilidades de", diag_A, "(Nefropatía hipertensiva crónica)"),
    y = paste("Comorbilidades de", diag_B),
    fill = "Similitud"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(t = 15))
  )
