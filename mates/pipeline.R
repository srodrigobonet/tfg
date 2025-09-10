# cargar librerías
library(simona)
library(igraph)

# 1. MÉTODOS PARA CIE-10--------------------------------------------------------

# cargar CIE-10 de RDF
rdf_data1 <- read.csv("capituloFcie10.ttl", stringsAsFactors = FALSE)

term1 <- "F17"
term2 <- "F60"

# construir igraph con los RDF
g1 <- graph_from_data_frame(d = rdf_data1[, c("Sujeto", "Objeto")], directed = TRUE)
E(g1)$label <- rdf_data1$Predicado

# visualizarlo
l <- layout_with_fr(g1)
plot(g1, 
     layout = l, 
     vertex.size = 20, 
     vertex.label.cex = 0.8, 
     vertex.color = "skyblue", 
     vertex.frame.color = "black", 
     edge.arrow.size = 0.6, 
     edge.label = E(g1)$Predicado, 
     edge.label.cex = 0.8, 
     edge.color = "gray50", 
     main = "Visualización RDF con igraph")

# Métodos basados en IC (necesitan annotation en create_ontology_DAG)
  
# Métodos basados en caminos
  
  # crear DAG con el grafo
  ontology1 <- create_ontology_DAG_from_igraph(g1, E(g1)$label)
  dag_circular_viz(ontology1)
  
  # Rada
  sim_rada <- term_sim(ontology1, terms = c(term1, term2), method = "Sim_Rada_1989")
  
  # Resnik-edge
  sim_resnik_edge <- term_sim(ontology1, terms = c(term1, term2), method = "Sim_Resnik_edge_2005")
  
  # Leacock-Chodorow
  sim_lc <- term_sim(ontology1, terms = c(term1, term2), method = "Sim_Leocock_1998")
  
  
# Método Wang 
  sim_wang <- term_sim(ontology1, terms = c(term1, term2), method = "Sim_Wang_2007",
         control = list(contribution_factor = c("contains" = 0.75, "include" = 0.8, "Isa" = 0.85, "synonym"= 0.9)))

  
# Gráfico
  # similarity_scores <- c("Resnik" = sim_resnik[1, 2], "Lin" = sim_lin[1, 2], "JC" = sim_jc[1, 2], "Rada" = sim_rada[1, 2], "LC" = sim_lc[1, 2], "Resnik-edge" = sim_resnik_edge[1, 2], "Wang" = sim_wang[1, 2])
  similarity_scores <- c("Rada" = sim_rada[1, 2], "Resnik-edge" = sim_resnik_edge[1, 2], "LC" = sim_lc[1, 2], "Wang" = sim_wang[1, 2])
  similarity_scores <- similarity_scores[is.finite(similarity_scores)] 
  
  if (length(similarity_scores) > 0) {
    bar_colors <- RColorBrewer::brewer.pal(n = length(similarity_scores), name = "Pastel1")
    bar_positions <- barplot(
      height = similarity_scores,
      main = paste("Similitud semántica entre", basename(term1), "y", basename(term2)),
      xlab = "Método",
      ylab = "Similitud [0, 1]",
      ylim = c(0, max(1, max(similarity_scores, na.rm = TRUE)) * 1.15),
      col = bar_colors,
      las = 1,
      cex.names = 0.8
    )
    text(
      x = bar_positions,
      y = similarity_scores,
      labels = round(similarity_scores, 3),
      pos = 3,
      cex = 1,
      col = "black"
    )
  }
  
# 2. MÉTODO AGREGACIÓN DE CIAP-1 y SNOMED---------------------------------------

library(dplyr) # función pull

# cargar CIAP-1, SNOMED y los mapeos de RDF
rdf_data2 <- read.csv("capituloPciap.ttl", stringsAsFactors = FALSE)
rdf_data3 <- read.csv("SnomedRel.ttl", stringsAsFactors = FALSE)
rdf_mapeos <- read.csv("mapeosCiapCieSnomed.ttl", stringsAsFactors = FALSE)

# construir igraph con ciap y snomed
g2 <- graph_from_data_frame(d = rdf_data2[, c("Sujeto", "Objeto")], directed = TRUE)
E(g2)$label <- rdf_data2$Predicado
g3 <- graph_from_data_frame(d = rdf_data3[, c("Sujeto", "Objeto")], directed = TRUE)
E(g3)$label <- rdf_data3$Predicado

# crear DAG con ciap y snomed
ontology2 <- create_ontology_DAG_from_igraph(g2, E(g2)$label)
dag_circular_viz(ontology2)
ontology3 <- create_ontology_DAG_from_igraph(g3, E(g3)$label)
dag_circular_viz(ontology3)

# Función de agregación
get_max_similarity <- function(term_a, term_b, mapeos_df, g1, g2, g3, ont_cie10, ont_snomed, sim_method) {
  
  # auxiliar para determinar la similitud con simona
  sim_ont <- function(term1, term2, ontologia) {
    if (term1 == term2) return(1.0)
    tryCatch({
      if (sim_method == "Sim_Wang_2007") {
        sim_matrix <- term_sim(ontologia, terms = c(term1, term2), method = sim_method,
            control = list(contribution_factor = c("contains" = 0.75, "include" = 0.8, "Isa" = 0.85, "synonym"= 0.9)))
      } else {
        sim_matrix <- term_sim(ontologia, terms = c(term1, term2), method = sim_method)
      }
      if (is.null(sim_matrix) || !is.matrix(sim_matrix) || !all(dim(sim_matrix) == c(2, 2))) return(NA)
      cat(paste("      Similitud entre", term1, "y", term2, ": ", sim_matrix[1, 2], "\n"))
      sim_matrix[1, 2]
    }, error = function(e) {
      message("Error al calcular term_sim: ", e$message)
      return(NA)
    })
  }
  
  # auxiliar para obtener mapeos
  get_mapeos <- function(term, target, predicado) {
    mapeos_df %>%
      filter(Sujeto == term, Predicado == predicado) %>%
      pull(Objeto) %>%
      unique()
  }
  
  
  nodos_cie10 <- names(V(g1))
  nodos_ciap1 <- names(V(g2))
  nodos_snomed <- names(V(g3))
  
  # CASO 1: CIAP - CIE10
  if ((term_a %in% nodos_ciap1 && term_b %in% nodos_cie10) || (term_b %in% nodos_ciap1 && term_a %in% nodos_cie10)) {
    if (term_a %in% nodos_ciap1) { term_ciap <- term_a; term_cie <- term_b }
    else { term_ciap <- term_b; term_cie <- term_a }
    
    cat(paste("Calculando similitud entre", term_ciap, "(CIAP-1) y", term_cie, "(CIE-10)\n"))
    mapeos <- get_mapeos(term_ciap, nodos_cie10, "hasICD10")
    cat("    Mapeos a CIE-10:", mapeos, "\n")
    
    if (length(mapeos) > 0) {
      similitudes <- sapply(mapeos, function(x) sim_ont(x, term_cie, ont_cie10))
    } else {
      # fallback a SNOMED
      mapeos_ciap_sno <- get_mapeos(term_ciap, nodos_snomed, "hasSNOMED")
      mapeos_cie_sno <- get_mapeos(term_cie, nodos_snomed, "hasSNOMED")
      cat("    Fallback a SNOMED. Mapeos del CIAP:", mapeos_ciap_sno, " | Mapeos del CIE:", mapeos_cie_sno, "\n")
      pares <- expand.grid(mapeos_ciap_sno, mapeos_cie_sno, stringsAsFactors = FALSE)
      similitudes <- apply(pares, 1, function(par) sim_ont(par[1], par[2], ont_snomed))
    }
    
  # CASO 2: CIAP - SNOMED (parecido)
  } else if ((term_a %in% nodos_ciap1 && term_b %in% nodos_snomed) || (term_b %in% nodos_ciap1 && term_a %in% nodos_snomed)) {
    if (term_a %in% nodos_ciap1) { term_ciap <- term_a; term_sno <- term_b }
    else { term_ciap <- term_b; term_sno <- term_a }
    
    cat(paste("Calculando similitud entre", term_ciap, "(CIAP-1) y", term_sno, "(SNOMED)\n"))
    mapeos <- get_mapeos(term_ciap, nodos_snomed, "hasSNOMED")
    cat("    Mapeos a SNOMED:", mapeos, "\n")
    
    if (length(mapeos) > 0) {
      similitudes <- sapply(mapeos, function(x) sim_ont(x, term_sno, ont_snomed))
    } else {
      # fallback a CIE-10 (nunca ocurre)
      mapeos_ciap_cie <- get_mapeos(term_ciap, nodos_cie10, "hasICD10")
      mapeos_sno_cie <- get_mapeos(term_sno, nodos_cie10, "hasICD10")
      cat("    Fallback a CIE-10. Mapeos del CIAP:", mapeos_ciap_cie, " | Mapeos del SNOMED:", mapeos_sno_cie, "\n")
      pares <- expand.grid(mapeos_ciap_cie, mapeos_sno_cie, stringsAsFactors = FALSE)
      similitudes <- apply(pares, 1, function(par) sim_ont(par[1], par[2], ont_cie10))
    }
    
  # CASO 3: CIAP - CIAP
  } else if (term_a %in% nodos_ciap1 && term_b %in% nodos_ciap1) {
    term_ciap1 <- term_a; term_ciap2 <- term_b
    
    cat(paste("  Calculando similitud entre", term_ciap1, "(CIAP-1) y", term_ciap2, "(CIAP-1)\n"))
    mapeos1_cie <- get_mapeos(term_ciap1, nodos_cie10, "hasICD10")
    mapeos2_cie <- get_mapeos(term_ciap2, nodos_cie10, "hasICD10")
    mapeos1_sno <- get_mapeos(term_ciap1, nodos_snomed, "hasSNOMED")
    mapeos2_sno <- get_mapeos(term_ciap2, nodos_snomed, "hasSNOMED")
    
    cat("    Mapeos a CIE10:", mapeos1_cie, mapeos2_cie, "\n")
    cat("    Mapeos a SNOMED:", mapeos1_sno, mapeos2_sno, "\n")
    
    pares_cie <- expand.grid(mapeos1_cie, mapeos2_cie, stringsAsFactors = FALSE)
    pares_sno <- expand.grid(mapeos1_sno, mapeos2_sno, stringsAsFactors = FALSE)
    
    similitudes_cie <- apply(pares_cie, 1, function(par) sim_ont(par[1], par[2], ont_cie10))
    similitudes_sno <- apply(pares_sno, 1, function(par) sim_ont(par[1], par[2], ont_snomed))
    
    similitudes <- c(similitudes_cie, similitudes_sno)
    
  } else {
    warning("No se pudo determinar un término.")
    return(NA)
  }
  
  similitudes_validas <- similitudes[!is.na(similitudes)]
  if (length(similitudes_validas) == 0) return(0)
  
  # agregar usando max (mapeos son 0.95)
  sim_final <- 0.95*max(similitudes_validas)
  cat(paste("Similitud máxima:", round(sim_final, 4), "\n"))
  return(sim_final)
 
}

metodo_interno = "Sim_Wang_2007"
term_a = "P76"; term_b = "P06"
sim <- get_max_similarity(term_a, term_b, rdf_mapeos, g1, g2, g3, ontology1, ontology3, metodo_interno)


# 3. EVALUACIÓN ----------------------------------------------------------------

# Cargar librería
library(ggplot2)

# Cargar el dataset de evaluación
ejemplos <- read.csv("ejemplosAP.csv", stringsAsFactors = FALSE, sep = "|",)

# Filtrar filas
casos_eval <- ejemplos %>%
  filter(similitud == 1, code_ap != code_llm) %>%
  distinct()

# Calcular la similitud semántica para cada par (code_ap, code_llm)
similitudes_semanticas <- mapply(function(c1, c2) {
  get_max_similarity(c1, c2, rdf_mapeos, g1, g2, g3, ontology1, ontology3, metodo_interno)
}, casos_eval$code_ap, casos_eval$code_llm)

# Añadir columna y eliminar filas incoherentes
casos_eval$sim_wang <- similitudes_semanticas
casos_eval$sim_wang <- as.numeric(casos_eval$sim_wang)
casos_eval <- casos_eval %>%
  filter(!is.na(sim_wang), sim_wang != 0)

# Gráfico de barras
casos_eval$pair_id <- paste0(casos_eval$code_ap, ", ", casos_eval$code_llm, " [", seq_len(nrow(casos_eval)), "]")
casos_eval$pair_label <- paste0(casos_eval$code_ap, ", ", casos_eval$code_llm)
casos_eval$pair_id <- factor(casos_eval$pair_id, levels = casos_eval$pair_id)

ggplot(casos_eval, aes(x = pair_id, y = sim_wang)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(sim_wang, 2)), vjust = -0.3, size = 4) +
  scale_x_discrete(labels = casos_eval$pair_label) +
  labs(
    title = "Similitud semántica entre códigos distintos de CIAP-1",
    x = "(code_ap, code_llm)",
    y = "Similitud [0,1]"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
