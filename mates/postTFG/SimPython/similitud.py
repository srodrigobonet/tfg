import warnings, logging
warnings.filterwarnings("ignore",
                        category=UserWarning,
                        module=r"rpy2\.rinterface_lib")
warnings.filterwarnings("ignore",
                        category=UserWarning,
                        module=r"rpy2\.rinterface")

import rpy2.robjects as ro
from   rpy2.robjects.packages import importr
import rpy2.rinterface_lib.callbacks as cb


cb.logger.setLevel(logging.ERROR)
base      = importr("base")

# ------------------------------------------------------------------
# Mandar a R el codigo
# ------------------------------------------------------------------
ro.r("""
suppressPackageStartupMessages({
  library(simona)
  library(igraph)
  library(dplyr)
  setwd("~/Escritorio/SimPython")

  # cargar RDF
  rdf_data1 <- read.csv("cie10.ttl", stringsAsFactors = FALSE)
  rdf_data2 <- read.csv("ciap.ttl", stringsAsFactors = FALSE)
  rdf_data3 <- read.csv("snomed.ttl", stringsAsFactors = FALSE)
  rdf_mapeos <- read.csv("mapeosCiapSnomed.ttl", stringsAsFactors = FALSE)

  # construir igraph con los RDF
  g1 <- graph_from_data_frame(d = rdf_data1[, c("Sujeto", "Objeto")], directed = TRUE)
  E(g1)$label <- rdf_data1$Predicado
  g2 <- graph_from_data_frame(d = rdf_data2[, c("Sujeto", "Objeto")], directed = TRUE)
  E(g2)$label <- rdf_data2$Predicado
  g3 <- graph_from_data_frame(d = rdf_data3[, c("Sujeto", "Objeto")], directed = TRUE)
  E(g3)$label <- rdf_data3$Predicado 
  nodos_cie10 <- names(V(g1))
  nodos_ciap1 <- names(V(g2))
  nodos_snomed <- names(V(g3))

  # crear DAG con el grafo
  ontology1 <- create_ontology_DAG_from_igraph(g1, E(g1)$label)
  ontology2 <- create_ontology_DAG_from_igraph(g2, E(g2)$label)
  ontology3 <- create_ontology_DAG_from_igraph(g3, E(g3)$label)

  # Función de agregación
  get_max_similarity <- function(term_a, term_b, mapeos_df, g1, g2, g3, ont_cie10, ont_snomed, sim_method) {
    
    # auxiliar para determinar la similitud con simona
    sim_ont <- function(term1, term2, ontologia) {
      if (term1 == term2) return(1.0)
      tryCatch({
        if (sim_method == "Sim_Wang_2007") {
          sim_matrix <- term_sim(ontologia, terms = c(term1, term2), method = sim_method,
                                control = list(contribution_factor = c("contains" = 0.75, "include" = 0.8, "hasFindingSite" = 0.8, "laterality" = 0.8, "Isa" = 0.85, "synonym"= 0.9)))
        } else {
          sim_matrix <- term_sim(ontologia, terms = c(term1, term2), method = sim_method)
        }
        if (is.null(sim_matrix) || !is.matrix(sim_matrix) || !all(dim(sim_matrix) == c(2, 2))) return(NA)
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
      mapeos <- get_mapeos(term_ciap, nodos_cie10, "hasICD10")
      
      if (length(mapeos) > 0) {
        similitudes <- sapply(mapeos, function(x) sim_ont(x, term_cie, ont_cie10))
      } else {
        # fallback a SNOMED
        mapeos_ciap_sno <- get_mapeos(term_ciap, nodos_snomed, "hasSNOMED")
        mapeos_cie_sno <- get_mapeos(term_cie, nodos_snomed, "hasSNOMED")
        pares <- expand.grid(mapeos_ciap_sno, mapeos_cie_sno, stringsAsFactors = FALSE)
        similitudes <- apply(pares, 1, function(par) sim_ont(par[1], par[2], ont_snomed))
      }
      
      # CASO 2: CIAP - SNOMED (parecido)
    } else if ((term_a %in% nodos_ciap1 && term_b %in% nodos_snomed) || (term_b %in% nodos_ciap1 && term_a %in% nodos_snomed)) {
      if (term_a %in% nodos_ciap1) { term_ciap <- term_a; term_sno <- term_b }
      else { term_ciap <- term_b; term_sno <- term_a }
      
      mapeos <- get_mapeos(term_ciap, nodos_snomed, "hasSNOMED")
      
      if (length(mapeos) > 0) {
        similitudes <- sapply(mapeos, function(x) sim_ont(x, term_sno, ont_snomed))
      } else {
        # fallback a CIE-10 (nunca ocurre)
        mapeos_ciap_cie <- get_mapeos(term_ciap, nodos_cie10, "hasICD10")
        mapeos_sno_cie <- get_mapeos(term_sno, nodos_cie10, "hasICD10")
        pares <- expand.grid(mapeos_ciap_cie, mapeos_sno_cie, stringsAsFactors = FALSE)
        similitudes <- apply(pares, 1, function(par) sim_ont(par[1], par[2], ont_cie10))
      }
      
      # CASO 3: CIAP - CIAP
    } else if (term_a %in% nodos_ciap1 && term_b %in% nodos_ciap1) {
      term_ciap1 <- term_a; term_ciap2 <- term_b
      
      mapeos1_cie <- get_mapeos(term_ciap1, nodos_cie10, "hasICD10")
      mapeos2_cie <- get_mapeos(term_ciap2, nodos_cie10, "hasICD10")
      mapeos1_sno <- get_mapeos(term_ciap1, nodos_snomed, "hasSNOMED")
      mapeos2_sno <- get_mapeos(term_ciap2, nodos_snomed, "hasSNOMED")
      
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
    return(sim_final)
    
  }
      
  get_sim_R <- function(code1, code2,
                      metodo = "Sim_Wang_2007") {
      get_max_similarity(code1, code2,
                          rdf_mapeos,
                          g1, g2, g3,
                          ontology1, ontology3,
                          metodo)
  }

  get_mapeos<- function(term, target, predicado) {
    rdf_mapeos %>%
      filter(Sujeto == term, Predicado == predicado) %>%
      pull(Objeto) %>%
      unique()
  }
     
  get_mapeos_R <- function(term, target_vocab) {
    if (target_vocab == "CIAP1") {
      target_nodes <- nodos_ciap1
      predicado <- "hasCIAP1"
    } else if (target_vocab == "CIE10") {
      target_nodes <- nodos_cie10
      predicado <- "hasICD10"
    } else if (target_vocab == "SNOMED") {
      target_nodes <- nodos_snomed
      predicado <- "hasSNOMED"
    } else {
      warning("Vocabulario desconocido.")
      return(character(0))
    }
    
    get_mapeos(term, target_nodes, predicado)
  }
     
})
""")

# ------------------------------------------------------------------
# Llamada desde Python
# ------------------------------------------------------------------
get_sim = ro.globalenv['get_sim_R']
get_map = ro.globalenv['get_mapeos_R']

similarity = get_sim("P76", "35489007")  # códigos a comparar
sim_float  = float(similarity[0])
print(f"Similitud:  {sim_float:.4f}")

mapeos = get_map("P76", "SNOMED")
print("Mapeos:", list(mapeos))
