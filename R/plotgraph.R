plotgraph <-
function(nodes, edges, land_polyg, scale_nodes = 10, col_nodes = "darkblue", col_labels = "darkblue", cex_labels = 1, main = "Graph"){#GRAFICAR
  
  #Arguments:
  #nodes - output of function node.creation
  #edges - output of function prioritize
  #land_polyg - land_polyg given to node.creation
  #scale_nodes = 10
  #col_nodes = "darkblue"
  #col_labels = "darkblue"
  #cex_labels = 1
  #main = "Graph" - Title
  
  if(!(crs(land_polyg) == crs(edges) & crs(edges) == crs(land_polyg))) stop("Vectors provided not in the same crs!")
  
  node_T <- data.frame(terra::crds(nodes), nodes)
  edge_P <- data.frame(edges)  #16-11-2019 - changes in sp and rgdal

  
  if ("priorization" %in% colnames(edge_P)) { # new
 value_width <- edge_P[, "priorization"]
 value_norm <- (value_width - min(value_width)) / (max(value_width) - min(value_width)) * 10
 } else value_norm <- 5  # new

#Plot
  terra::plot(land_polyg, col="lightblue", main = main)
  terra::plot(edges, lwd=3, col = gray(1-(value_norm/10)), add = TRUE)

  symbols(node_T[, "x"], node_T[, "y"], circles = sqrt(node_T[, "pol_value"] / pi) * scale_nodes, fg = col_nodes, add = TRUE, inches = FALSE)
  text(x = terra::crds(nodes), labels = as.character(node_T$node_ID), cex = cex_labels, col = col_labels)
}
