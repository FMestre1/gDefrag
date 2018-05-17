plotgraph <-
function(nodes, edges, land_polyg, scale_nodes = 10, col_nodes = "darkblue", col_labels = "darkblue", cex_labels = 1, main = "Graph"){#GRAFICAR
  #Arguments:
  #node_T - Nodes data frame; ## NOW nodes - output of function 1
  #edge_P - Edges data frame with priority valuese; ## NOW edges - output of function 2 or 3
  #land_polyg - Road shapefile (polygons);

  node_T <- nodes@data  # new
  edge_P <- edges@data  # new

  #LINES  # no longer necessary, as input edges are spatial lines
  #out_lines <- list()
  #for(i in 1:nrow(edge_P)){
  #  l0 <- edge_P[i,]
  #  xA <- as.numeric(l0[4])
  #  yA <- as.numeric(l0[5])
  #  xB <- as.numeric(l0[6])
  #  yB <- as.numeric(l0[7])
  #  pts <- matrix(c(xA, yA, xB, yB), nrow = 2, ncol = 2, byrow = TRUE)
  #  l1 <- Line(pts)
  #  l2 <- Lines(l1, ID = rownames(edge_P)[i])
  #  out_lines[[i]] <- l2
  #}
  #l3 <- SpatialLines(out_lines)

  #Edge values
  #Isto era para usar de algum modo o value_norm para
  #por as espessuras nas linhas, usando o lwd nao consigo fazer
  #varia a espessura da linha.
  if ("priorization" %in% colnames(edge_P)) { # new
     value_width <- edge_P[, "priorization"]
 value_norm <- (value_width - min(value_width)) / (max(value_width) - min(value_width)) * 10
 #value_norm <- (value_width - min(value_width)) / (max(value_width) - min(value_width)) 
 }
#else value_width <- 1  # new
  else value_norm <- 5  # new
  #if (!("priorization" %in% colnames(edge_P))) value_norm <- 1

  #NODES
  # [passei os nodes la de cima aqui mais para baixo, para os nodes ficarem por cima, sem ser tapados pelas linhas]

  #nodes <- node_T$nodes
  #plot(land_polyg)

  #juntar dados dos nodes aos poligonos, para os colorir segundo habitat:
  #land_polyg@data <- data.frame(land_polyg@data, nodes@data[match(land_polyg@data[, "ID"], nodes@data[, "ID"]), ])
  land_polyg@data <- data.frame(land_polyg@data, nodes@data)

  plot(land_polyg, col="lightblue", main = main)
  plot(edges, lwd=3, col = gray(1-(value_norm/10)), add = TRUE)

#plot(land_polyg, col = grey(1-(land_polyg@data$pol_value / max(land_polyg@data$pol_value))), main = main)
  #plot(edges, lwd=3, col = heat.colors(10)[10-value_norm], add = TRUE)
#pal <- colorRampPalette(c("yellow","red"))
#grd <- pal(100)
#col_vect <- grd[100-edges@data$priorization]
#plot(edges, lwd=3, col = col_vect, add = TRUE)
  #plot(edges, lwd=3, col = grey(1-value_norm/10), add = TRUE)
  
  symbols(node_T[, "X"], node_T[, "Y"], circles = sqrt(node_T[, "pol_value"] / pi) * scale_nodes, fg = col_nodes, add = TRUE, inches = FALSE)
  text(x = coordinates(nodes), labels = as.character(node_T$node_ID), cex = cex_labels, col = col_labels)
}
