edge.creation <-
function(nodes, land_polyg, min_length = 0, min_pol_area = 0, plot = TRUE, shape = FALSE, shape_name_edges = "shape_edges"){#FUNCTION 2

autoID <- NULL
road_length <- NULL
node_A <- NULL
node_B <- NULL


  #Arguments:
  #node_T - Nodes data frame - DEPRECATED
  #nodes - now output of function 1, containing 'nodes' (spatial object, with node_T as data attributes table) and 'adjacents' (table);
  #road_L - Road line shapefile - DEPRECATED
  # min_length: minimum length of road (in the input map projection units) for an edge to be considered for crossing it
  # min_pol_area: (NEW ARGUMENT) minimum area (in the input map projection units) for a polygon to generate edges
  # plot: (NEW ARGUMENT) whether to produce a plot of the edges overlaid to the road polygons

  #if (!(all.equal(nodes@proj4string@projargs, land_polyg@proj4string@projargs)))##16-11-2019 - changes in sp and rgdal
  if (!(all.equal(slot(slot(nodes, "proj4string"), "projargs"), slot(slot(land_polyg, "proj4string"), "projargs"))))  ##16-11-2019 - changes in sp and rgdal
    stop("Input maps have different CRS.")

  #if (missing(road_L)) {  # NEW
    message("Extracting road lines from polygon borders...")
    borders <- gDifference(as(land_polyg, "SpatialLines"), as(gUnaryUnion(land_polyg), "SpatialLines"), byid = TRUE)
    road_L <- disaggregate(gLineMerge(gUnion(borders, borders)))
  #} else {
  #  if (!(class(road_L) %in% c('SpatialLines', 'SpatialLinesDataFrame')))
  #    stop("'road_L' must be of class 'SpatialLines' or 'SpatialLinesDataFrame'.")
  #}  # end check road_L

  message("Creating edges...")
  #Build table
  #node_T <- nodes@data  # attribute table of new spatial nodes##16-11-2019 - changes in sp and rgdal
  node_T <- slot(nodes, "data")##16-11-2019 - changes in sp and rgdal
  #adj <- nodes$adjacents

  adj <- as.data.frame(gTouches(land_polyg, byid = TRUE))  # transferred from function 1
  rownames(adj) <- colnames(adj) <- as.character(1:length(land_polyg))  # transferred from function 1
  adj <- upper.tri(adj, diag = FALSE) * adj
  adj_names <- rownames(adj)
  adj <- as.matrix(adj)
  adj <- array(as.logical(adj), dim(adj))
  rownames(adj)<- adj_names
  colnames(adj) <- adj_names
  ID1 <- which(adj,TRUE)
  ID1[,1] <- as.numeric(rownames(adj)[ID1[,1]])
  ID1[,2] <- as.numeric(colnames(adj)[ID1[,2]])

  # remove adjacents that touch only at a road crossing (point), not a road (line): NEW
  rownames(ID1) <- as.character(1:nrow(ID1))
  for (i in rownames(ID1)) {
    P1 <- land_polyg[ID1[i, 1], ]
    P2 <- land_polyg[ID1[i, 2], ]
    if (!inherits(gIntersection(P1, P2, byid = TRUE), "SpatialLines"))
      ID1 <- ID1[-which(rownames(ID1) == i), ]
  }  # end for i

  #Defining distances
  dist1 <- dist(node_T)
  dist1 <- as.data.frame(as.matrix(dist1))
  rownames(dist1) <- adj_names
  colnames(dist1) <- adj_names

  d2 <- rep(NA,nrow(ID1))

  for(i in 1:nrow(ID1)){

    row1 <- ID1[i,]
    ID_A <- row1[1]
    ID_B <- row1[2]

    d1 <- dist1[as.character(ID_A), as.character(ID_B)]

    d2[i] <- d1

  }

  ID1 <- cbind(ID1, d2)

  #data2 <- road_L@data

  #R_int <- rasterize(road_L, habitat_R,field="intensity")
  #R_type <- rasterize(road_L, habitat_R,field="type")

  #How is "type" coded in numbers to built R_type
  #E1 <- extract(R_type, road_L)
  #E2 <- road_L@data
  #E3 <- rep(NA, nrow(E2))

  #for(i in 1:length(E3)){

  #E4 <- E1[[i]]
  #E5 <- getmode(E4)
  #E3[i] <- E5

  #}

  #How "type" in character and numeric (in the raster) match
  #E6 <- cbind(E2[,c(2,3)], E3)
  #E6 <- E6[,-2]
  #E6 <- unique(E6)

  #Put the coordinates of the connected nodes in the output data frame
  x1_A <- rep(NA, nrow(ID1))
  y1_A <- rep(NA, nrow(ID1))

  x1_B <- rep(NA, nrow(ID1))
  y1_B <- rep(NA, nrow(ID1))

  area_A <- rep(NA, nrow(ID1))
  area_B <- rep(NA, nrow(ID1))

  for(i in 1:nrow(ID1)){

    #which nodes?
    row2 <- ID1[i,]
    nodeA <- as.numeric(row2[1])#added as.numeric(16-11-2019)
    nodeB <- as.numeric(row2[2])#added as.numeric(16-11-2019)

    lineA <- which(node_T$node_ID == nodeA)
    lineB <- which(node_T$node_ID == nodeB)
    nodeA <- node_T[lineA, ]
    nodeB <- node_T[lineB, ]

    x1_A[i] <- nodeA[2]
    y1_A[i] <- nodeA[3]

    x1_B[i] <- nodeB[2]
    y1_B[i] <- nodeB[3]

    area_A[i] <- nodeA[4]
    area_B[i] <- nodeB[4]

  }

  x1_A <- as.numeric(x1_A)
  y1_A <- as.numeric(y1_A)

  x1_B <- as.numeric(x1_B)
  y1_B <- as.numeric(y1_B)

  area_A <- as.numeric(area_A)
  area_B <- as.numeric(area_B)

  edge_T <- cbind(ID1, x1_A, y1_A, x1_B, y1_B, area_A, area_B)
  rownames(edge_T) <- 1:nrow(edge_T)
  edge_T <- as.data.frame(edge_T)

  colnames(edge_T) <- c("node_A", "node_B", "distance", "x_node_A", "y_node_A", "x_node_B", "y_node_B", "value_A", "value_B")

  #Edges as spatial lines (transferred from Function 6 'create.shapes'):
  out_lines <- vector("list", nrow(edge_T))
  
  for(i in 1:nrow(edge_T)){
    l0 <- edge_T[i,]
    xA <- as.numeric(l0["x_node_A"])
    yA <- as.numeric(l0["y_node_A"])
    xB <- as.numeric(l0["x_node_B"])
    yB <- as.numeric(l0["y_node_B"])
    pts <- matrix(c(xA, yA, xB, yB), nrow=2, ncol=2, byrow=TRUE)
    l1 <- Line(pts)
    l2 <- Lines(l1, ID=rownames(edge_T)[i])
    out_lines[[i]] <- l2
  }

  edge_l <- SpatialLines(out_lines)  # new
  edge_L <- SpatialLinesDataFrame(edge_l, data = edge_T)  # new
  #edge_L@proj4string@projargs <- nodes@proj4string@projargs  # new##16-11-2019 - changes in sp and rgdal
  slot(slot(edge_L, "proj4string"), "projargs") <- slot(slot(nodes, "proj4string"), "projargs")


  # OBTER A ESTRADA CORRESPONDENTE A CADA EDGE (new):
  # primeiro calcular a distancia de cada estrada a cada poligono:

  if (class(road_L) == "SpatialLinesDataFrame")  # NEW
    #road_L@data$autoID <- 1:nrow(road_L@data)  # NEW##16-11-2019 - changes in sp and rgdal
    slot(road_L, "data")$autoID <- 1:nrow(slot(road_L,"data")) ##16-11-2019 - changes in sp and rgdal
    else if (class(road_L) == "SpatialLines")  # NEW
    road_L <- SpatialLinesDataFrame(road_L, data = data.frame(autoID = 1:length(road_L)))  # NEW

  #land_polyg@data$autoID <- 1:nrow(land_polyg@data)  # NEW##16-11-2019 - changes in sp and rgdal
  slot(land_polyg, "data")$autoID <- 1:nrow(slot(land_polyg, "data"))#16-11-2019 - changes in sp and rgdal 
  line_dists <- matrix(data = NA, nrow = length(road_L), ncol = length(land_polyg))
  #rownames(line_dists) <- paste("line", 1:length(road_L), sep = "_")
  #colnames(line_dists) <- paste("pol", 1:length(land_polyg), sep = "_")

  for (l in 1:length(road_L)) {  # double loop
    road <- subset(road_L, autoID == l)
    l_centr <- SpatialLinesMidPoints(road)
    for (p in 1:length(land_polyg)) {
      line_dists[l, p] <- gDistance(subset(land_polyg, autoID == p), l_centr)
    }  # end for p
  }  # end for l


  # depois identificar o par de poligonos com distancia zero a cada estrada:
  line_neighbours <- vector("list", length(road_L))
  for (d in 1:nrow(line_dists)) {
    #line_neighbours[[d]] <- which(sapply(line_dists[d, ], all.equal, 0) == TRUE)
    line_neighbours[[d]] <- which(line_dists[d, ] %in% sort(line_dists[d, ])[1:2])  # get the two closest ones (there could be more than 2 very close to 0 distance)
  }  # end for d

  names(line_neighbours) <- as.character(1:length(line_neighbours))


  # associar cada edge 'a(s) estrada(s) entre o seu par de poligonos:
  edge_road_IDs <- vector("list", length(edge_L))
  for (e in 1:length(edge_L)) {
    road_index <- which(sapply(line_neighbours, setequal, t(edge_L@data[e, c("node_A", "node_B")])))#16-11-2019 - changes in sp and rgdal 
    road_index <- which(sapply(line_neighbours, setequal, t(slot(edge_L, "data")[e, c("node_A", "node_B")])))#16-11-2019 - changes in sp and rgdal 
    #edge_L@data$road_ID[e] <- as.integer(names(line_neighbours)[road_index])
    edge_road_IDs[[e]] <- as.integer(names(line_neighbours)[road_index])
  }  # end for e


  # adicionar o comprimento da(s) estrada(s) de cada edge:
  #edge_L@data$road_length <- NA##16-11-2019 - changes in sp and rgdal
  slot(edge_L, "data")$road_length <- NA #16-11-2019 - changes in sp and rgdal
  
  #for (i in 1:nrow(edge_L@data)) {#16-11-2019 - changes in sp and rgdal
  for(i in 1:nrow(slot(edge_L, "data"))){#16-11-2019 - changes in sp and rgdal  
    edge_road <- road_L[road_L$autoID %in% edge_road_IDs[[i]], ]
    edge_road_sl <- as(edge_road, "SpatialLines")  # needed for SpatialLinesLengths
    #edge_L@data[i, "road_length"] <- sum(SpatialLinesLengths(edge_road_sl))#16-11-2019 - changes in sp and rgdal
    slot(edge_L, "data")[i, "road_length"] <- sum(SpatialLinesLengths(edge_road_sl))#16-11-2019 - changes in sp and rgdal
  }  # end for i


  # eliminar edges de estradas abaixo do tamanho minimo:
  edge_L <- subset(edge_L, road_length >= min_length)

  # eliminar edges de poligonos abaixo do tamanho minimo: (NEW)
  big_pol_IDs <- node_T[node_T$pol_area >= min_pol_area, ]$node_ID
  edge_L <- subset(edge_L, node_A %in% big_pol_IDs & node_B %in% big_pol_IDs)


  if (shape==TRUE){
    suppressWarnings(writeOGR(edge_L, ".", shape_name_edges, driver="ESRI Shapefile",overwrite_layer=TRUE))
    message("Shapefile created! Check the working directory, please.")
  }

  if (plot) {
    plot(land_polyg)
    plot(edge_L, col = "blue", add = TRUE)
  }

  message("Done!")
  return(edge_L)
}
