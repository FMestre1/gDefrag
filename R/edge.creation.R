edge.creation <-
  function(nodes, land_polyg, min_length = 0, min_pol_area = 0, plot = TRUE, shape = FALSE, shape_name_edges = "shape_edges"){#FUNCTION 2
    
    #Create empty objects
    autoID <- NULL
    road_length <- NULL
    node_A <- NULL
    node_B <- NULL
    
    nodes_coords <- data.frame(as.data.frame(nodes),as.data.frame(terra::crds(nodes)))
    
    #Arguments:
    #node_T - Nodes data frame - DEPRECATED
    #nodes - now output of function 1, containing 'nodes' (spatial object, with node_T as data attributes table) and 'adjacents' (table);
    #road_L - Road line shapefile - DEPRECATED
    # min_length: minimum length of road (in the input map projection units) for an edge to be considered for crossing it
    # min_pol_area: (NEW ARGUMENT) minimum area (in the input map projection units) for a polygon to generate edges
    # plot: (NEW ARGUMENT) whether to produce a plot of the edges overlaid to the road polygons
    
    if(terra::crs(nodes) != terra::crs(land_polyg)) stop("Input maps have different CRS.")# NEW 02-02-2025
    
    message("Extracting road lines from polygon borders...")
    
    road_L <- sharedPaths(land_polyg)
    #Add info on length
    road_L$length <- perim(road_L)
    
    
    message("Creating edges...")
    
    #Get table of nodes
    node_T <- as.data.frame(nodes)  # new 02-02-2025
    
    #Which nodes are adjacent?
    adj <- adjacent(land_polyg, pairs = FALSE)
    rownames(adj) <- colnames(adj) <- as.character(1:length(land_polyg))  # transferred from function 1
    adj <- upper.tri(adj, diag = FALSE) * adj
    adj_names <- rownames(adj)
    adj <- array(as.logical(adj), dim(adj))
    rownames(adj)<- adj_names
    colnames(adj) <- adj_names
    ID1 <- which(adj,TRUE)
    ID1[,1] <- as.numeric(rownames(adj)[ID1[,1]])
    ID1[,2] <- as.numeric(colnames(adj)[ID1[,2]])
    
    dist1 <- terra::distance(nodes)
    dist1 <- data.frame(as.matrix(dist1))
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
    
    #Add distance to the table
    ID2 <- cbind(ID1, d2)
    
    # ut the coordinates and areas of the connected nodes in the output data frame
    x1_A <- rep(NA, nrow(ID2))
    y1_A <- rep(NA, nrow(ID2))
    
    x1_B <- rep(NA, nrow(ID2))
    y1_B <- rep(NA, nrow(ID2))
    
    area_A <- rep(NA, nrow(ID2))
    area_B <- rep(NA, nrow(ID2))
    
    
    for(i in 1:nrow(ID2)){
      
      #which nodes?
      row2 <- ID2[i,]
      nodeA <- as.numeric(row2[1])#added as.numeric(16-11-2019)
      nodeB <- as.numeric(row2[2])#added as.numeric(16-11-2019)
      
      nodeAline <- nodes_coords[nodes_coords$node_ID == nodeA, ]
      nodeBline <- nodes_coords[nodes_coords$node_ID == nodeB, ]
      
      x1_A[i] <- nodeAline$x
      y1_A[i] <- nodeAline$y
      
      x1_B[i] <- nodeBline$x
      y1_B[i] <- nodeBline$y
      
      area_A[i] <- nodeAline$pol_area
      area_B[i] <- nodeBline$pol_area
      
    }
    
    x1_A <- as.numeric(x1_A)
    y1_A <- as.numeric(y1_A)
    
    x1_B <- as.numeric(x1_B)
    y1_B <- as.numeric(y1_B)
    
    area_A <- as.numeric(area_A)
    area_B <- as.numeric(area_B)
    
    edge_T <- cbind(ID2, x1_A, y1_A, x1_B, y1_B, area_A, area_B)
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
      l1 <- sp::Line(pts)
      l2 <- sp::Lines(l1, ID=rownames(edge_T)[i])
      out_lines[[i]] <- l2
    }
    
    edge_l <- sp::SpatialLines(out_lines)  # new
    edge_L <- sp::SpatialLinesDataFrame(edge_l, data = as.data.frame(edge_T))  # new
    edge_L <- terra::vect(edge_l)
    terra::crs(edge_L) <- terra::crs(land_polyg)
    values(edge_L) <- edge_T
    
    #Get to the edge the value from the underlaying road
    line_dists <- matrix(data = NA, nrow = length(road_L), ncol = length(land_polyg))
    
    for (l in 1:length(road_L)) {  # double loop
      road <- road_L[l,]
      l_centr <- terra::centroids(road)
      
      for (p in 1:length(land_polyg)) {
        line_dists[l, p] <- terra::distance(land_polyg[p,], l_centr)
      }  # end for p
    }  # end for l
    
    # Indentify adjacent polygons to each road
    line_neighbours <- vector("list", length(road_L))
    
    for (d in 1:nrow(line_dists)) {
      line_neighbours[[d]] <- c(which(line_dists[d, ] %in% sort(line_dists[d, ])[1:2]))  # get the two closest ones (there could be more than 2 very close to 0 distance)
    }  # end for d
    
    names(line_neighbours) <- as.character(1:length(line_neighbours))
    
    # Create a data frame
    df_from_list <- data.frame(
      names(line_neighbours),
      column1 = sapply(line_neighbours, function(x) x[1]), 
      column2 = sapply(line_neighbours, function(x) x[2])
    )
    
    
    for (i in 1:nrow(edge_T)) {
      
      # Define the specific numbers to find
      number1 <- edge_T[i, "node_A"]
      number2 <- edge_T[i, "node_B"]
      
      # Find the row where both numbers appear
      road_index <- which((df_from_list$column1 == number1 & df_from_list$column2 == number2) | 
                            (df_from_list$column1 == number2 & df_from_list$column2 == number1))
      
      edge_T$road_ID[i] <- as.integer(names(line_neighbours)[road_index])
    }  # end for i
    
    
    #Add road length to the data frame
    values(edge_L) <- data.frame(values(edge_L),perim(edge_L))
    names(values(edge_L))[10] <- "road_length"
    
    # Remove edges of roads under a given distance:
    edge_L <- edge_L[values(edge_L)$road_length >= 0,]
    
    # Eliminate edges from nodes under a given area:
    big_pol_IDs <- node_T[node_T$pol_area >= min_pol_area, ]$node_ID
    
    #keep only those in big_pol_IDs
    edge_L <- edge_L[edge_L$node_A %in% big_pol_IDs & edge_L$node_B %in% big_pol_IDs,]
    
    if (shape==TRUE){
      
      #Create shapefile
      message("Creating shapefile...")
      terra::writeVector(edge_L, "edges_shapefile.shp")
      terra::writeVector(edge_L, paste0(shape_name_edges, ".shp"))
      
      message("Shapefile created! Check the working directory, please.")
    }
    
    if (plot) {
      plot(land_polyg)
      plot(edge_L, col = "blue", add = TRUE)
    }
    
    message("Done!")
    return(edge_L)
  }