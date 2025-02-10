node.creation <-
function(land_polyg, value_col, plot = TRUE, scale_nodes = 1, col_nodes = "red", cex_labels = 1, shape = FALSE, shape_name_nodes = "shape_nodes", overwrite){#FUNCTION 1
  

  if (terra::is.lonlat(land_polyg))  stop ("'land_polyg' must be in a projected coordinate system.")
  if(shape == FALSE && overwrite == TRUE) message("Not writing vector to file, no need to define value to overwrite argument.")
  if(shape == FALSE && shape_name_nodes != "shape_nodes") message("Not writing vector to file, no need to define shapefile name.")
  if(shape == TRUE && shape_name_nodes == "shape_nodes") message("You did not define a name for the shapefile. It will be named shape_nodes.shp!")
  
  message("Creating nodes...")
  node_ID <- 1:length(land_polyg)  # crio objecto porque vai ser usado mais vezes
  

  #Extract habitats:
  #road_P_R <- rasterize(land_polyg, habitat_R)
  #road_P_zonal <- zonal(habitat_R, road_P_R, fun = zonal_fun, na.rm = TRUE)
  #road_P_zonal <- road_P_zonal[, 2]
  #zonal_fun <- ifelse(proportion, mean, sum)  # new
  #message("Extracting raster values per polygon (may take long for large rasters)...")  # new
  #road_P_zonal <- velox(habitat_R)$extract(land_polyg, fun = zonal_fun)  # much faster than raster pkg, but leaves some polygons with NA
  #road_P_zonal <- extract(habitat_R, land_polyg, fun = zonal_fun, na.rm = TRUE)  # slightly faster than rasterize + zonal

  #Derive centroids
  #centroids <- gCentroid(land_polyg, byid = TRUE, id = node_ID)  # acrescentei ultimo argumento
  #centroids <- centroids@coords
  #centroids <- coordinates(land_polyg)  # poupa algum tempo
  
  #centroids <- coordinates(gPointOnSurface(land_polyg, byid = TRUE, id = node_ID))  # NEW: random points within the polygons; actual centroids may fall outside L-shaped polygons and cause errors downstream # 18-01-2023
  centroids <- terra::centroids(land_polyg, inside = TRUE)
  centroids <- as.data.frame(terra::crds(centroids))
  
  #Create output table
  nodes_T <- data.frame(node_ID, centroids, as.data.frame(land_polyg[ , value_col]) , terra::expanse(land_polyg))
  colnames(nodes_T) <- c("node_ID", "X", "Y", "pol_value", "pol_area")
  
  #Create output table # commentedn in 18-01-2023
  #nodes_T <- data.frame(node_ID,
  #                      centroids,
  #                      #land_polyg@data[ , value_col],##16-11-2019 - changes in sp and rgdal
  #                      slot(land_polyg, "data")[ , value_col],##16-11-2019 - changes in sp and rgdal
  #                      gArea(land_polyg, byid = TRUE))  # Acertar isto com as unidades!
  #colnames(nodes_T) <- c("node_ID", "X", "Y", "pol_value", "pol_area")
  #nodes_T <- data.frame(land_polyg@data, centroids, road_P_zonal)  # new
  #colnames(nodes_T)[(ncol(nodes_T)-2):ncol(nodes_T)] <- c("X", "Y", "value")
  
  ######
  #Adicionei isto a 23-04-2018 (23:45)
  # eliminar edges de poligonos abaixo do tamanho minimo: (NEW)
  #nodes_T <- nodes_T[ which(nodes_T$pol_area > min_pol_area),]
  #rownames(nodes_T) <- 1:nrow(nodes_T)#Sera que esta e a melhor opcao? 
  #nodes_T$node_ID <- 1:nrow(nodes_T)   
  ######
  
  if (plot) {
    plot(land_polyg)
    symbols(nodes_T[ , "X"], nodes_T[ , "Y"], circles = sqrt(nodes_T[ , "pol_value"] / pi) * scale_nodes, fg = col_nodes, add = TRUE, inches = FALSE)
    text(x = centroids, labels = as.character(node_ID), cex = cex_labels)
    #text(x = nodes_T[ , c("X", "Y")], labels = as.character(nodes_T[ , "node_ID"]), cex = cex_labels)  # to plot only nodes above min_pol_area
  }

  #nodes <- SpatialPointsDataFrame(coords = nodes_T[ , c("X", "Y")], data = nodes_T)  # 18-01-2023
  nodes <-terra::vect(nodes_T, geom = c("X","Y")) # 18-01-2023
  terra::crs(nodes) <- terra::crs(land_polyg)
  
  #nodes@proj4string@projargs <- land_polyg@proj4string@projargs#16-11-2019 - changes in sp and rgdal
  #slot(slot(nodes, "proj4string"), "projargs") <- slot(slot(land_polyg, "proj4string"), "projargs")#16-11-2019 - changes in sp and rgdal   # 18-01-2023
  
  #proj4string(nodes) <- CRS(proj4string(land_polyg))#18-11-2019   # 18-01-2023
  
  #Adjacency - trensferred to Function 2 where it was needed
  #adj <- gTouches(land_polyg, byid = TRUE)
  ################################################
  #a <- poly2nb(land_polyg)
  #b <- nb2mat(a, style="B")
  #c1 <- as.data.frame(b)
  #ID_adj <- as.character(node_ID)
  #rownames(c1) <- ID_adj
  #colnames(c1) <- ID_adj
  ################################################
  #adj <- as.data.frame(adj)
  #rownames(adj) <- colnames(adj) <- as.character(node_ID)

  #result <- list(nodes = nodes, adjacents = adj)
  
  if (shape == TRUE){
  message("Shapefile created! Check the working directory, please.")
  terra::writeVector(nodes, filename = paste0(shape_name_nodes, ".shp"), overwrite = overwrite)
  }

  message("Done!")
  return(nodes)
}
