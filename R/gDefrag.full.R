gDefrag.full <-
function(land_polyg, method, value_col = NULL, min_length = 0, min_pol_area = 0, shape = FALSE, shape_name_nodes = "shape_all_nodes", shape_name_edges = "shape_edges", shape_name_out = "priorities_shape", shape_name_nodes_edges = "nodes_with_edges", scale_nodes = 10, col_nodes = "deepskyblue4", col_labels = "white", cex_labels = 1, main = "Graph") {

road_P <- NULL

  #if (missing(road_L)) stop('argument "road_L" is missing, with no default')  # senao a falta deste so seria detectada mais tarde e dava erro depois de algum tempo de espera
  if (missing(method)) stop('argument "method" is missing, with no default')
  if (method == "value" && missing(value_col)) stop ("for 'method = value', you need to define 'value_col'")
  if (!(method %in% c("value", "between", "IIC", "AWM"))) stop('invalid "method" - please check the help file for available (case-sensitive) options')
  #if (!is.null(traffic_column) && !(traffic_column %in% 1:ncol(road_L@data)) && !(traffic_column %in% colnames(road_L@data))) stop('"traffic_column" not found in road_L@data')
  nodes <- node.creation(land_polyg = land_polyg, value_col = value_col, plot = FALSE, shape = shape, shape_name_nodes = shape_name_nodes)

  edges0 <- edge.creation(nodes = nodes, land_polyg = land_polyg, min_length = min_length, min_pol_area = min_pol_area, plot = FALSE, shape = shape, shape_name_edges = shape_name_edges)
 
  message("Computing priorities...")

  #road_L@data$autoID <- 1:nrow(road_L@data)
  #edges0@data <- data.frame(edges0@data, road_L@data[match(edges0@data[, "road_ID"], road_L@data[, "autoID"]), ])

  edges <- prioritize(nodes = nodes, edges = edges0, method = method, shape = shape, shape_name_out = shape_name_out, shape_name_nodes_edges = shape_name_nodes_edges)

  message("Done!")

  plotgraph(nodes = nodes, edges = edges, land_polyg = land_polyg, scale_nodes = scale_nodes, col_nodes = col_nodes, col_labels = col_labels, cex_labels = cex_labels, main = main)

  return(list(nodes = nodes, edges = edges))
}
