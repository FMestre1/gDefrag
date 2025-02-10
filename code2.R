#FMestre
#02-04-2025

#New version
#https://github.com/FMestre1/gDefrag

#First install the package
#library(devtools)
# Install the package from GitHub
#devtools::install_github("FMestre1/gDefrag") 

library(gDefrag)
library(terra)

road_P <- terra::vect("data/road_P.shp")

#Obtaining nodes
out1 <- node.creation(land_polyg = road_P, 
                      value_col = "frst_sm",
                      scale_nodes = 10, 
                      cex_labels = 1, 
                      shape = TRUE,
                      shape_name_nodes = "shape_nodes_file", 
                      overwrite = TRUE)

#Obtaining edges
out2 <- edge.creation(nodes = out1, 
                      land_polyg = road_P,
                      min_length = 0, 
                      min_pol_area = 0, 
                      shape_name_edges = "shape_edges_file",
                      shape = TRUE,
                      overwrite = TRUE)

#Prioritize
out3 <- prioritize(nodes = out1, 
                   edges = out2, 
                   method = "value",
                   shape=TRUE, 
                   shape_name_out = "priorities_shape2", 
                   overwrite = TRUE)

#Plotting results
plotgraph(nodes = out1, edges = out3, land_polyg = road_P, main = "Habitat value")


#################################################################################

# Wraper function to run the whole analysis

out1 <- gDefrag.full(land_polyg = road_P, method = "value", 
                     value_col = "frst_sm", main = "value-based graph", 
                     shape_name_nodes = "fullrun_shape_all_nodes0", 
                     shape_name_edges = "fullrun_shape_edges0", 
                     shape_name_out = "fullrun_priorities_shape0",
                     shape = TRUE,
                     overwrite = TRUE)
