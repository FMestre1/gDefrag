#FMestre
#02-04-2025

#New version
#https://github.com/FMestre1/gDefrag

#First install the package
#library(devtools)
# Install the package from GitHub
#devtools::install_github("FMestre1/gDefrag") 

rm(list=(ls()))

library(gDefrag)
library(terra)

road_P <- terra::vect("data/road_P.shp")
  
#Obtaining nodes ---------------------------------------------------------------
out1 <- node.creation(land_polyg = road_P, 
                      value_col = "frst_sm",
                      scale_nodes = 10, 
                      cex_labels = 1#, 
                      #shape = FALSE,
                      #shape_name_nodes = "shape_nodes_file", 
                      #overwrite = FALSE
                      )

out1_2 <- node.creation(land_polyg = road_P, 
                      value_col = "proprtn",
                      scale_nodes = 100, 
                      cex_labels = 1#, 
                      #shape = FALSE,
                      #shape_name_nodes = "shape_nodes_file", 
                      #overwrite = FALSE
                      )

#Obtaining edges ---------------------------------------------------------------
out2 <- edge.creation(nodes = out1, 
                      land_polyg = road_P,
                      min_length = 0, 
                      min_pol_area = 0#, 
                      #shape_name_edges = "shape_edges_file",
                      #shape = FALSE,
                      #overwrite = FALSE
                      )

#Prioritize --------------------------------------------------------------------
out3 <- prioritize(nodes = out1, 
                   edges = out2, 
                   method = "value"#,
                   #shape=FALSE, 
                   #shape_name_out = "priorities_shape1", 
                   #overwrite = FALSE
                   )

out4 <- prioritize(nodes = out1, 
                   edges = out2, 
                   method = "IIC"#,
                   #shape=FALSE, 
                   #shape_name_out = "priorities_shape2", 
                   #overwrite = FALSE
                   )

out5 <- prioritize(nodes = out1, 
                   edges = out2, 
                   method = "between",
                   shape=FALSE, 
                   shape_name_out = "priorities_shape3", 
                   overwrite = FALSE)

out6 <- prioritize(nodes = out1_2, 
                   edges = out2, 
                   method = "AWM",
                   shape=FALSE, 
                   shape_name_out = "priorities_shape4", 
                   overwrite = FALSE)

#Plotting results --------------------------------------------------------------
par(mfrow = c(2, 2))
plotgraph(nodes = out1, edges = out3, land_polyg = road_P, main = "Habitat value")
plotgraph(nodes = out1, edges = out5, land_polyg = road_P, main = "Edge betweenness")
plotgraph(nodes = out1, edges = out6, land_polyg = road_P, main = "AWC")
plotgraph(nodes = out1, edges = out4, land_polyg = road_P, main = "IIC")

dev.off()

out4$priorization
out6$priorization

#################################################################################

# Wraper function to run the whole analysis

out1 <- gDefrag.full(land_polyg = road_P, method = "value", 
                     value_col = "frst_sm", main = "value-based graph", 
                     shape_name_nodes = "fullrun_shape_all_nodes0", 
                     shape_name_edges = "fullrun_shape_edges0", 
                     shape_name_out = "fullrun_priorities_shape0",
                     shape = FALSE,
                     overwrite = FALSE)
