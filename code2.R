#FMestre
#02-04-2025

#New version
#https://github.com/FMestre1/gDefrag

#First install the pachage
#library(devtools)
# Install the package from GitHub
#devtools::install_github("FMestre1/gDefrag") 

library(gDefrag)

road_P <- terra::vect("data/road_P.shp")

#Obtaining nodes
out1 <- node.creation(land_polyg = road_P, value_col = "frst_sm",
                      scale_nodes = 10, col_nodes = "pink", cex_labels = 1)

#Obtaining edges
out2 <- edge.creation(nodes = out1, land_polyg = road_P,
                      min_length = 0, min_pol_area = 0)

#Prioritize
out3 <- prioritize(nodes = out1, edges = out2, method = "value")

#Plotting results
plotgraph(nodes = out1, edges = out3, land_polyg = road_P, main = "Habitat value")

