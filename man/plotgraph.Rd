\name{plotgraph}
\alias{plotgraph}
\title{
Plotting the spatial graph 
}
\description{
Function to plot the spatial graph 
}
\usage{
plotgraph(nodes, edges, land_polyg, scale_nodes = 10, col_nodes = "darkblue", 
col_labels = "darkblue", cex_labels = 1, main = "Graph")
}
\arguments{
  \item{nodes}{
Output of \code{\link{node.creation}}.
}
  \item{edges}{
Output of \code{\link{edge.creation}}.
}
  \item{land_polyg}{
SpatialPolygonsDataFrame with the polygons defined by roads and the study site limits.
}
  \item{scale_nodes}{
Node size scaling factor for correct viewing in the plot.
}
  \item{col_nodes}{
Node colour. 
}
  \item{col_labels}{
Labels colour. 
}
  \item{cex_labels}{
Text labels font size.
}
  \item{main}{
Main plot title.
}
}
\details{
Produces a plot of the study site, nodes and edges with the prioritization order. 
}
\value{
This function plots the spatial graph.
}
\author{
Frederico Mestre, Fernando Ascensao and A. Marcia Barbosa
}
\seealso{
\code{\link{node.creation}}, \code{\link{edge.creation}}, \code{\link{prioritize}}, \code{\link{road_P}}
}
\examples{
data(road_P)

#Obtaining nodes
out1 <- node.creation(land_polyg = road_P, value_col = "forest_sum", 
scale_nodes = 10, col_nodes = "pink", cex_labels = 1)

#Obtaining edges
out2 <- edge.creation(nodes = out1, land_polyg = road_P, 
min_length = 0, min_pol_area = 0)

#Prioritize
out3 <- prioritize(nodes = out1, edges = out2, method = "value")

#Plotting results
plotgraph(nodes = out1, edges = out3, land_polyg = road_P, main = "Habitat value")
}
