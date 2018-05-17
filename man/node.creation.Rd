\name{node.creation}
\alias{node.creation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creating graph nodes
}
\description{
Generates the nodes that are the landscape polygons resulting from the landscape clip by infrastructure network.   
}
\usage{
node.creation(land_polyg, value_col, plot = TRUE, 
scale_nodes = 1, col_nodes = "deepskyblue4", cex_labels = 1, 
shape = FALSE, shape_name_nodes = "shape_nodes")
}
\arguments{
  \item{land_polyg}{
SpatialPolygonsDataFrame loaded with readOGR function of the package rgdal (polygon shapefile).
}
  \item{value_col}{
Which column in the SpatialPolygonsDataFrame has the relevant information on the underlying raster. This information must be retrieved previously using a GIS, if the user wishes to use the method 'value' for prioritizing edges.
}
  \item{plot}{
Plot the output (TRUE/FALSE).
}
  \item{scale_nodes}{
Scaling factor to represent the nodes graphically.
}
  \item{col_nodes}{
Nodes colour.
}
  \item{cex_labels}{
Font size of the labels.
}
  \item{shape}{
Create a shapefile with the edges (TRUE/FALSE).
}
  \item{shape_name_nodes}{
Name of the nodes shapefile to be created (all nodes).
}
}
\details{
If shape = TRUE, this function produces one shapefile with all the nodes.
}
\value{
The function returns a SpatialPointsDataFrame with the nodes. The data table of this data frame has the following fields:
\itemize{
\item X - Node latitude.
\item Y - Node longitude.
\item  pol_value - Underlying raster value summed for each polygon. 
\item  pol_area - Polygon area. 
}
}
\author{
Frederico Mestre, Fernando Ascensao and A. Marcia Barbosa
}
\seealso{
\code{\link{edge.creation}}
}
\examples{
data(road_P)

out1 <- node.creation(land_polyg = road_P, value_col = "forest_sum", 
scale_nodes = 10, col_nodes = "pink", cex_labels = 1)

}
