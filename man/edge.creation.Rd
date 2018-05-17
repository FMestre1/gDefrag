\name{edge.creation}
\alias{edge.creation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creating graph edges
}
\description{
Generates the graph edges, connecting nodes that are the landscape polygons resulting from the landscape clip by the road network. 
}
\usage{
edge.creation(nodes, land_polyg, min_length = 0, 
min_pol_area = 0, plot = TRUE, shape = FALSE, 
shape_name_edges = "shape_edges")
}
\arguments{
  \item{nodes}{
Output of the function \code{\link{node.creation}}.
}
  \item{land_polyg}{
SpatialPolygonsDataFrame loaded with readOGR function of the package \code{rgdal} (polygon shapefile).
}
  \item{min_length}{
Minimum length of the road segment to consider creating an edge representing it.
}
  \item{min_pol_area}{
Minimum area for a given polygon to be considered a node.
}
  \item{plot}{
Plot after running? (TRUE/FALSE).
}
  \item{shape}{
Create a shapefile with the edges (TRUE/FALSE).
}
  \item{shape_name_edges}{
Name of the edges shapefile to be created (without prioritization).
}
}
\details{
If shape = TRUE, this function produces one shapefile with edges without prioritization metrics.
}
\value{
The function returns a SpatialLinesDataFrame with the edges. The data table of this data frame has the following fields:
\itemize{
\item node_A - Node A Id.
\item node_B - Node B Id.
\item  distance - Euclidean distance between nodes A and B (edge length).
\item x_node_A - Node A longitude.
\item y_node_A - Node A latitude.
\item x_node_B - Node B longitude.
\item y_node_A - Node A latitude.
\item raster_value_A - Average value of the underlying raster in the polygon represented by node A.
\item raster_value_B - Average value of the underlying raster in the polygon represented by node B.
\item road_ID - Id of the road represented by the edge.
\item road_length - Length of the road represented by the edge.
}
}
\author{
Frederico Mestre, Fernando Ascensao and A. Marcia Barbosa 
}
\seealso{
\code{\link{node.creation}}
}
\examples{
data(road_P)

out1 <- node.creation(land_polyg=road_P, value_col = "forest_sum", scale_nodes = 10, 
col_nodes = "pink", cex_labels = 0.7)

out2 <- edge.creation(nodes = out1, land_polyg = road_P, min_length = 0, 
min_pol_area = 0, plot = FALSE)

}
