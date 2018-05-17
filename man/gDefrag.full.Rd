\name{gDefrag.full}
\alias{gDefrag.full}
\title{
Wrapper function running the full process
}
\description{
This function runs the full process of creating the nodes, creating the 
edges and establishing priority edges to be used to enhance connectivity 
in the landscape for the focal species.
}
\usage{
gDefrag.full(land_polyg, method, value_col = NULL, 
min_length = 0, min_pol_area = 0, shape = FALSE, 
shape_name_nodes = "shape_all_nodes", shape_name_edges = "shape_edges", 
shape_name_out = "priorities_shape", 
shape_name_nodes_edges = "nodes_with_edges", 
scale_nodes = 10, col_nodes = "deepskyblue4", col_labels = "white", 
cex_labels = 1, main = "Graph")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{land_polyg}{
SpatialPolygonsDataFrame loaded with readOGR function of the package rgdal (polygon shapefile). To be internally passed to as \code{\link{node.creation}} and as \code{\link{edge.creation}}.
}
  \item{method}{
Method to be used in the prioritization of the connectivity corridors. Detailed description in the 'details' section. To be internally passed to as \code{\link{prioritize}}.
}
  \item{value_col}{
Identification of the column in the 'land_polyg' object. To be internally passed to as \code{\link{prioritize}}.
}
  \item{min_length}{
Minimum length of the road segment to consider creating an edge representing it. To be internally passed to as \code{\link{edge.creation}}.
}
  \item{min_pol_area}{
Minimum area of the polygon to consider creating a node representing it. To be internally passed to as \code{\link{node.creation}}.
}
  \item{shape}{
Produce node and edges shapefiles as output? (TRUE/FALSE) To be internally passed to as \code{\link{node.creation}} and as \code{\link{edge.creation}}.
}
  \item{shape_name_nodes}{
Name of the nodes shapefile to be created (all nodes).
}
  \item{shape_name_edges}{
Name of the edges shapefile to be created (without prioritization).
}
  \item{shape_name_out}{
Name of the edges prioritization output shapefile to be created.
}
  \item{shape_name_nodes_edges}{
Instead of 'shape_name_nodes', that depicts all the nodes, this shape has only nodes considered in the prioritization procedure (those with an area greater that the argument 'min_pol_area' in the function \code{\link{edge.creation}}).
}
  \item{scale_nodes}{
Node size scaling factor for correct viewing in the plot. To be internally passed to as \code{\link{plotgraph}}.  
}
  \item{col_nodes}{
Node colour. To be internally passed to as \code{\link{plotgraph}}.
}
  \item{col_labels}{
Labels colour. To be internally passed to as \code{\link{plotgraph}}.
}
  \item{cex_labels}{
Text labels font size. To be internally passed to as \code{\link{plotgraph}}.
}
  \item{main}{
Main plot title. To be internally passed to as \code{\link{plotgraph}}.
}
}
\details{
If shape = TRUE, this function produces four shapefiles: all nodes, nodes with area greater than 'min_pol_area', edges without prioritization metrics, edges with prioritization metrics.
The prioritization of the connectivity corridors can be made with one of four methods (argument 'method'):
\itemize{
	\item 'value' - Give priority to edges connecting nodes with higher attribute value (e.g. suitability for the focal species);
	\item 'traffic' - Give priority to edges representing roads with more traffic;
	\item 'IIC' - Give priority to edges with higher overall impact in the connectivity. This was implemented by using the Integral Index of Connectivity (IIC) developed by Pascual-Hortal and Saura (2006). The connectivity relevance of each edge was evaluated usinf the approach described in Pascual-Hortal and Saura (2006): dIIC=((I1-I2)/I1)*100, where I1 is the IIC before edge removal and I2 is the IIC after. 
	\item 'between' - Give priority to edges with higher betweenness using the function \code{edge.beetweenness.estimate} from the \code{igraph} package.
		\item 'AWM' - Give priority to edges connecting nodes with more suitable area.
}
}
\value{
This function produces a 'list' object. This list has two objects, one with the information of the nodes and the other with the information on the edges.
}
\author{
Frederico Mestre, Fernando Ascensao and A. Marcia Barbosa
}
\seealso{
\code{\link{node.creation}}, \code{\link{edge.creation}}, \code{\link{plotgraph}}, \code{\link{prioritize}}
}
\examples{

data(road_P)

out1 <- gDefrag.full(land_polyg = road_P, method = "value", 
value_col = "forest_sum", main = "value-based graph", 
shape = TRUE)

}
