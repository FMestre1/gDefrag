\name{prioritize}
\alias{prioritize}
\title{
Prioritizing connectivity corridors 
}
\description{
Function to stablish a priority on the edges (connections) to be created in order to de-fragment a landscape.
}
\usage{
prioritize(nodes, edges, method, shape=FALSE, shape_name_out = "priorities_shape", 
shape_name_nodes_edges = "nodes_with_edges")
}
\arguments{
  \item{nodes}{
Output of \code{\link{node.creation}}.
}
  \item{edges}{
Output of \code{\link{edge.creation}}.
}
  \item{method}{
Method to be used in the prioritization of the connectivity corridors. Detailed description in the 'details' section. 
}
  \item{shape}{
Create a shapefiles? (TRUE/FALSE).
}
  \item{shape_name_out}{
Name of the shapefile to be created.
}
  \item{shape_name_nodes_edges}{
Name of the nodes shapefile to be created. This shape has only nodes considered in the prioritization procedure (those with an area greater that the argument 'min_pol_area' in the function \code{\link{edge.creation}}).
}
}
\details{
If shape = TRUE, this function produces two shapefiles: nodes with area greater than 'min_pol_area' (in \code{\link{edge.creation}}), edges with prioritization metrics.
The prioritization of the connectivity corridors can be made with one of these methods (argument 'method'):
\itemize{
	\item 'value' - Give priority to edges connecting nodes with higher attribute value (e.g. suitability for the focal species);
	\item 'IIC' - Give priority to edges with higher overall impact in the connectivity. This was implemented by using the Integral Index of Connectivity (IIC) developed by Pascual-Hortal and Saura (2006). The connectivity relevance of each edge was evaluated usinf the approach described in Pascual-Hortal and Saura (2006): dIIC=((I1-I2)/I1)*100, where I1 is the IIC before edge removal and I2 is the IIC after. 
	\item 'between' - Give priority to edges with higher betweenness using the function \code{edge.beetweenness.estimate} from the \code{igraph} package.
	\item 'AWM' - Area Weighted Metric - The user should provide the value of the variable to be weighted by area (e.g. habitat area) in the 'value_col' argument of the function \code{\link{node.creation}}. The expression for the Area Weighted Metric is: AWM = (value_A*area_B)+(value_B*area_A), where: 'value_A' and 'area_B' are the node A argument and the habitat in polygon B, respectively and 'value_B' and 'area_A' are the node B argument and the habitat in polygon A, respectively.
}
}
\value{
The output is a SpatialLinesDataFrame with an additional field, 'priorization'. This field provides the desired priorization value.
}
\references{
Pascual-Hortal, L., Saura, S. (2006). Comparison and development of new graph-based landscape connectivity indices: towards the priorization of habitat patches and corridors for conservation. Landscape Ecology, 21(7), 959-967.
}
\author{
Frederico Mestre, Fernando Ascensao and A. Marcia Barbosa
}
\note{
The computation of IIC is made resorting to the node attribute in place of the 'habitat patch areas' (ai and aj) as originally devised by Pascual-Hortal & Saura (2006). 
}
\seealso{
\code{\link{node.creation}}, \code{\link{edge.creation}} 
}
\examples{
data(road_P)

####### To the value, betweenness and connectivity metrics #######

#Obtaining nodes
out1 <- node.creation(land_polyg = road_P, value_col = "forest_sum", 
scale_nodes = 10, col_nodes = "pink", cex_labels = 1)

#Obtaining edges
out2 <- edge.creation(nodes = out1, land_polyg = road_P, 
min_length = 0, min_pol_area = 0)

#Prioritize
out3 <- prioritize(nodes = out1, edges = out2, 
	method = "value")#Raster value
out4 <- prioritize(nodes = out1, edges = out2, 
	method = "between")#Betweenness
out5 <- prioritize(nodes = out1, edges = out2, 
	method = "IIC")  #Connectivity

####### To the area weighted metric #######

#Obtaining nodes
out6 <- node.creation(land_polyg = road_P, value_col = "proportion", 
scale_nodes = 10, col_nodes = "pink", cex_labels = 1)

#Obtaining edges
out7 <- edge.creation(nodes = out6, land_polyg = road_P, 
min_length = 0, min_pol_area = 0)

#Prioritize
out8 <- prioritize(nodes = out6, edges = out7, 
	method = "AWM")#Habitat proportion

}