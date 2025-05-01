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
  \item{re_scale}{
TRUE/FALSE. Should the output be re-scaled between 0 and 100? 
}
  \item{shape}{
Create a shapefiles? (TRUE/FALSE).
}
  \item{shape_name_out}{
Name of the shapefile to be created.
}
  \item{overwrite}{
Should the shapefile file be overwritten?
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
The output is a SpatVector with an additional field, 'priorization'. This field provides the desired priorization value.
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

}