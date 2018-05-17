\name{road_P}
\alias{road_P}
\docType{data}
\title{
Road polygons dataset with habitat area  
}
\description{
Sample dataset. SpatialPolygonsDataFrame with the polygons defined by roads and the study site limits. The field characterizing the raster is the habitat area in the polygon. 
}
\usage{data("road_P")}
\format{
An object of the class 'SpatialPolygonsDataFrame' [package "sp"] with six features. The argument table should have a column with the values of an underlying raster (e.g. averaged or summed).
}
\details{
This dataset has the spatial information on the polygons in the sample study site. The polygons are defined by the roads and the limits of the sample study site (Odemira Municipality, Portugal).
The field "forest_amount" is to be used with method = 'hab_prop' in the function \code{\link{prioritize}}, a new metric that accounts for habitat proportion in each polygon. However, it can also be used with method = 'value' or 'IIC'. The field "forest_sum" can not be used with the method = "hab_prop" of the function \code{\link{prioritize}}, since this method uses only the ratio of suitable habitat. It is intended for use with the methods 'IIC' and 'value'.
}
\seealso{
\code{\link{node.creation}}, \code{\link{edge.creation}}, \code{\link{prioritize}}
}
\examples{
data(road_P)
str(road_P)

}