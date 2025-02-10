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
An object of the class 'SpatVector' [package "terra"] with eleven polygons. The argument table should have a column with the values of an underlying raster (e.g. averaged or summed).
}
\details{
This dataset has the spatial information on the polygons in the sample study site. The polygons are defined by the roads and the limits of the sample study site.
}
\seealso{
\code{\link{node.creation}}, \code{\link{edge.creation}}, \code{\link{prioritize}}
}
\examples{
data(road_P)
str(road_P)

}