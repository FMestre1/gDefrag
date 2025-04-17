midpoint.gdefrag <- function(line) {
  # Check input
  if (!inherits(line, "SpatVector") || terra::geomtype(line) != "lines") {
    stop("Input must be a 'SpatVector' of line type.")
  }
  if (length(line) != 1) {
    stop("Please provide a single line (a SpatVector of length 1).")
  }
  
  # Extract the geometry: returns a matrix
  g <- terra::geom(line)
  
  # Subset to just the first part of the line (single-part geometry)
  coords <- g[g[, "part"] == 1, c("x", "y"), drop = FALSE]
  
  # Use middle vertex
  mid_index <- floor(nrow(coords) / 2)
  if (mid_index < 1) mid_index <- 1
  
  mid_coords <- coords[mid_index, ]
  
  # Create SpatVector point
  pt <- terra::vect(t(as.matrix(mid_coords)), type = "points", crs = terra::crs(line))
  return(pt)
}


table(df_from_list$column1 %in% edge_T$node_A)

which(!df_from_list$column2 %in% edge_T$node_B)

df_from_list$column2[64]
