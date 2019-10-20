make_grid <- function(x, cell_diameter, cell_area, clip = FALSE) {
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_diameter, 
                offset = c(0, 0))
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}

make_hex <- function(x, cell_diameter, cell_area, clip = FALSE) {
  
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g2 <- spsample(ext, type = "hexagonal", cellsize = cell_diameter, 
                 offset = c(0, 0))
  
  # convert center points to hexagons
  g2 <- HexPoints2SpatialPolygons(g2, dx = cell_diameter)
  # clip to boundary of study area
  if (clip) {
    g2 <- gIntersection(g2, x, byid = TRUE)
  } else {
    g2 <- g2[x, ]
  }
  # clean up feature IDs
  row.names(g2) <- as.character(1:length(g2))
  return(g2)
}
