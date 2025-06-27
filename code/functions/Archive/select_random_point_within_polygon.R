# Function to generate a random point within a polygon ####
#this simulates the user of the iDeer tool selecting a point within a woodland polygon
#requires an sf object containing polygons
#creates a list of sf points
generate_random_point <- function(polygon) {
  # Get bounding box and generate random point within it
  bbox <- st_bbox(polygon)
  x <- runif(1, bbox$xmin, bbox$xmax)
  y <- runif(1, bbox$ymin, bbox$ymax)
  point <- st_sfc(st_point(c(x, y)), crs = st_crs(polygon))
  
  # Check if the point is within the polygon; if not, generate a new one
  while (!st_within(point, polygon, sparse = FALSE)) {
    x <- runif(1, bbox$xmin, bbox$xmax)
    y <- runif(1, bbox$ymin, bbox$ymax)
    point <- st_sfc(st_point(c(x, y)), crs = st_crs(polygon))
  }
  
  point
}
